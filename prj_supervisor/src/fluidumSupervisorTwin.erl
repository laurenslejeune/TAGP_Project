-module(fluidumSupervisorTwin).
-behaviour(supervisor).
-export([create/4,init/1]).
-export([getChildren/0]).
-export([generateNPipesIds/2]).


create({N_pipes,Begin_N,End_N},N_pumps,N_hex,DifList)->
    supervisor:start_link({local,?MODULE},?MODULE,[{N_pipes,Begin_N,End_N},N_pumps,N_hex,DifList]).

init([{_,Begin_N,End_N},N_pumps,N_hex,DifList])->
    SupFlags = #{strategy => one_for_all, intensity => 4, period => 5},

    %% First, connect all the pipes:
    %Generate a list of all pipes for the given information

    %Next, create a fluimum and fill the pipes with it
    [FirstPipe] = generateNPipesIds(Begin_N,Begin_N),
    {ok,[C,_]} = resource_instance:list_connectors(FirstPipe),
    
    FluidumInstSpec = #{id => fluiduminst_child,
                    start => {fluidumInst, create, [C,fluidumTyp,1]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [fluidumInst]},
    %Also create a supervisor to fill the circuit with the created fluidum + install the pumps
    FillCircuitSupervisorSpec = #{id => fillcircuitsupervisor_child,
                                start => {fillCircuitSupervisorTwin, create, [Begin_N,End_N,fluidumInst_1]},
                                restart => permanent,
                                shutdown => brutal_kill,
                                type => supervisor,
                                modules => [fillCircuitSupervisorTwin]},

    %Finally create a supervisor that takes care of the other parts of the circuit
    PumpFlowmeterHESupervisorSpec = #{id => pumpFlowmeterHESupervisor_twin_child,
                                    start => {pumpFlowmeterHESupervisorTwin, create, [{Begin_N,End_N},N_pumps,N_hex,DifList]},
                                    restart => permanent,
                                    shutdown => brutal_kill,
                                    type => supervisor,
                                    modules => [pumpFlowmeterHESupervisorTwin]},
    ChildSpecs = [FluidumInstSpec,FillCircuitSupervisorSpec,PumpFlowmeterHESupervisorSpec],
    %ChildSpecs = [FluidumTypSpec,FluidumInstSpec,FillCircuitSupervisorSpec],
    {ok,{SupFlags,ChildSpecs}}.

getChildren()->
    supervisor:which_children(?MODULE).

generateNPipesIds(N,N)->
    Atom = testFunctions:numberToAtom(pipeInst,N),
    [Atom];

generateNPipesIds(CurrentN,N)->
    Atom = testFunctions:numberToAtom(pipeInst,CurrentN),
    [Atom] ++ generateNPipesIds(CurrentN+1,N).