-module(systemSupervisor).
-behaviour(supervisor).
-export([create/3,init/1]).
-export([getChildren/0]).

create(N_pipes,N_pumps,N_hex)->
    supervisor:start_link({local,?MODULE},?MODULE,[N_pipes,N_pumps,N_hex]).

init([N_pipes,N_pumps,N_hex])->

    SupFlags = #{strategy => one_for_all, intensity => N_pipes+3, period => 5},
    %Generating all child specs:
    %First, the pipes are generated
    PipeTypSpec = #{id => pipetyp_child,
                start => {pipeTyp, createRegister, []},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => [pipeTyp]},
    PipeInstSpecs = generateNPipesSpecs(1,N_pipes),
    PipeSpecs = [PipeTypSpec] ++ PipeInstSpecs,
    
    %Create a supervisor that connects all pipes
    ConnectorSupervisorSpec = #{id => connector_supervisor_child,
                                start => {connectorSupervisor, create, [{1,N_pipes}]},
                                restart => permanent,
                                shutdown => brutal_kill,
                                type => supervisor,
                                modules => [connectorSupervisor]},
    %Finally, create a supervisor that takes care of the rest of the system
    FluidumSupervisorSpec = #{id => fluidumsupervisor_child,
                            start => {fluidumSupervisor, create, [{N_pipes,1,N_pipes},N_pumps,N_hex]},
                            restart => permanent,
                            shutdown => brutal_kill,
                            type => supervisor,
                            modules => [fluidumSupervisor]},
    ChildSpecs = PipeSpecs ++ [ConnectorSupervisorSpec] ++ [FluidumSupervisorSpec],
    %ChildSpecs = PipeSpecs ++ [ConnectorSupervisorSpec],
    {ok,{SupFlags,ChildSpecs}}.

getChildren()->
    supervisor:which_children(?MODULE).

generateNPipesSpecs(N,N)->
    Id = list_to_atom(string:concat("pipeinst_child",integer_to_list(N))),
    PipeInst = #{id => Id,
                start => {pipeInst, create, [self(),pipeTyp,N]},
                modules => [pipeInst]},
    [PipeInst];
generateNPipesSpecs(CurrentN,N)->
    Id = list_to_atom(string:concat("pipeinst_child",integer_to_list(CurrentN))),
    PipeInst = #{id => Id,
                start => {pipeInst, create, [self(),pipeTyp,CurrentN]},
                modules => [pipeInst]},
    [PipeInst]++generateNPipesSpecs(CurrentN+1,N).
