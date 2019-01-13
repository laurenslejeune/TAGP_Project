-module(systemSupervisorTwin).
-behaviour(supervisor).
-export([create/4,init/1]).
-export([getChildren/0]).

create(N_pipes,N_pumps,N_hex,DifList)->
    supervisor:start_link({local,?MODULE},?MODULE,[N_pipes,N_pumps,N_hex,DifList]).

init([N_pipes,N_pumps,N_hex,DifList])->
    % Since we're working in a digital twin, no additional types need to be defined:

    SupFlags = #{strategy => one_for_all, intensity => N_pipes+3, period => 5},
    %Generating all child specs:
    %First, the pipes are generated
    PipeInstSpecs = generateNPipesSpecs(N_pipes+1,2*N_pipes),
    PipeSpecs = PipeInstSpecs,
    
    %Create a supervisor that connects all pipes
    ConnectorSupervisorSpec = #{id => connector_supervisor_twin_child,
                                start => {connectorSupervisorTwin, create, [{N_pipes+1,2*N_pipes}]},
                                restart => permanent,
                                shutdown => brutal_kill,
                                type => supervisor,
                                modules => [connectorSupervisorTwin]},
    %Finally, create a supervisor that takes care of the rest of the system
    FluidumSupervisorSpec = #{id => fluidumsupervisor_twin_child,
                            start => {fluidumSupervisorTwin, create, [{N_pipes,N_pipes+1,2*N_pipes},N_pumps,N_hex,DifList]},
                            restart => permanent,
                            shutdown => brutal_kill,
                            type => supervisor,
                            modules => [fluidumSupervisorTwin]},
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
