-module(connectorSupervisor).
-behaviour(supervisor).

-export([create/1,init/1]).


create({Begin_N,End_N})->
    supervisor:start_link({local,?MODULE},?MODULE,[{Begin_N,End_N}]).

init([{Begin_N,End_N}])->
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},

    %% First, connect all the pipes:
    %Generate a list of all pipes for the given information
    io:format("Connecting the pipes~n"),
    Pipes = fluidumSupervisor:generateNPipesIds(Begin_N,End_N),
    buildSystem:connectPipesCircle(Pipes),

    % %% First, connect all the pipes:
    % %Generate a list of all pipes for the given information
    % Pipes = generateNPipesIds(Begin_N,End_N),
    % buildSystem:connectPipesCircle(Pipes),

    % %Next, create a fluimum and fill the pipes with it
    % FluidumTypSpec = #{id => fluidumtyp_child,
    %                 start => {fluidumTyp, createRegister, []},
    %                 restart => permanent,
    %                 shutdown => brutal_kill,
    %                 type => worker,
    %                 modules => [fluidumTyp]},
    % [FirstPipe] = generateNPipesIds(Begin_N,Begin_N),
    % {ok,[C,_]} = resource_instance:list_connectors(FirstPipe),
    
    % FluidumInstSpec = #{id => fluiduminst_child,
    %                 start => {fluidumInst, create, [C,fluidumTyp,0]},
    %                 restart => permanent,
    %                 shutdown => brutal_kill,
    %                 type => worker,
    %                 modules => [fluidumInst]},
    % %Also create a supervisor to fill the circuit with the created fluidum + install the pumps
    % FillCircuitSupervisorSpec = #{id => fillcircuitsupervisor_child,
    %                             start => {fillCircuitSupervisor, create, [Begin_N,End_N,fluidumInst_0]},
    %                             restart => permanent,
    %                             shutdown => brutal_kill,
    %                             type => supervisor,
    %                             modules => [fillCircuitSupervisor]},

    % %Finally create a supervisor that takes care of the other parts of the circuit
    % PumpFlowmeterHESupervisorSpec = #{id => pumpFlowmeterHESupervisor_child,
    %                                 start => {pumpFlowmeterHESupervisor, create, [{Begin_N,End_N},N_pumps,N_hex]},
    %                                 restart => permanent,
    %                                 shutdown => brutal_kill,
    %                                 type => supervisor,
    %                                 modules => [pumpFlowmeterHESupervisor]},
    % ChildSpecs = [FluidumTypSpec,FluidumInstSpec,FillCircuitSupervisorSpec,PumpFlowmeterHESupervisorSpec],
    % {ok,{SupFlags,ChildSpecs}}.
    {ok,{SupFlags,[]}}.
