-module(pumpFlowmeterHESupervisor).
-behaviour(supervisor).
-export([create/4,init/1]).
-export([getChildren/0]).
-export([generateNPumpIds/2]).

create({Begin_N,End_N},N_pumps,N_hex,StartDigitalTwin)->
    supervisor:start_link({local,?MODULE},?MODULE,[{Begin_N,End_N},N_pumps,N_hex,StartDigitalTwin]).

init([{Begin_N,End_N},N_pumps,N_hex,false])->
    SupFlags = #{strategy => one_for_all, intensity => N_pumps+N_hex+1, period => 5},

    %Create pump specs:
    PumpTypSpec = #{id => pumptyp_child,
                    start => {pumpTyp, createRegister, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [pumpTyp]},
    Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
    Pipes = fluidumSupervisor:generateNPipesIds(Begin_N,End_N),
    PumpInstSpecs = generateNPumpsSpecs(1,N_pumps,Pipes,Fun),
    PumpSpecs = [PumpTypSpec] ++ PumpInstSpecs,
    
    %% Create flowMeter specs:
    %Select random pipe to put the flowmeter on:
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipeInst = lists:nth(RandomIndex,Pipes),
    FlowMeterCMD = fun()->
					{ok,real_flow}
				end,

    FlowMeterTypSpec = #{id => flowmetertyp_child,
                    start => {flowMeterTyp, createRegister, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [flowMeterTyp]},

    FlowMeterInstSpec = #{id => flowmeterinst_child,
                        start => {flowMeterInst, create, [self(),flowMeterTyp,RandomPipeInst,FlowMeterCMD,0]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => worker,
                        modules => [flowMeterInst]},
    FlowMeterSpecs = [FlowMeterTypSpec,FlowMeterInstSpec],

    %% Create heat exchanger specs:
    HeatExchangerTypSpec = #{id => heatexchangertyp_child,
                            start => {heatExchangerTyp, createRegister, []},
                            restart => permanent,
                            shutdown => brutal_kill,
                            type => worker,
                            modules => [heatExchangerTyp]},
    DifList = testFunctions:generateDifList(N_hex,[]),
    HeatExchangerInstSpecs = generateNHexSpecs(1,N_hex,Pipes,DifList),
    HeatExchangerSpecs = [HeatExchangerTypSpec] ++ HeatExchangerInstSpecs,

    %% Insert a systemFlow:
    Pumps = generateNPumpIds(1,N_pumps),
    SystemFlowSpec = #{id => systemflow_child,
                    start => {systemFlow, create, [Pumps,flowMeterInst_0,100,0]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemFlow]},
    %% Insert a systemTemp:
    HEX = generateNHEXIds(1,N_hex),
    SystemTempSpec = #{id => systemtemp_child,
                    start => {systemTemp, create, [HEX,systemFlow_0,120,0]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemTemp]},

    ChildSpecs = PumpSpecs++FlowMeterSpecs++HeatExchangerSpecs++[SystemFlowSpec]++[SystemTempSpec],
    {ok,{SupFlags,ChildSpecs}};

init([{Begin_N,End_N},N_pumps,N_hex,true])->
    SupFlags = #{strategy => one_for_all, intensity => N_pumps+N_hex+1, period => 5},

    %Create pump specs:
    PumpTypSpec = #{id => pumptyp_child,
                    start => {pumpTyp, createRegister, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [pumpTyp]},
    Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
    Pipes = fluidumSupervisor:generateNPipesIds(Begin_N,End_N),
    PumpInstSpecs = generateNPumpsSpecs(1,N_pumps,Pipes,Fun),
    PumpSpecs = [PumpTypSpec] ++ PumpInstSpecs,
    
    %% Create flowMeter specs:
    %Select random pipe to put the flowmeter on:
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipeInst = lists:nth(RandomIndex,Pipes),
    FlowMeterCMD = fun()->
					{ok,real_flow}
				end,

    FlowMeterTypSpec = #{id => flowmetertyp_child,
                    start => {flowMeterTyp, createRegister, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [flowMeterTyp]},

    FlowMeterInstSpec = #{id => flowmeterinst_child,
                        start => {flowMeterInst, create, [self(),flowMeterTyp,RandomPipeInst,FlowMeterCMD,0]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => worker,
                        modules => [flowMeterInst]},
    FlowMeterSpecs = [FlowMeterTypSpec,FlowMeterInstSpec],

    %% Create heat exchanger specs:
    HeatExchangerTypSpec = #{id => heatexchangertyp_child,
                            start => {heatExchangerTyp, createRegister, []},
                            restart => permanent,
                            shutdown => brutal_kill,
                            type => worker,
                            modules => [heatExchangerTyp]},
    DifList = testFunctions:generateDifList(N_hex,[]),
    HeatExchangerInstSpecs = generateNHexSpecs(1,N_hex,Pipes,DifList),
    HeatExchangerSpecs = [HeatExchangerTypSpec] ++ HeatExchangerInstSpecs,

    %% Insert a systemFlow:
    Pumps = generateNPumpIds(1,N_pumps),
    SystemFlowSpec = #{id => systemflow_child,
                    start => {systemFlow, create, [Pumps,flowMeterInst_0,100,0]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemFlow]},
    %% Insert a systemTemp:
    HEX = generateNHEXIds(1,N_hex),
    SystemTempSpec = #{id => systemtemp_child,
                    start => {systemTemp, create, [HEX,systemFlow_0,120,0]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemTemp]},

    DigitalTwinSpec = #{id => systemsupervisor_twin_child,
                        start => {systemSupervisorTwin, create, [7,4,3,DifList]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [systemSupervisorTwin]},

    ChildSpecs = PumpSpecs++FlowMeterSpecs++HeatExchangerSpecs++[SystemFlowSpec]++[SystemTempSpec]++[DigitalTwinSpec],
    {ok,{SupFlags,ChildSpecs}}.

getChildren()->
    supervisor:which_children(?MODULE).

generateNPumpsSpecs(N,N,Pipes,Fun)->
    Id = list_to_atom(string:concat("pumpinst_child",integer_to_list(N))),
    
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {pumpInst, create, [self(),pumpTyp,RandomPipe,Fun,N]},
                modules => [pipeInst]},
    lists:delete(RandomPipe,Pipes),
    [PipeInst];

generateNPumpsSpecs(CurrentN,N,Pipes,Fun)->
    Id = list_to_atom(string:concat("pumpinst_child",integer_to_list(CurrentN))),
    
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {pumpInst, create, [self(),pumpTyp,RandomPipe,Fun,CurrentN]},
                modules => [pipeInst]},
    lists:delete(RandomPipe,Pipes),

    [PipeInst]++generateNPumpsSpecs(CurrentN+1,N,Pipes,Fun).


generateNHexSpecs(N,N,Pipes,[Dif|_])->
    Id = list_to_atom(string:concat("heatexchangerinst_child",integer_to_list(N))),
    HE_link_spec = #{delta => Dif},
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {heatExchangerInst, create, [self(),heatExchangerTyp,RandomPipe,HE_link_spec,N]},
                modules => [heatExchangerInst]},
    lists:delete(RandomPipe,Pipes),
    [PipeInst];

generateNHexSpecs(CurrentN,N,Pipes,[Dif|Difs])->
    Id = list_to_atom(string:concat("heatexchangerinst_child",integer_to_list(CurrentN))),
    HE_link_spec = #{delta => Dif},
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {heatExchangerInst, create, [self(),heatExchangerTyp,RandomPipe,HE_link_spec,CurrentN]},
                modules => [heatExchangerInst]},
    
    lists:delete(RandomPipe,Pipes),
    [PipeInst]++generateNHexSpecs(CurrentN+1,N,Pipes,Difs).

generateNPumpIds(N,N)->
    Atom = testFunctions:numberToAtom(pumpInst,N),
    [Atom];

generateNPumpIds(CurrentN,N)->
    Atom = testFunctions:numberToAtom(pumpInst,CurrentN),
    [Atom] ++ generateNPumpIds(CurrentN+1,N).

generateNHEXIds(N,N)->
    Atom = testFunctions:numberToAtom(heatExchangerInst,N),
    [Atom];

generateNHEXIds(CurrentN,N)->
    Atom = testFunctions:numberToAtom(heatExchangerInst,CurrentN),
    [Atom] ++ generateNHEXIds(CurrentN+1,N).
