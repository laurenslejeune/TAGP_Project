-module(pumpFlowmeterHESupervisorTwin).
-behaviour(supervisor).
-export([create/4,init/1]).
-export([getChildren/0]).
-export([generateNPumpIds/2]).

create({Begin_N,End_N},N_pumps,N_hex,DifList)->
    supervisor:start_link({local,?MODULE},?MODULE,[{Begin_N,End_N},N_pumps,N_hex,DifList]).

init([{Begin_N,End_N},N_pumps,N_hex,DifList])->
    SupFlags = #{strategy => one_for_all, intensity => N_pumps+N_hex+1, period => 5},

    %Create pump specs:
    Pumps1 = generateNPumpIds(1,N_pumps),
    Pipes = fluidumSupervisor:generateNPipesIds(Begin_N,End_N),
    %io:format("~p|Generating ~p -> ~p pumps~n",[N_pumps,N_pumps+1,2*N_pumps]),
    PumpInstSpecs = generateNPumpsSpecsTwin(N_pumps+1,2*N_pumps,Pipes,Pumps1),
    PumpSpecs = PumpInstSpecs,
    
    %% Create flowMeter specs:
    %Select random pipe to put the flowmeter on:
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipeInst = lists:nth(RandomIndex,Pipes),
    FlowMeterCMD = fun()->
					{ok,{N,Flow}} = systemFlow:getSystemFlow(systemFlow_0),
					{N,Flow}
				end,

    FlowMeterInstSpec = #{id => flowmeterinst_child,
                        start => {flowMeterInst, create, [self(),flowMeterTyp,RandomPipeInst,FlowMeterCMD,1]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => worker,
                        modules => [flowMeterInst]},
    FlowMeterSpecs = [FlowMeterInstSpec],

    %% Create heat exchanger specs:
    
    HeatExchangerInstSpecs = generateNHexSpecs(N_hex+1,2*N_hex,Pipes,DifList),
    HeatExchangerSpecs = HeatExchangerInstSpecs,

    %% Insert a systemFlow:
    Pumps2 = generateNPumpIds(N_pumps+1,2*N_pumps),
    SystemFlowSpec = #{id => systemflow_child,
                    start => {systemFlow, create, [Pumps2,flowMeterInst_1,100,1]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemFlow]},
    %% Insert a systemTemp:
    HEX = generateNHEXIds(N_hex+1,2*N_hex),
    SystemTempSpec = #{id => systemtemp_child,
                    start => {systemTemp, create, [HEX,systemFlow_1,120,1]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemTemp]},

    ChildSpecs = PumpSpecs++FlowMeterSpecs++HeatExchangerSpecs++[SystemFlowSpec]++[SystemTempSpec],
    {ok,{SupFlags,ChildSpecs}}.

getChildren()->
    supervisor:which_children(?MODULE).

generateNPumpsSpecsTwin(N,N,Pipes,[RefPump|_])->
    %io:format("Creating pump ~p~n",[N]),
    Id = list_to_atom(string:concat("pumpinst_child",integer_to_list(N))),
    Fun = fun(on) ->
			pumpInst:switch_on(RefPump),
            io:format("Switching on ~p~n",[RefPump]),
			{ok,on};
			(off)->
			pumpInst:switch_off(RefPump),
            io:format("Switching off ~p~n",[RefPump]),
			{ok,off}
		end,
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {pumpInst, create, [self(),pumpTyp,RandomPipe,Fun,N]},
                modules => [pumpInst]},
    lists:delete(RandomPipe,Pipes),
    [PipeInst];

generateNPumpsSpecsTwin(CurrentN,N,Pipes,[RefPump|RefPumps])->
    %io:format("Creating pump ~p -> ~p~n",[CurrentN,N]),
    Id = list_to_atom(string:concat("pumpinst_child",integer_to_list(CurrentN))),
    Fun = fun(on) ->
			pumpInst:switch_on(RefPump),
            io:format("Switching on ~p~n",[RefPump]),
			{ok,on};
			(off)->
            io:format("Switching off ~p~n",[RefPump]),
			pumpInst:switch_off(RefPump),
			{ok,off}
		end,
    RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

    PipeInst = #{id => Id,
                start => {pumpInst, create, [self(),pumpTyp,RandomPipe,Fun,CurrentN]},
                modules => [pumpInst]},
    lists:delete(RandomPipe,Pipes),

    [PipeInst]++generateNPumpsSpecsTwin(CurrentN+1,N,Pipes,RefPumps).


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
