-module(prop_base).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_discover_circuit() ->
	?FORALL(Int,integer(2,100),testFunctions:list_is_true(test_discover_circuit_N(Int))).

prop_test_flow_influence() ->
	%Generate a network with a pump
	%Request the influence for a given flow on that pump
	%Calculate the influence, and compare the 2 values
	%Repeat this many times
	survivor:start(),
	Result = ?FORALL(Flow,integer(0,1000),test_flow_influence(Flow)),
	%Give all processes the time to do whatever they are doing
	%The stop the survivor
	%If there is no delay, the survivor will be stopped
	%while the test_flow_influeces processes are still running
	%potentially causes significant issues
	timer:send_after(1000,survivor,stop),
	Result.


prop_test_random_system_pump_always_on() ->
	%% Test a randomly generated system, with all pumps always turned on

	%There cannot be more heat exchangers or pumps than pipes in the system
	?FORALL({N_pipes,N_pumps,N_he},{integer(5,20),integer(1,5),integer(1,5)},test_random_system_always_on(N_pipes,N_pumps,N_he)).

prop_test_random_system_pump_random_on() ->
	%% Test a randomly generated system, with all randomly on or off
	%% Whenever a pump is turned on, there is an 80% chance it remains turned on
	%% Whenever a pump is turned off, there is an 80% chance it remains turned off
	%There cannot be more heat exchangers or pumps than pipes in the system
	true.
	%?FORALL({N_pipes,N_pumps,N_he},{integer(3,20),integer(1,N_pipes),integer(1,N_pipes)},true).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
test_discover_circuit_N(N) ->
	if N >1 ->
		%io:format("Creating circuit with ~p pipes...~n",[N]),
		{n_connected_circle,_N,Pipes} = buildSystem:start_Npipes_circle(N),
		%io:format("Circuit created...~n"),
		[RootC|Connectors] = buildSystem:getAllConnectors(Pipes),
		%io:format("Connectors collected~n"),
		{ok,{RootC,CircuitMap}} = fluidumTyp:discover_circuit(RootC),
		%io:format("Circuit discovered...~n"),
		List = testFunctions:check_circuit_map([RootC]++Connectors,CircuitMap),
		buildSystem:stop(),
		List;
	true ->
		[true]
	end.
	
test_flow_influence(Flow)->
	{ok,{_,_,_,_,_,_,PumpInst,_}} = buildSystem:start_3pipes_water_pump(false),
	pumpInst:switch_on(PumpInst),
	{ok, InfluenceFunction1} = pumpInst:flow_influence(PumpInst),
	Influence1 = InfluenceFunction1(Flow),
	Influence2 = 250 - 5 * Flow - 2 * Flow * Flow,
	%buildSystem:stop(),
	Influence1 == Influence2.

test_random_system_always_on(N_pipes,N_pumps,N_he)->
	{Pipes,Pumps,FlowMeter,HeatExchangers} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true),
	switchOnAllPumps(Pumps),
	{ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create(Pumps,FlowMeter,GetSystemFlowPid),

	timer:sleep(rand:uniform(100)),
	{ok,{N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	CorrectFlow = testFunctions:flowForAnySituation(N,N_pipes,N_pumps),
	%io:format("N:~p| ~p pipes, ~p pumps, Flow = ~p, CorrectFlow = ~p~n",[N,N_pipes,N_pumps,Flow,CorrectFlow]),
	Result = (abs(CorrectFlow-Flow)<1),
	
	SystemFlowPid ! stop,
    getSystemFlow:stopSystemFlow(GetSystemFlowPid),
	Result.

test_random_system_random_on(N_pipes,N_pumps,N_he)->
	{Pipes,Pumps,FlowMeter,HeatExchangers} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true),
	[Pump1|_] = Pumps,
	switchOnAllPumps(Pumps),
	{ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create(Pumps,FlowMeter,GetSystemFlowPid),


	SwitchingList = [{0,true}],
	%%Now there are 4 moments where there is a chance the status of the pump (on/off) will be flipped
	timer:sleep(rand:uniform(25)),
	%There is a 30% that the pump will be toggled on/off
	RandomNumber0 = (1.0-rand:uniform()),
	if(RandomNumber0>=0.7)->
		{ok,OnOff0} = pumpInst:is_on(Pump1),
		{ok,{N0,_}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
		if(OnOff0==on) ->
			SwitchingList++[{N0,false}],
			switchOffAllPumps(Pumps);
		true->
			SwitchingList++[{N0,true}],
			switchOnAllPumps(Pumps)
		end;
	true ->
		ok
	end,

	timer:sleep(rand:uniform(25)),
	%There is a 30% that the pump will be toggled on/off
	RandomNumber1 = (1.0-rand:uniform()),
	if(RandomNumber1>=0.7)->
		{ok,OnOff1} = pumpInst:is_on(Pump1),
		{ok,{N1,_}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
		if(OnOff1==on) ->
			SwitchingList++[{N1,false}],
			switchOffAllPumps(Pumps);
		true->
			SwitchingList++[{N1,true}],
			switchOnAllPumps(Pumps)
		end;
	true ->
		ok
	end,

	timer:sleep(rand:uniform(25)),
	%There is a 30% that the pump will be toggled on/off
	RandomNumber2 = (1.0-rand:uniform()),
	if(RandomNumber2>=0.7)->
		{ok,OnOff2} = pumpInst:is_on(Pump1),
		{ok,{N2,_}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
		if(OnOff2==on) ->
			SwitchingList++[{N2,false}],
			switchOffAllPumps(Pumps);
		true->
			SwitchingList++[{N2,true}],
			switchOnAllPumps(Pumps)
		end;
	true ->
		ok
	end,

	timer:sleep(rand:uniform(25)),
	%There is a 30% that the pump will be toggled on/off
	RandomNumber3 = (1.0-rand:uniform()),
	if(RandomNumber3>=0.7)->
		{ok,OnOff3} = pumpInst:is_on(Pump1),
		{ok,{N3,_}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
		if(OnOff3==on) ->
			SwitchingList++[{N3,false}],
			switchOffAllPumps(Pumps);
		true->
			SwitchingList++[{N3,true}],
			switchOnAllPumps(Pumps)
		end;
	true ->
		ok
	end,

	TimingInformation = {SwitchingList,0,true},
	{ok,{N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	CorrectFlow = testFunctions:flowForAnySituationWithPumpControl(N,N_pipes,N_pumps,TimingInformation),
	%io:format("N:~p| ~p pipes, ~p pumps, Flow = ~p, CorrectFlow = ~p~n",[N,N_pipes,N_pumps,Flow,CorrectFlow]),
	Result = (abs(CorrectFlow-Flow)<1),
	
	SystemFlowPid ! stop,
    getSystemFlow:stopSystemFlow(GetSystemFlowPid),
	Result.

switchOnAllPumps([Pump])->
	pumpInst:switch_on(Pump);

switchOnAllPumps([Pump|OtherPumps])->
	pumpInst:switch_on(Pump),
	switchOnAllPumps(OtherPumps).

switchOffAllPumps([Pump])->
	pumpInst:switch_off(Pump);

switchOffAllPumps([Pump|OtherPumps])->
	pumpInst:switch_off(Pump),
	switchOnAllPumps(OtherPumps).