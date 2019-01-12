-module(prop_base).
-include_lib("proper/include/proper.hrl").
-export([switchOnAllPumps/1, switchOffAllPumps/1]).

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

prop_test_random_system_pump_switched_off() ->
	?FORALL({N_pipes,N_pumps,N_he},{integer(5,20),integer(1,5),integer(1,5)},begin

			{Result,OldFlow,Flow} = test_random_system_switched_off(N_pipes,N_pumps,N_he),
			?WHENFAIL(io:format("~nFlow mismatch: ~p <-> ~p~n",[OldFlow,Flow]),Result)
		end).

prop_test_random_system_pump_switched_on() ->
	?FORALL({N_pipes,N_pumps,N_he},{integer(5,20),integer(1,5),integer(1,5)},
		begin

			{Result,OldFlow,Flow} = test_random_system_switched_on(N_pipes,N_pumps,N_he),
			?WHENFAIL(io:format("~nFlow mismatch: ~p <-> ~p~n",[OldFlow,Flow]),Result)
		end).

prop_test_digital_twin_generation()->
	?FORALL({N_pipes,N_pumps,N_he},{integer(5,20),integer(1,5),integer(1,5)},
		begin
			{Result,ConditionList} = test_digital_twin_generation(N_pipes,N_pumps,N_he),
			?WHENFAIL(
				begin
					[C1,C2,C3,C4,C5,C6,{C7,{TempA1,TempA2},{TempB1,TempB2}}] = ConditionList,
					if
					((C1==false)or(C2==false)or(C3==false))->
						io:format("Fatal error:Systems don't correspond in size~n");
					(C4==false) ->
						io:format("Flow when pumps turned off is not zero~n");
					(C5==false) ->
						io:format("Pumps of original are not turned on, after turning on the twin~n");
					(C6==false) ->
						io:format("Timing mismatch in measuring real flow~n");
					(C7==false) ->
						io:format("Temperature error: (~p -> ~p) and (~p -> ~p)~n",[TempA1,TempA2,TempB1,TempB2]);
					true ->
						io:format("This should never occur~n")
					end
				end
			,Result)
		end).

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
	{_,Pumps,FlowMeter,_} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true),
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

test_random_system_switched_off(N_pipes,N_pumps,N_he)->
	{_,Pumps,FlowMeter,_} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true),
	%io:format("Testing randomly generated system with ~p pipes, ~p pumps and ~p heat exchangers~n",[N_pipes,N_pumps,N_he]),
	switchOnAllPumps(Pumps),
	{ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create(Pumps,FlowMeter,GetSystemFlowPid),


	
	%%Now, after a random time, the pump is switched off
	timer:sleep(rand:uniform(50)+10),
	{ok,{_,OldFlow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	switchOffAllPumps(Pumps),
	% TimingInformation = {[{0,true},{N0,false}],0,true},
	% io:format("Received ~p, Timing information:~p~n",[{N0,MeasuredFlow},TimingInformation]),
	timer:sleep(rand:uniform(50)+10),

	
	{ok,{_N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	%CorrectFlow = testFunctions:flowForAnySituationWithPumpControl(N,N_pipes,N_pumps,TimingInformation),
	%io:format("N:~p| ~p pipes, ~p pump(s), Flow = ~p, CorrectFlow = ~p|~n",[N,N_pipes,N_pumps,Flow,CorrectFlow]),
	%io:format("Old ~p, New ~p~n",[OldFlow,Flow]),
	Result = (OldFlow>Flow),
	
	SystemFlowPid ! stop,
    getSystemFlow:stopSystemFlow(GetSystemFlowPid),
	{Result,OldFlow,Flow}.

test_random_system_switched_on(N_pipes,N_pumps,N_he)->
	{_,Pumps,FlowMeter,_} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true),
	%io:format("Testing randomly generated system with ~p pipes, ~p pumps and ~p heat exchangers~n",[N_pipes,N_pumps,N_he]),
	{ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create(Pumps,FlowMeter,GetSystemFlowPid),


	
	%%Now, after a random time, the pump is switched off
	timer:sleep(rand:uniform(50)),
	{ok,{_,OldFlow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	switchOnAllPumps(Pumps),
	timer:sleep(rand:uniform(50)),
	{ok,{_,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	%TimingInformation = {[{0,false},{N0,true}],0,false},
	%io:format("Timing information:~p~n",[TimingInformation]),
	%{ok,{N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	%CorrectFlow = testFunctions:flowForAnySituationWithPumpControl(N,N_pipes,N_pumps,TimingInformation),
	%io:format("N:~p| ~p pipes, ~p pump(s), Flow = ~p, CorrectFlow = ~p|~n",[N,N_pipes,N_pumps,Flow,CorrectFlow]),
	%Result = (abs(CorrectFlow-Flow)<1),
	Result = (OldFlow < Flow),
	SystemFlowPid ! stop,
    getSystemFlow:stopSystemFlow(GetSystemFlowPid),
	{Result,OldFlow,Flow}.

test_digital_twin_generation(N_pipes,N_pumps,N_Hex)->
	survivor:start(),
	DifList = testFunctions:generateDifList(N_Hex,[]),
	{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_Hex,false,DifList),
	{ok, GetSystemFlowPid1} = getSystemFlow:create(),
    {ok,SystemFlowPid1} = systemFlow:create(Pumps1,FlowMeterInst1,GetSystemFlowPid1),
	{ok, GetSystemTempPid1} = getSystemTemp:create(),
    {ok, SystemTempPid1} = systemTemp:create(HeatExchangers1,GetSystemFlowPid1,GetSystemTempPid1),

	{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2} = buildSystem:generateDigitalTwin({Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1,GetSystemFlowPid1,DifList}),
	{ok, GetSystemFlowPid2} = getSystemFlow:create(),
    {ok,SystemFlowPid2} = systemFlow:create(Pumps2,FlowMeterInst2,GetSystemFlowPid2),
	{ok, GetSystemTempPid2} = getSystemTemp:create(),
    {ok, SystemTempPid2} = systemTemp:create(HeatExchangers2,GetSystemFlowPid2,GetSystemTempPid2),


	%% First test: Assert that the number of pipes, pumps and heat exchangers is the same:
	Condition1 = length(Pipes1)==length(Pipes2),
	Condition2 = length(Pumps1)==length(Pumps2),
	Condition3 = length(HeatExchangers1)==length(HeatExchangers2),
	%io:format("Conditions 1,2,3 are ~p, ~p and ~p~n",[Condition1,Condition2,Condition3]),
	Check1 = (Condition1 == true) and (Condition2 == true) and (Condition3==true),
	
	%% Second test: Check that both systems give a flow of 0 when all pumps are turned off:
	{ok,{_,FlowOff1}} = getSystemFlow:getSystemFlow(GetSystemFlowPid1),
	{ok,{_,FlowOff2}} = getSystemFlow:getSystemFlow(GetSystemFlowPid2),
	Condition4 = (FlowOff1 == FlowOff2) and (FlowOff1 == 0),
	Check2 = Check1 and Condition4,

	%% Third test: Check that, if the twin turns on its pumps, the original system follows:
	switchOnAllPumps(Pumps2),
	%Wait a little while to be sure that all switching signals are handled correctly
	timer:sleep(1),
	Condition5 = areSwitchedOn(Pumps1),
	Check3 = Check2 and Condition5,

	timer:sleep(2),
	% Fourth test: Check that the digital twin can ask for the real flow of the original twin
	{ok, {_,RealFlow1}} = getSystemFlow:getSystemFlow(GetSystemFlowPid1),
	{ok, {_,RealFlow2}} = flowMeterInst:measure_flow(FlowMeterInst2),
	%io:format("Comparing flows: ~p ~p~n",[RealFlow1,RealFlow2]),
	
	if((RealFlow1==RealFlow2) == false) ->
		%Sometimes a little extra delay is necessary to account for the discrepancy in startup time
		timer:sleep(1),
		{ok, {_,RealFlow3}} = getSystemFlow:getSystemFlow(GetSystemFlowPid1),
		{ok, {_,RealFlow4}} = flowMeterInst:measure_flow(FlowMeterInst2),
		Condition6 = (RealFlow3==RealFlow4);
	true->
		Condition6 = (RealFlow1==RealFlow2)
	end,
	Check4 = Check3 and Condition6,
	

	%% Fifth test: Heat exchangement
	%Test that the temperature of the systems either increases or decreases after some delay,
	%but that the trend is equal for both systems
	{ok,{_,TempA1}} = getSystemTemp:getSystemTemp(GetSystemTempPid1),
	{ok,{_,TempB1}} = getSystemTemp:getSystemTemp(GetSystemTempPid2),
	timer:sleep(40),
	{ok,{_,TempA2}} = getSystemTemp:getSystemTemp(GetSystemTempPid1),
	{ok,{_,TempB2}} = getSystemTemp:getSystemTemp(GetSystemTempPid2),
	A1Rounded = testFunctions:round(TempA1,5),
	A2Rounded = testFunctions:round(TempA2,5),
	B1Rounded = testFunctions:round(TempB1,5),
	B2Rounded = testFunctions:round(TempB2,5),
	if(A1Rounded >= A2Rounded) ->
		Condition7 = (TempB1 >= TempB2);
	true ->
		Condition7 = (B1Rounded =< B2Rounded)
	end,
	%io:format("(~p -> ~p and ~p -> ~p)~n",[TempA1,TempA2,TempB1,TempB2]),
	Check5 = Check4 and Condition7,
	%Correctly terminate running processes
	SystemFlowPid1 ! stop,
	SystemFlowPid2 ! stop,
	getSystemFlow:stopSystemFlow(GetSystemFlowPid1),
	getSystemFlow:stopSystemFlow(GetSystemFlowPid2),
	SystemTempPid1 ! stop,
	SystemTempPid2 ! stop,
	getSystemTemp:stopSystemTemp(GetSystemTempPid1),
	getSystemTemp:stopSystemTemp(GetSystemTempPid2),
	survivor ! stop,

	%Return value is test result
	%If all test succeeded, the digital twin is correctly constructed
	{Check5,[Condition1,Condition2,Condition3,Condition4,Condition5,Condition6,{Condition7,{TempA1,TempA2},{TempB1,TempB2}}]}.

switchOnAllPumps([Pump])->
	pumpInst:switch_on(Pump);

switchOnAllPumps([Pump|OtherPumps])->
	pumpInst:switch_on(Pump),
	switchOnAllPumps(OtherPumps).

switchOffAllPumps([Pump])->
	pumpInst:switch_off(Pump);

switchOffAllPumps([Pump|OtherPumps])->
	pumpInst:switch_off(Pump),
	switchOffAllPumps(OtherPumps).

areSwitchedOn([Pump])->
	{ok,OnOff} = pumpInst:is_on(Pump),
	if(OnOff==off)->
		false;
	true->
		true
	end;

areSwitchedOn([Pump|Pumps])->
	{ok,OnOff} = pumpInst:is_on(Pump),
	if(OnOff==off)->
		false;
	true->
		areSwitchedOn(Pumps)
	end.

% switch(_,_,0,_,SwitchingList,_,FlowList)->
% 	{SwitchingList,FlowList};

% switch(SamplePump,Pumps,NumberOfSwitches,RequiredTime,SwitchingList,GetSystemFlowPid,FlowList)->
% 	{ok,{N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
% 	if (N>=RequiredTime)->
% 		%%Switch and update switching list
% 		{ok,OnOff} = pumpInst:is_on(SamplePump),
% 		if(OnOff == on)->
% 			NewSwitchingList = SwitchingList ++ [{N,false}],
% 			switchOffAllPumps(Pumps);
% 		true->
% 			NewSwitchingList = SwitchingList ++ [{N,true}],
% 			switchOnAllPumps(Pumps)
% 		end,
% 		%Generate new Required Time
% 		NewRequiredTime = N + rand:uniform(20),
% 		switch(SamplePump,Pumps,NumberOfSwitches-1,NewRequiredTime,NewSwitchingList,GetSystemFlowPid,FlowList++[Flow]);
% 	true->
% 		switch(SamplePump,Pumps,NumberOfSwitches,RequiredTime,SwitchingList,GetSystemFlowPid,FlowList)
% 	end.

