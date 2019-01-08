-module(buildSystem_tests).
-include_lib("eunit/include/eunit.hrl").

-export([contains_el/2]).
-export([contains/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
	{"1) The system can be started and stopped, and all processes exist
	  2) Test if the given pipes are really connected to their connectors
	  3) Test if the network can be traversed, from one pipe to another
	  4) Test if the network can be traversed, storing information about Pipe, Connector and Location
	  5) Check if the pipe resource instances belong to the pipeTyp type",
	{foreach,
	fun start_3pipes/0,
	fun stop_survivor/1,
	[fun has_started/1,fun basic_connections/1,fun traverse_pipe/1,fun test_pipes_connectors_locations/1,fun test_pipe_type/1]}}.

% basic_connections_test_test_() ->
% 	{"Test if the given pipes are really connected to their connectors",
% 	{setup,
% 	fun start_3pipes/0,
% 	fun stop_survivor/1,
% 	fun basic_connections/1}}.	

% traverse_test_() ->
% 	{"Test if the network can be traversed, from one pipe to another",
% 	{setup,
% 	fun start_3pipes/0,
% 	fun stop_survivor/1,
% 	fun traverse_pipe/1}}.
	
% test_all_connectors_test_() ->
% 	{"Test if the network can be traversed, storing information about Pipe, Connector and Location",
% 	{setup,
% 	fun start_3pipes/0,
% 	fun stop_survivor/1,
% 	fun test_pipes_connectors_locations/1}}.

% pipe_type_test_() ->
% 	{"Check if the pipe resource instances belong to the pipeTyp type",
% 	{setup,
% 	fun start_3pipes/0,
% 	fun stop_survivor/1,
% 	fun test_pipe_type/1}}.

fluidum_basic_test() ->
	{"1) Check the basics functioning of the fluidum modules
	  2) Check the basic operations of the fluidum modules",
	{foreach,
	fun start_3pipes_water/0,
	fun stop_survivor/1,
	[fun test_fluidum_basics/1,fun test_fluidum_operations/1]}}.

% fluidum_operations_test() ->
% 	{"Check the basic operations of the fluidum modules",
% 	{setup,
% 	fun start_3pipes_water/0,
% 	fun stop_survivor/1,
% 	fun test_fluidum_operations/1}}.

pump_basic_test_()->
	{"1) Test the basics of creating a pump in a system.
	  2) Test the basics of operating a pump in a system",
	{foreach,
	fun start_3pipes_water_pump/0,
	fun stop/1,
	[fun test_pump_basics/1,fun test_pump_operation/1]}}.
	
% pump_operation_test_()->
% 	{"Test the basics of operating a pump in a system.",
% 	{setup,
% 	fun start_3pipes_water_pump/0,
% 	fun stop/1,
% 	fun test_pump_operation/1}}.

flowmeter_basic_test_()->
	{"1) Test the basics of creating a flowmeter in a system.
	  2) Test the basics of operating a flowmeter in a system.",
	{foreach,
	fun start_3pipes_water_pump_flowmeter/0,
	fun stop/1,
	[fun test_flowmeter_basics/1,fun test_flowmeter_operation/1]}}.

% flowmeter_operation_test_()->
% 	{"Test the basics of operating a flowmeter in a system.",
% 	{setup,
% 	fun start_3pipes_water_pump_flowmeter/0,
% 	fun stop/1,
% 	fun test_flowmeter_operation/1}}.

heatex_basic_test_()->
	{"1) Test the basics of creating a heat exchanger in a system.
	  2) Test the basics of operating a heat exchanger in a system.",
	{foreach,
	fun start_3pipes_water_pump_flowmeter_heatex/0,
	fun stop/1,
	[fun test_heatex_basics/1,fun test_heatex_operation/1]}}.

% heatex_operation_test_()->
% 	{"Test the basics of operating a heat exchanger in a system.",
% 	{setup,
% 	fun start_3pipes_water_pump_flowmeter_heatex/0,
% 	fun stop/1,
% 	fun test_heatex_operation/1}}.

fully_integrated_test_() ->
	{"Testing a custom, hand-made and fully-fledged system.",
	{setup,
	fun start_3pipes_water_pump_flowmeter_heatex/0,
	fun stop/1,
	fun test_fully_integrated_system/1}}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%Opmerking: Als de testmodule halverwege de tests crasht (en dus niet op de juiste manier afsluit),
%%wordt de start_link nooit beëindigd en kan je de start() niet opnieuw gebruiken omdat er een pipeTyp
%%proces running is

start_3pipes() ->
	survivor:start(),
	{ok, {PipeTypePID,Pipes,Connectors,Locations}} = buildSystem:start_3pipes(false),
	{PipeTypePID,Pipes,Connectors,Locations}.

start_3pipes_water()->
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid}} = buildSystem:start_3pipes_water(false),
	{PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid}.

start_3pipes_water_pump()->
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,PumpInst,PumpTypPID}} = buildSystem:start_3pipes_water_pump(true),
	{PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,PumpInst,PumpTypPID}.

start_3pipes_water_pump_flowmeter()->
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}} = buildSystem:start_3pipes_water_pump_flowmeter(true),
	{PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}.

start_3pipes_water_pump_flowmeter_heatex()->
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}} = buildSystem:start_3pipes_water_pump_flowmeter_heatex(true),
	{PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}.

stop_survivor(_)->
	survivor ! stop.

stop(_) ->
	buildSystem:stop().
 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

has_started({PipeTypePID,Pipes,Connectors,Locations}) ->
	[Pipe1, Pipe2, Pipe3|_RestPipes] = Pipes,
	[[C11, C12], [C21,C22], [C31,C32]|_RestConnectors] = Connectors,
	[L1, L2, L3|_RestLocations] = Locations,
	%check_has_started(Pipes,[]),
	%check_has_started(Locations,[]),
	[?_assert(erlang:is_process_alive(PipeTypePID)),
	 ?_assert(erlang:is_process_alive(Pipe1)),
	 ?_assert(erlang:is_process_alive(Pipe2)),
	 ?_assert(erlang:is_process_alive(Pipe3)),
	 ?_assert(erlang:is_process_alive(C11)),
	 ?_assert(erlang:is_process_alive(C12)),
	 ?_assert(erlang:is_process_alive(C21)),
	 ?_assert(erlang:is_process_alive(C22)),
	 ?_assert(erlang:is_process_alive(C31)),
	 ?_assert(erlang:is_process_alive(C32)),
	 ?_assert(erlang:is_process_alive(L1)),
	 ?_assert(erlang:is_process_alive(L2)),
	 ?_assert(erlang:is_process_alive(L3))].

basic_connections({_PipeTypePID,Pipes,Connectors,_Locations})->
	[Pipe1, Pipe2, Pipe3|_RestPipes] = Pipes,
	[[C11, C12], [C21,C22], [C31,C32]|_RestConnectors] = Connectors,
	
	{ok, [P1_C1,P1_C2]} = resource_instance:list_connectors(Pipe1),
	{ok, [P2_C1,P2_C2]} = resource_instance:list_connectors(Pipe2),
	{ok, [P3_C1,P3_C2]} = resource_instance:list_connectors(Pipe3),
	
	[?_assertEqual(C11,P1_C1),
	 ?_assertEqual(C12,P1_C2),
	 ?_assertEqual(C21,P2_C1),
	 ?_assertEqual(C22,P2_C2),
	 ?_assertEqual(C31,P3_C1),
	 ?_assertEqual(C32,P3_C2)].
	
traverse_pipe({_PipeTypePID,Pipes,_Connectors,_Locations}) ->
	[Pipe1, Pipe2, Pipe3|_RestPipes] = Pipes,
	
	{ok, [C11,C12]} = resource_instance:list_connectors(Pipe1),
	{ok,C11_C21} = connector:get_connected(C11),
	{ok,C12_C31} = connector:get_connected(C12),
	
    %?debugFmt("Getting the pipe of connector~p", [C11_C21]),
	{ok,C21_pipe} = connector:get_ResInst(C11_C21),
	{ok,C31_pipe} = connector:get_ResInst(C12_C31),
	
	[?_assertEqual(Pipe2,C31_pipe),
	 ?_assertEqual(Pipe3,C21_pipe)].

test_pipes_connectors_locations({_PipeTypePID,Pipes,Connectors,Locations}) ->
	[Pipe1, Pipe2, Pipe3|_RestPipes] = Pipes,
	[[C11, C12], [C21,C22], [C31,C32]|_RestConnectors] = Connectors,
	[L1, L2, L3|_RestLocations] = Locations,
	
	%We start from Pipe1 and slowly discover the entire network (consisting of 3 pipes and 6 connectors)
	
	%Find the connectors connected to Pipe1
	{ok, [PipeTest_C11,PipeTest_C12]} = resource_instance:list_connectors(Pipe1),
	
	%Find the locations corresponding to Pipe1
	{ok, [PipeTest_L1]}= resource_instance:list_locations(Pipe1),
	
	%Find the connectors connecting the found connectors to other pipes
	{ok,PipeTest_C21} = connector:get_connected(PipeTest_C11),
	{ok,PipeTest_C31} = connector:get_connected(PipeTest_C12),
	
	%Find those other pipes
	{ok,PipeTest_Pipe2} = connector:get_ResInst(PipeTest_C21),
	{ok,PipeTest_Pipe3} = connector:get_ResInst(PipeTest_C31),
	
	%Finally find the last connectors of those pipes
	{ok, [PipeTest_C2a,PipeTest_C2b]} = resource_instance:list_connectors(PipeTest_Pipe2),
	{ok, [PipeTest_C3a,PipeTest_C3b]} = resource_instance:list_connectors(PipeTest_Pipe3),
	
	%Find the locations corresponding to Pipe2 and Pipe3
	{ok, [PipeTest_L2]}= resource_instance:list_locations(PipeTest_Pipe2),
	{ok, [PipeTest_L3]}= resource_instance:list_locations(PipeTest_Pipe3),
	
	%Either PipeTest_C2a or PipeTest_C2b corresponds to PipeTest_C21
	%Instead of using comparation functions, the contain methods will sort this out
	
	PipeTest_Pipes = [Pipe1,PipeTest_Pipe2,PipeTest_Pipe3],
	PipeTest_Connectors = [PipeTest_C11,PipeTest_C12,PipeTest_C21,PipeTest_C31,
							PipeTest_C2a,PipeTest_C2b,PipeTest_C3a,PipeTest_C3b],
	
	Actual_Pipes = [Pipe1, Pipe2, Pipe3],
	Actual_Connectors = [C11, C12, C21, C22, C31, C32],
	
	PipeTest_Locations = [PipeTest_L1, PipeTest_L2, PipeTest_L3],
	Actual_Locations = [L1, L2, L3],
	
	{PresentList_Pipes,AbsentList_Pipes}=contains(PipeTest_Pipes,Actual_Pipes,[],[]),
	{PresentList_Cs,AbsentList_Cs}=contains(PipeTest_Connectors,Actual_Connectors,[],[]),
	{PresentList_L,AbsentList_L}=contains(PipeTest_Locations,Actual_Locations,[],[]),
	%%Assert that:
	% 1) All pipes found during traversal correspond to those given by the start of testModule
	% 2) No pipes found during traversal don't correspond to those given by the start of testModule
	% 3) All connectors found during traversal correspond to those given by the start of testModule	
	% 4) No connectors found during traversal don't correspond to those given by the start of testModule
	% 5) All locations found during traversal correspond to those given by the start of testModule	
	% 6) No locations found during traversal don't correspond to those given by the start of testModule
	[?_assertEqual(length(PipeTest_Pipes),length(PresentList_Pipes)),
	 ?_assertEqual([],AbsentList_Pipes),
	 ?_assertEqual(length(PipeTest_Connectors),length(PresentList_Cs)),
	 ?_assertEqual([],AbsentList_Cs),
	 ?_assertEqual(length(PipeTest_Locations),length(PresentList_L)),
	 ?_assertEqual([],AbsentList_L)].

test_pipe_type({PipeTypePID,Pipes,_Connectors,_Locations}) ->
	[Pipe1, Pipe2, Pipe3|_RestPipes] = Pipes,
	
	{ok,ResType1}  = resource_instance:get_type(Pipe1),
	{ok,ResType2}  = resource_instance:get_type(Pipe2),
	{ok,ResType3}  = resource_instance:get_type(Pipe3),
	
	[?_assertEqual(PipeTypePID,ResType1),
	?_assertEqual(PipeTypePID,ResType2),
	?_assertEqual(PipeTypePID,ResType3)].
	

test_fluidum_basics({_,_,_,Locations,FluidumType,Fluid})->
	[Location1, Location2, Location3] = Locations,
	
	%%Test if the processes exist
	Test1 = ?_assert(erlang:is_process_alive(FluidumType)),
	Test2 = ?_assert(erlang:is_process_alive(Fluid)),

	%Test if the fluidum was correctly added to the pipe-locations
	Test3 = ?_assertEqual(location:get_Visitor(Location1),Fluid),
	Test4 = ?_assertEqual(location:get_Visitor(Location2),Fluid),
	Test5 = ?_assertEqual(location:get_Visitor(Location3),Fluid), 
	[Test1,Test2,Test3,Test4,Test5].

test_fluidum_operations({_,Pipes,Connectors,_,FluidumType,Fluid})->
	[Connector|_] = Connectors,
	%1) Test get_type
	{ok,Type} = msg:get(Fluid,get_type),
	Test1 = ?_assertEqual(Type,FluidumType),

	%2) Test get_locations
	{ok,L_List} = msg:get(Fluid,get_locations),
	Test2  = ?_assertEqual(L_List,[]),

	%3) Test resource circuit
	{ok,Circuit} = msg:get(Fluid,get_resource_circuit),
	CheckList = testFunctions:check_circuit_map(Pipes,Circuit),
	ListIsTrue  = testFunctions:list_is_true(CheckList),
	Test3 = ?_assert(ListIsTrue),

	%4) Test discover_circuit
	{ok,{RootC,C_Circuit}} = fluidumTyp:discover_circuit(Connector),
	CheckList2 = testFunctions:check_circuit_map(Connectors,C_Circuit),
	ListIsTrue2  = testFunctions:list_is_true(CheckList2),
	Test4 = ?_assert(ListIsTrue2),
	Test5 = ?_assertEqual(RootC,Connector),

	[Test1,Test2,Test3,Test4,Test5].

test_pump_basics({_PipeTypePID,_Pipes,_Connectors,_Locations,_FluidumType,_Fluid,PumpInst,PumpTypPID})->
	[?_assert(erlang:is_process_alive(PumpTypPID)),
	 ?_assert(erlang:is_process_alive(PumpInst))].

test_pump_operation({_PipeTypePID,_Pipes,_Connectors,_Locations,_FluidumType,_Fluid,PumpInst,_PumpTypPID})->	
	%At the start, the pump should be off:
	{ok,OnOff1} = pumpInst:is_on(PumpInst),
	Test1 = ?_assertEqual(OnOff1,off),

	%Then, the pump is switched on
	pumpInst:switch_on(PumpInst),

	%Test if the pump now actually is on
	{ok,OnOff3} = pumpInst:is_on(PumpInst),
	Test2 = ?_assertEqual(OnOff3,on),

	%Check the flow influence:
	{ok, InfluenceFunction1} = pumpInst:flow_influence(PumpInst),
	Flow = 10,
	Test3 = ?_assertEqual(InfluenceFunction1(Flow),(250 - 5 * Flow - 2 * Flow * Flow)),

	%Switch the pump off again
	pumpInst:switch_off(PumpInst),

	%Test if the pump is actually off again
	{ok,OnOff5} = pumpInst:is_on(PumpInst),
	Test4 = ?_assertEqual(OnOff5,off),

	%Finally check the flow influence one last time
	{ok, InfluenceFunction2} = pumpInst:flow_influence(PumpInst),
	Flow2 = 10,
	Test5 = ?_assertEqual(InfluenceFunction2(Flow2),(250 - 5 * Flow2 - 2 * Flow2 * Flow2)),

	[Test1,Test2,Test3,Test4,Test5].

test_flowmeter_basics({_,_,_,_,_,_,Tasks})->	
	[_,_,FlowMeterInst,FlowMeterTyp] = Tasks,
	[?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 ?_assert(erlang:is_process_alive(FlowMeterInst))].

test_flowmeter_operation({_,Pipes,_,_,_,_,Tasks})->
	[Pipe1,Pipe2,Pipe3] = Pipes,
	[_,_,FlowMeterInst,FlowMeterTyp] = Tasks,
	% ?debugFmt("Testing flowmeter operation~n",[]),
	% ?debugFmt("Pipes=~p~n",[Pipes]),
	% ?debugFmt("Connectors=~p~n",[Connectors]),
	% ?debugFmt("Locations=~p~n",[Locations]),
	% ?debugFmt("FluidumTyp=~p~n",[FluidumType]),
	% ?debugFmt("Fluid=~p~n",[Fluid]),
	% ?debugFmt("PumpInst=~p~n",[PumpInst]),
	% ?debugFmt("PumpTyp=~p~n",[PumpTypPID]),
	% ?debugFmt("FlowMInst=~p~n",[FlowMeterInst]),
	% ?debugFmt("FlowMTyp=~p~n",[FlowMeterTyp]),
	
	%Test request for the Type:
	{ok,Typ} = msg:get(FlowMeterInst,get_type),
	Test0 = ?_assertEqual(Typ,FlowMeterTyp),
	%First test the real measurement value of the flow
	%?debugFmt("In between test ~p~n",[flowMeterInst:measure_flow(FlowMeterInst)]),
	{ok,{ok,RealMeasurement}} = flowMeterInst:measure_flow(FlowMeterInst),
	Test1 = ?_assertEqual(RealMeasurement,real_flow),

	%Then test the estimation of the flow
	%?debugFmt("Now we will estimate the flow~n",[]),
	{ok,Flow} = flowMeterInst:estimate_flow(FlowMeterInst),
	{ok,Fn1} = apply(resource_instance, get_flow_influence, [Pipe1]),
	{ok,Fn2} = apply(resource_instance, get_flow_influence, [Pipe2]),
	{ok,Fn3} = apply(resource_instance, get_flow_influence, [Pipe3]),
	FluidumInfluenceFunctions = [Fn1, Fn2, Fn3],
	RequiredFlow = compute({0,10},FluidumInfluenceFunctions),
	Test2 = ?_assertEqual(Flow,RequiredFlow),
	[Test0,Test1,Test2].

test_heatex_basics({_,_,_,_,_,_,Tasks})->	
	[_,_,_,_,HeatExTyp,HeatEx] = Tasks,
	[?_assert(erlang:is_process_alive(HeatExTyp)),
	 ?_assert(erlang:is_process_alive(HeatEx))].

test_heatex_operation({_,_,_,_,_,_,Tasks})->	
	[_,_,_,_,_,HeatEx] = Tasks,
	%Test temp_influence:
	{ok, {ok,Influence}} = heatExchangerInst:temp_influence(HeatEx),
	Flow = 10,
	Difference = 0.9,
	Temp = 32,
	{ok,GivenInfluence} = Influence(Flow,Temp),
	[?_assertEqual(Temp + (Difference/Flow),GivenInfluence)].

test_fully_integrated_system({PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}) ->
	
	%Sanity checks: Test if all given processes are alive
	[Pipe1, Pipe2, Pipe3] = Pipes,
	TestList1 = [?_assert(erlang:is_process_alive(PipeTypePID)),?_assert(erlang:is_process_alive(Pipe1)),
				 ?_assert(erlang:is_process_alive(Pipe2)),?_assert(erlang:is_process_alive(Pipe3))],

	[[P1C1,P1C2],[P2C1,P2C2],[P3C1,P3C2]] = Connectors,
	TestList2 = [?_assert(erlang:is_process_alive(P1C1)),?_assert(erlang:is_process_alive(P1C2)),
				 ?_assert(erlang:is_process_alive(P2C1)),?_assert(erlang:is_process_alive(P2C2)),
				 ?_assert(erlang:is_process_alive(P3C1)),?_assert(erlang:is_process_alive(P3C2))],

	[L1, L2, L3] = Locations,
	TestList3 = [?_assert(erlang:is_process_alive(L1)),?_assert(erlang:is_process_alive(L2)),
				 ?_assert(erlang:is_process_alive(L3))],

	TestList4 = [?_assert(erlang:is_process_alive(FluidumType)),?_assert(erlang:is_process_alive(Fluid))],

	[PumpInst,PumpTypPID,FlowMeterInst,FlowMeterTyp,HeatExTyp,HeatEx] = Tasks,
	TestList5 = [?_assert(erlang:is_process_alive(PumpInst)),?_assert(erlang:is_process_alive(PumpTypPID)),
				 ?_assert(erlang:is_process_alive(FlowMeterInst)),?_assert(erlang:is_process_alive(FlowMeterTyp)),
				 ?_assert(erlang:is_process_alive(HeatExTyp)),?_assert(erlang:is_process_alive(HeatEx))],
	
	%Now test the flow functionality of the system:

	%First, test with the pump still turned off

	%Next, turn the pump on
	pumpInst:switch_on(PumpInst),
	%Initialize the process monitoring the flow
	{ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,_} = systemFlow:create([PumpInst],FlowMeterInst,GetSystemFlowPid),
	%Test if flow estimations are correct, based on the calculations in the .xlsx file
	{ok,{N1,Flow1}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	Test1 = ?_assertEqual(Flow1,testFunctions:flowForBasicSituation(N1)),
	timer:sleep(5),
	{ok,{N2,Flow2}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	Test2 = ?_assertEqual(Flow2,testFunctions:flowForBasicSituation(N2)),
	timer:sleep(5),
	{ok,{N3,Flow3}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	Test3 = ?_assertEqual(Flow3,testFunctions:flowForBasicSituation(N3)),
	timer:sleep(5),
	{ok,{N4,Flow4}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
	Test4 = ?_assertEqual(Flow4,testFunctions:flowForBasicSituation(N4)),
	%
	%?debugFmt("Simulation testvalue ~p~n",[simulation:simulateFlow(FlowMeterInst,PumpInst)]),
	TestList1++TestList2++TestList3++TestList4++TestList5++[Test1,Test2,Test3,Test4].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%For each element in List1, check if it also is present in List2
%All elements present in bother List1 and List2 are stored in PresentList
%All other elements are stored in AbsentList

contains([A|Rest], List2, PresentList, AbsentList) ->
	Voorwaarde = contains_el(A,List2),
	if Voorwaarde==true ->
		contains(Rest, List2,PresentList++[A], AbsentList);
	true ->
		contains(Rest, List2,PresentList, AbsentList ++ [A])
	end;

contains([], _List2, PresentList, AbsentList) ->
	{PresentList,AbsentList}.
	
%Check if the given list contains the given element
contains_el(El, [El|_Rest]) ->
	true;

contains_el(El, [_A|Rest]) ->
	contains_el(El,Rest);
		
contains_el(_El, []) ->
	false.

%Function to simulate the "estimate flow" calculations performed:
compute({Low, High}, _InflFnCircuit) when (High - Low) < 1 -> 
	%Todo convergentiewaarde instelbaar maken. 
	(Low + High) / 2 ;
	
compute({Low, High}, InflFnCircuit) ->

	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	%?debugFmt("H1 is ~p~n",[H]),
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	%?debugFmt("H2 is ~p~n",[H]),
	Mid = (H + L) / 2, M = eval(Mid, InflFnCircuit, 0),
	if 	M > 0 -> 
			compute({Low, Mid}, InflFnCircuit);
        true -> % works as an 'else' branch
            compute({Mid, High}, InflFnCircuit)
    end.

	
eval(Flow, [Fn | RemFn] , Acc) ->
	eval(Flow, RemFn, Acc + Fn(Flow));

eval(_Flow, [], Acc) -> Acc. 