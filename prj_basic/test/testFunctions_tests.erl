-module(testFunctions_tests).
-include_lib("eunit/include/eunit.hrl").

list_is_true_test_() ->
	{"Test if the list_is_true function works as supposed to.",
	{setup,
	fun start/0,
	fun stop/1,
	fun test_list_is_true/1}}.


check_circuit_map_test_() ->
	{"Test if the list_is_true function works as supposed to.",
	{setup,
	fun start/0,
	fun stop/1,
	fun test_check_circuit_map/1}}.

flowForBasicSituation_test_()->
	{"Test if the flow, given a standard situation, for a given point in time, is correctly calculated",
	{setup,
	fun start/0,
	fun stop/1,
	fun test_flowForBasicSituation/0}}.

flowForAnySituation_test_()->
	{"Test if the flow, given a situation with N1 pipes and N2 pumps, for a given point in time, is correctly calculated",
	{setup,
	fun start/0,
	fun stop/1,
	fun test_flowForAnySituation/0}}.

flowForAnySituationWithPumpControl_test_()->
	{"Test if the flow, given a situation with N1 pipes and N2 pumps, while turning the pump on and off,
	 for a given point in time, is correctly calculated",
	{setup,
	fun start/0,
	fun stop/1,
	fun test_flowForAnySituationWithPumpControl/0}}.

start() ->
	ok.

stop(_) ->
	ok.

test_list_is_true(ok) ->
    A = [true,true,true],
    B = [true,false,true],
    [?_assertEqual(testFunctions:list_is_true(A),true),
     ?_assertEqual(testFunctions:list_is_true(B),false)].

test_check_circuit_map(ok) ->
	%%Generate a number of test processes:
	Pid1 = spawn(testFunctions,init,[]),
	Pid2 = spawn(testFunctions,init,[]),
	Pid3 = spawn(testFunctions,init,[]),
	Pid4 = spawn(testFunctions,init,[]),
	Pid5 = spawn(testFunctions,init,[]),
	Map = #{Pid1 => processed, Pid2 => processed,
			Pid3 => processed, Pid4 => processed,
			Pid5 => processed},
	Pids = [Pid1, Pid2, Pid3, Pid4, Pid5],
	[Result1, Result2, Result3, Result4, Result5] = testFunctions:check_circuit_map(Pids,Map),
		[?_assertEqual(true,Result1),
		 ?_assertEqual(true,Result2),
		 ?_assertEqual(true,Result3),
		 ?_assertEqual(true,Result4),
		 ?_assertEqual(true,Result5)].


test_flowForBasicSituation()->
	Test0 = ?_assertEqual(0,testFunctions:flowForBasicSituation(0)),
	
	Calculated1 = testFunctions:round(testFunctions:flowForBasicSituation(10),5),
	Correct1 = testFunctions:round(1.453171752,5),
	
	Calculated2 = testFunctions:round(testFunctions:flowForBasicSituation(50),5),
	Correct2= testFunctions:round(2.370520486,5),
	
	Calculated3 = testFunctions:round(testFunctions:flowForBasicSituation(100),5),
	Correct3 = testFunctions:round(3.189435625,5),
	
	Calculated4 = testFunctions:round(testFunctions:flowForBasicSituation(200),5),
	Correct4 = testFunctions:round(4.13720856,5),
	
	[Test0,?_assertEqual(Calculated1,Correct1),?_assertEqual(Calculated2,Correct2),
	 ?_assertEqual(Calculated3,Correct3),?_assertEqual(Calculated4,Correct4)].

test_flowForAnySituation()->
	Test0 = ?_assertEqual(0,testFunctions:flowForAnySituation(0)),
	N_pipes = 50,
	N_pumps = 3,
	Calculated1 = testFunctions:round(testFunctions:flowForAnySituation(10,N_pipes,N_pumps),5),
	Correct1 = testFunctions:round(10.31694193,5),
	
	Calculated2 = testFunctions:round(testFunctions:flowForAnySituation(50,N_pipes,N_pumps),5),
	Correct2= testFunctions:round(13.9823587,5),
	
	Calculated3 = testFunctions:round(testFunctions:flowForAnySituation(100,N_pipes,N_pumps),5),
	Correct3 = testFunctions:round(13.99997777,5),
	
	Calculated4 = testFunctions:round(testFunctions:flowForAnySituation(15,N_pipes,N_pumps),5),
	Correct4 = testFunctions:round(15.88907339,5),
	
	[Test0,?_assertEqual(Calculated1,Correct1),?_assertEqual(Calculated2,Correct2),
	 ?_assertEqual(Calculated3,Correct3),?_assertEqual(Calculated4,Correct4)].

test_flowForAnySituationWithPumpControl()->
	Test0 = ?_assertEqual(0,testFunctions:flowForAnySituationWithPumpControl(0)),
	N_pipes = 50,
	N_pumps = 3,
	PumpControlList = {[{0,true},{40,false},{90,true}],0,true},
	Calculated1 = testFunctions:round(testFunctions:flowForAnySituationWithPumpControl(10,N_pipes,N_pumps,PumpControlList),5),
	Correct1 = testFunctions:round(21.1807251,5),
	
	Calculated2 = testFunctions:round(testFunctions:flowForAnySituationWithPumpControl(50,N_pipes,N_pumps,PumpControlList),5),
	Correct2= testFunctions:round(1.906714696,5),
	
	Calculated3 = testFunctions:round(testFunctions:flowForAnySituationWithPumpControl(100,N_pipes,N_pumps,PumpControlList),5),
	Correct3 = testFunctions:round(10.66633457,5),
	
	Calculated4 = testFunctions:round(testFunctions:flowForAnySituationWithPumpControl(200,N_pipes,N_pumps,PumpControlList),5),
	Correct4 = testFunctions:round(13.99999471,5),
	
	[Test0,?_assertEqual(Calculated1,Correct1),?_assertEqual(Calculated2,Correct2),
	 ?_assertEqual(Calculated3,Correct3),?_assertEqual(Calculated4,Correct4)].