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