-module(testFunctions).
-export([list_is_true/1,check_circuit_map/2]).
-export([init/0]).
-include_lib("eunit/include/eunit.hrl").

init() ->
	ok.

list_is_true([true|Acc]) ->
	list_is_true(Acc);
	
list_is_true([false|_Acc]) ->
	false;
	
list_is_true([]) ->
	true.

check_circuit_map(Connectors,CircuitMap) ->
	check_circuit_map(Connectors,CircuitMap,[]).
	
check_circuit_map([C1|OtherC],CircuitMap, Checks) ->
	{ok,StoredValue} = maps:find(C1,CircuitMap),
	%?debugFmt("Checking map with ~p <-> ~p", [C1,StoredValue]),
	check_circuit_map(OtherC,CircuitMap,Checks++[StoredValue==processed]);

check_circuit_map([],_CircuitMap, Checks) ->
	Checks.