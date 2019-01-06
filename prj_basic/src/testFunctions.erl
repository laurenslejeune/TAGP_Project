-module(testFunctions).
-export([list_is_true/1,check_circuit_map/2]).
-export([init/0]).
-export([is_odd/1, is_even/1]).
-export([flowForBasicSituation/1]).
-export([round/2]).
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

is_odd(Number) ->
	(Number rem 2) /= 0.

is_even(Number) ->
	(Number rem 2) == 0.

flowForBasicSituation(0)->
	0;

flowForBasicSituation(Time)->
	Flow = 0,
	Loss = getLoss(Flow),
	ActualFlow = Flow + Loss,
	PumpForce = 250 -5*ActualFlow - 2*ActualFlow*ActualFlow,
	NextFlow = 10 - ((-5/4) + math:sqrt(2025-8*PumpForce)/4),
	flowForBasicSituation(Time-1,NextFlow).

flowForBasicSituation(0,Flow)->
	Flow;

flowForBasicSituation(Time,Flow)->
	Loss = getLoss(Flow),
	ActualFlow = Flow + Loss,
	PumpForce = 250 -5*ActualFlow - 2*ActualFlow*ActualFlow,
	NextFlow = 10 - ((-5/4) + math:sqrt(2025-8*PumpForce)/4),
	flowForBasicSituation(Time-1,NextFlow).

getLoss(Flow) when Flow < 1 ->
	Flow/2;

getLoss(Flow)->
	-0.03*Flow/4.


round(Number, Precision) ->
	%Source:http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Erlang
    P = math:pow(10, Precision),
    round(Number * P) / P.