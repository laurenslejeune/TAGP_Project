-module(testFunctions).
-export([list_is_true/1,check_circuit_map/2]).
-export([init/0]).
-export([is_odd/1, is_even/1]).
-export([flowForBasicSituation/1,flowForAnySituation/3,flowForAnySituationWithPumpControl/4]).
-export([round/2]).
-export([generateDifList/2]).
-export([numberToAtom/2]).
-include_lib("eunit/include/eunit.hrl").

init() ->
	ok.

numberToAtom(pipeInst,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("pipeInst_",NumberString),
	list_to_atom(AtomString);

numberToAtom(fluidumInst,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("fluidumInst_",NumberString),
	list_to_atom(AtomString);

numberToAtom(pumpInst,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("pumpInst_",NumberString),
	list_to_atom(AtomString);

numberToAtom(flowMeterInst,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("flowMeterInst_",NumberString),
	list_to_atom(AtomString);

numberToAtom(heatExchangerInst,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("heatExchangerInst_",NumberString),
	list_to_atom(AtomString);

numberToAtom(systemFlow,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("systemFlow_",NumberString),
	list_to_atom(AtomString);

numberToAtom(systemTemp,Number)->
	NumberString = integer_to_list(Number),
	AtomString = string:concat("systemTemp_",NumberString),
	list_to_atom(AtomString).

-spec list_is_true(list(boolean())) -> boolean().
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

-spec is_odd(number())->boolean().
is_odd(Number) ->
	(Number rem 2) /= 0.

-spec is_even(number())->boolean().
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

flowForAnySituation(0,_,_)->
	0;

flowForAnySituation(Time,N_pipes,N_pumps)->
	Flow = 0,
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituation(Time-1,N_pipes,N_pumps,NextFlow).

flowForAnySituation(0,_,_,Flow)->
	Flow;

flowForAnySituation(Time,N_pipes,N_pumps,Flow)->
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituation(Time-1,N_pipes,N_pumps,NextFlow).


flowForAnySituationWithPumpControl(0,_,_,_)->
	0;

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,{[{Counter,true}|Rest],Counter,_})->
	Flow = 0,
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{Rest,Counter+1,true});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,{[{Counter,false}|Rest],Counter,_})->
	Flow = 0,
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	NextFlow = ActualFlow,
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{Rest,Counter+1,false});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,{PumpControlList,Counter,true})->
	Flow = 0,
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{PumpControlList,Counter+1,true});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,{PumpControlList,Counter,false})->
	Flow = 0,
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	NextFlow = ActualFlow,
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{PumpControlList,Counter+1,false}).

flowForAnySituationWithPumpControl(0,_,_,Flow,_)->
	Flow;

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,Flow,{[{Counter,true}|Rest],Counter,_})->
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{Rest,Counter+1,true});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,Flow,{[{Counter,false}|Rest],Counter,_})->
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	NextFlow = ActualFlow,
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{Rest,Counter+1,false});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,Flow,{PumpControlList,Counter,true})->
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	PumpForce = (250 -5*ActualFlow - 2*ActualFlow*ActualFlow) * N_pumps,
	NextFlow = N_pumps*10 - ((-5/4)+math:sqrt(2025-(8*PumpForce/N_pumps))/4),
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{PumpControlList,Counter+1,true});

flowForAnySituationWithPumpControl(Time,N_pipes,N_pumps,Flow,{PumpControlList,Counter,false})->
	Loss = getLoss(Flow,N_pipes),
	ActualFlow = Flow + Loss,
	NextFlow = ActualFlow,
	flowForAnySituationWithPumpControl(Time-1,N_pipes,N_pumps,NextFlow,{PumpControlList,Counter+1,false}).

getLoss(Flow,_) when Flow < 1 ->
	Flow/2;

getLoss(Flow,N_pipes) ->
	-0.01*N_pipes*Flow/4.

getLoss(Flow) when Flow < 1 ->
	Flow/2;

getLoss(Flow)->
	-0.03*Flow/4.


round(Number, Precision) ->
	%Source:http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Erlang
    P = math:pow(10, Precision),
    round(Number * P) / P.

-spec generateDifList(pos_integer(),list()) -> list().
generateDifList(1,DifList)->
    RandomDif = rand:uniform(3)-2,
    DifList++[RandomDif];

generateDifList(N,DifList)->
    RandomDif = rand:uniform(3)-2, %Generates random number in range [-1;1]
    generateDifList(N-1,DifList++[RandomDif]).