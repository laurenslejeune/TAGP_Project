-module(prop_base).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, term(),
        begin
            boolean(Type)
        end).

prop_discover_circuit() ->
	?FORALL(Int,integer(2,100),testFunctions:list_is_true(test_discover_circuit_N(Int))).

prop_test_flow_influence() ->
	%Generate a network with a pump
	%Request the influence for a given flow on that pump
	%Calculate the influence, and compare the 2 values
	%Repeat this many times
	survivor:start(),
	?FORALL(Flow,integer(0,1000),test_flow_influence(Flow)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.


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
	{ok,{_,_,_,_,_,_,PumpInst,_}} = buildSystem:start_3pipes_water_pump_no_survivor(),
	pumpInst:switch_on(PumpInst),
	{ok, InfluenceFunction1} = pumpInst:flow_influence(PumpInst),
	Influence1 = InfluenceFunction1(Flow),
	Influence2 = 250 - 5 * Flow - 2 * Flow * Flow,
	%buildSystem:stop(),
	Influence1 == Influence2.


	
%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mytype() -> term().
