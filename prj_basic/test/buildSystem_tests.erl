-module(buildSystem_tests).
-include_lib("eunit/include/eunit.hrl").

-export([contains_el/2]).
-export([contains/4]).

%%BELANGRIJK: ZORG ERVOOR DAT DE SURVIVOR AF TE SLUITEN IS!%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
	{"The system can be started and stopped, and all processes exist",
	{foreach,
	fun start_3pipes/0,
	fun stop/1,
	[fun has_started/1]}}.

basic_connections_test_test_() ->
	{"Test if the given pipes are really connected to their connectors",
	{setup,
	fun start_3pipes/0,
	fun stop/1,
	fun basic_connections/1}}.	

traverse_test_() ->
	{"Test if the network can be traversed, from one pipe to another",
	{setup,
	fun start_3pipes/0,
	fun stop/1,
	fun traverse_pipe/1}}.
	
test_all_connectors_test_() ->
	{"Test if the network can be traversed, storing information about Pipe, Connector and Location",
	{setup,
	fun start_3pipes/0,
	fun stop/1,
	fun test_pipes_connectors_locations/1}}.

pipe_type_test_() ->
	{"Check if the pipe resource instances belong to the pipeTyp type",
	{setup,
	fun start_3pipes/0,
	fun stop/1,
	fun test_pipe_type/1}}.	
	
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%Opmerking: Als de testmodule halverwege de tests crasht (en dus niet op de juiste manier afsluit),
%%wordt de start_link nooit beÃ«indigd en kan je de start() niet opnieuw gebruiken omdat er een pipeTyp
%%proces running is

start_3pipes() ->
	{ok, {PipeTypePID,Pipes,Connectors,Locations}} = buildSystem:start_3pipes(),
	{PipeTypePID,Pipes,Connectors,Locations}.

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