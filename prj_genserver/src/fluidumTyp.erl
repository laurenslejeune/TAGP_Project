-module(fluidumTyp).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([create/0, init/1, discover_circuit/1]).
-export([get_resource_circuit/2]).
-export([handle_call/3, handle_cast/2]).

create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%spawn(?MODULE, init, []).

init([]) -> 
	survivor:entry(fluidTyp_created), 
	{ok,[]}.
	%loop().

get_resource_circuit(TypePid, State) ->
	?debugFmt("En in de fluidumTyp?~n",[]),
	msg:get(TypePid, resource_circuit, State). 

handle_call({initial_state,[ResInst_Pid, [Root_ConnectorPid, TypeOptions]],_Ref},_From,State)->
	{ok, C} = discover_circuit(Root_ConnectorPid),
	{reply,#{resInst => ResInst_Pid, circuit => C, typeOptions => TypeOptions},State};

handle_call({connections_list, _State,_Ref},_From,State)->
	{reply,[],State};

handle_call({locations_list, _State,_Ref},_From,State)->
	{reply,[],State};

handle_call({resource_circuit, State,_Ref},_From,State)->
	#{circuit := C} = State,
	{_RootC, CircuitMap} = C,
	{reply,extract(CircuitMap),State}.

handle_cast(_,State)->
	{noreply,State}.

extract(C) -> extract(maps:next(maps:iterator(C)), #{}).

extract({C, _ , Iter }, ResLoop) ->
		{ok, ResPid} = connector:get_ResInst(C),
		extract(maps:next(Iter), ResLoop#{ResPid => processed});

extract( none , ResLoop) -> ResLoop. 

discover_circuit(Root_Pid) -> 
	{ok,  Circuit} = discover_circuit([Root_Pid], #{  }),
	{ok, {Root_Pid, Circuit}}.

discover_circuit([ disconnected | Todo_List], Circuit) -> 
	discover_circuit(Todo_List, Circuit);

discover_circuit([C | Todo_List], Circuit) -> 
	{ok, Updated_Todo_list, Updated_Circuit} = 
		process_connection(C, maps:find(C, Circuit ), Todo_List, Circuit),
	discover_circuit(Updated_Todo_list, Updated_Circuit);

discover_circuit([], Circuit) ->
	{ ok, Circuit }.

process_connection(C, error, Todo_List, Circuit) -> 
	Updated_Circuit = Circuit#{ C => processed },
    {ok, CC} = connector:get_connected(C),
	Updated_Todo_list = [ CC | Todo_List],
	{ok, ResPid} = connector:get_ResInst(C),
	{ok, C_list} = resource_instance:list_connectors(ResPid),
	{ok, C_list ++  Updated_Todo_list, Updated_Circuit};

process_connection( _, _ , Todo_List, Circuit) -> 
	{ok, Todo_List, Circuit}.





