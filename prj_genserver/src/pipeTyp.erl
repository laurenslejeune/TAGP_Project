-module(pipeTyp).
-behaviour(gen_server).
-export([create/0, init/1, get_flow_influence/2]). % More to be added later. 
-export([handle_call/3,handle_cast/2]).
create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%{ok, spawn(?MODULE, init, [])}.

init([]) -> 
	survivor:entry(pipeTyp_created),
	{ok,[]}.
	%loop().

get_flow_influence(TypePid, State) -> 
	msg:get(TypePid, flow_influence, State).

handle_call(test, _From, _State) ->
	io:format("Dit is een test~n"),
	{reply,reply,[]};
	
handle_call({initial_state, [ResInst_Pid, TypeOptions], _Ref},_From,_State) ->
	%io:format("Geraak ik tot ier ?~n"),
	{ok, Location} = location:create(ResInst_Pid, emptySpace),
	{ok, In} = connector:create(ResInst_Pid, simplePipe),
	{ok, Out} = connector:create(ResInst_Pid, simplePipe),

	Reply = #{resInst => ResInst_Pid, chambers => [Location], 
			cList => [In, Out], typeOptions => TypeOptions},
	{reply,Reply,[]};
	
handle_call({connections_list, State, _Ref},_From,_State) ->
	#{cList := C_List} = State, 
	{reply,C_List, []};

handle_call({flow_influence, _State, _Ref},_From,_State) ->
	FlowInfluenceFn = fun(Flow) -> flow(Flow) end, % placeholder only. 
	{reply,FlowInfluenceFn, []};


handle_call({locations_list, State, _Ref}, _From,_State) ->
	#{chambers := L_List} = State,
	{reply, L_List, []}.
		
handle_cast(something,_State) ->
	{noreply,[]}.	


flow(N) -> - 0.01 * N. 