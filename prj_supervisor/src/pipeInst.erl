-module(pipeInst).
-behaviour(gen_server).
-export([create/2, create/3, init/1, get_flow_influence/1, crash/1]).
-export([handle_call/3, handle_cast/2,terminate/2]).


create(Host, ResTyp_Pid) -> 
	gen_server:start_link(?MODULE, [Host, ResTyp_Pid],[]).

create(Host,ResTyp_Pid,Number)->
	% {ok,Pid} = gen_server:start_link(?MODULE, [Host, ResTyp_Pid],[]),
	% InstanceAtom = testFunctions:numberToAtom(pipeInst,Number),
	% register(InstanceAtom,Pid),
	% {ok,Pid}.
	Atom = testFunctions:numberToAtom(pipeInst,Number),
	Exists = whereis(Atom),
	if(Exists==undefined)->
		gen_server:start_link({local,Atom},?MODULE, [Host, ResTyp_Pid],[]);
	true->
		unregister(Atom),
		gen_server:start_link({local,Atom},?MODULE, [Host, ResTyp_Pid],[])
	end.
	

init([Host, ResTyp_Pid]) -> 
%	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), []]),	
	%io:format("Asking ~p for the initial state for instance ~p, with TypeOptions ~p~n",[ResTyp_Pid,self(),[]]),
	{ok, State} = resource_type:get_initial_state(ResTyp_Pid, self(), []),
	survivor:entry({ pipeInst_created, State }),
	{ok,{Host, State, ResTyp_Pid}}.

get_flow_influence(PipeInst_Pid) -> 
	msg:get(PipeInst_Pid, get_flow_influence).

crash(PipeInst_Pid) ->
	gen_server:cast(PipeInst_Pid,crash).

handle_call({get_connectors,_Ref},_From,{Host, State, ResTyp_Pid})->
	{ok,C_List} = resource_type:get_connections_list(ResTyp_Pid, State), 
	{reply,C_List,{Host, State, ResTyp_Pid}};
	
handle_call({get_locations,_Ref},_From,{Host, State, ResTyp_Pid})->
	{ok,List} = resource_type:get_locations_list(ResTyp_Pid, State),
	{reply,List,{Host, State, ResTyp_Pid}};

handle_call({get_type,_Ref},_From,{Host, State, ResTyp_Pid})->
	{reply,ResTyp_Pid,{Host, State, ResTyp_Pid}};

handle_call({get_flow_influence,_Ref},_From,{Host, State, ResTyp_Pid})->
	{ok, InfluenceFn} = msg:get(ResTyp_Pid, flow_influence, State),
	{reply,InfluenceFn,{Host, State, ResTyp_Pid}};

handle_call({get_ops,_Ref},_From,{Host, State, ResTyp_Pid})->
	{reply,[],{Host, State, ResTyp_Pid}}.

handle_cast(crash,State) ->
	1/0,
	{noreply,State};

handle_cast(_,State) ->
	{noreply,State}.

terminate(Reason,State)->
	{ok,[C1,C2]} = msg:get(self(),get_connectors),
	connector:discard(C1),
	connector:discard(C2),
	{ok,[Location]} = msg:get(self(),get_locations),
	location:dispose(Location),
	%io:format("Terminating pipe instance ~p~n",[self()]),
	%RegisteredAtom = whereis(self()),
	%if(RegisteredAtom==undefined)->
	%	{ok,Reason};
	%true->
	%	unregister(self()),
	%	{ok,Reason}
	%end.
	{ok,Reason}.