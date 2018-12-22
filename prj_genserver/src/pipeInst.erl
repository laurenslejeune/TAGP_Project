-module(pipeInst).
-behaviour(gen_server).
-export([create/2, init/1, get_flow_influence/1]).
-export([handle_call/3, handle_cast/2]).


create(Host, ResTyp_Pid) -> 
	gen_server:start_link(?MODULE, [Host, ResTyp_Pid],[]).

init([Host, ResTyp_Pid]) -> 
%	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), []]),	
	%io:format("Asking ~p for the initial state for instance ~p, with TypeOptions ~p~n",[ResTyp_Pid,self(),[]]),
	{ok, State} = resource_type:get_initial_state(ResTyp_Pid, self(), []),
	survivor:entry({ pipeInst_created, State }),
	{ok,{Host, State, ResTyp_Pid}}.

get_flow_influence(PipeInst_Pid) -> 
	msg:get(PipeInst_Pid, get_flow_influence).

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

handle_cast(something,State) ->
	{noreply,State}.
