-module(pumpTyp).
-behaviour(gen_server).
-export([create/0, createRegister/0, init/1]).
-export([handle_call/3, handle_cast/2,terminate/2]).
-include_lib("eunit/include/eunit.hrl").
% -export([dispose/2, enable/2, new_version/2]).
% -export([get_initial_state/3, get_connections_list/2]). % use resource_type
% -export([update/3, execute/7, refresh/4, cancel/4, update/7, available_ops/2]). 

create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%{ok, spawn(?MODULE, init, [])}.

createRegister() ->
	Exists = whereis(?MODULE),
	if(Exists==undefined)->
		gen_server:start_link({local,?MODULE},?MODULE,[],[]);
	true->
		unregister(?MODULE),
		gen_server:start_link({local,?MODULE},?MODULE,[],[])
	end.

init([]) -> 
	survivor:entry(pumpTyp_created),
	{ok,[]}.
	%loop().

handle_call({initial_state, [ResInst_Pid, [PipeInst_Pid, RealWorldCmdFn]],_Ref},_From,_)->
	Reply = #{resInst => ResInst_Pid, pipeInst => PipeInst_Pid, 
					  rw_cmd => RealWorldCmdFn, on_or_off => off},
	{reply,Reply,[]};

handle_call({flow_influence, State,_Ref},_From,_)->
	#{on_or_off := OnOrOff} = State,
	FlowInfluenceFn = fun(Flow) -> flow(Flow, OnOrOff) end, % placeholder only. 
	{reply,FlowInfluenceFn,[]};

handle_call({switchOff, State, _Ref},_From,_)->
	#{rw_cmd := ExecFn} = State, ExecFn(off),
	{reply,State#{on_or_off := off},[]};

handle_call({switchOn, State, _Ref},_From,_)->
	#{rw_cmd := ExecFn} = State, ExecFn(on),
	{reply,State#{on_or_off := on},[]};

handle_call({isOn, State, _Ref},_From,_)->
	#{on_or_off := OnOrOff} = State, 
	{reply,OnOrOff,[]}.

handle_cast(_,State)->
	{noreply,State}.

terminate(Reason,_)->
	RegisteredAtom = whereis(self()),
	if(RegisteredAtom==undefined)->
		{ok,Reason};
	true->
		unregister(self()),
		{ok,Reason}
	end.

flow(Flow, on)  -> (250 - 5 * Flow - 2 * Flow * Flow);
flow(_Flow, off) -> 0. 