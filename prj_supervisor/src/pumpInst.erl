-module(pumpInst).
-behaviour(gen_server).
-export([create/4,create/5, init/1, switch_on/1, switch_off/1, is_on/1, flow_influence/1]).
-export([handle_call/3, handle_cast/2,terminate/2]).
-include_lib("eunit/include/eunit.hrl").
% -export([commission/1, activate/1]).
% -export([deactivate/1, decommission/1]).

% Pump is a pipe and more; this pipe instance is passed to the create function.
% RealWorldCmdFn is a function to transfer commands to the real-world pump. 

create(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn) -> 
	gen_server:start_link(?MODULE,[Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn],[]).
	%{ok, spawn(?MODULE, init, [Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn])}.

create(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn,Number) -> 
	Atom = testFunctions:numberToAtom(pumpInst,Number),
	Exists = whereis(Atom),
	if(Exists==undefined)->
		gen_server:start_link({local,Atom},?MODULE,[Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn],[]);
	true->
		unregister(Atom),
		gen_server:start_link({local,Atom},?MODULE,[Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn],[])
	end.

init([Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn]) -> 
	{ok, State} = apply(resource_type, get_initial_state, [PumpTyp_Pid, self(),     [PipeInst_Pid, RealWorldCmdFn]]),
									%  get_initial_state  (ResTyp_Pid,  ResInst_Pid, TypeOptions) 
	survivor:entry({ pumpInst_created, State }),
	{ok,{Host, State, PumpTyp_Pid, PipeInst_Pid}}.
	%loop(Host, State, PumpTyp_Pid, PipeInst_Pid).

handle_cast(switchOff,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{ok, NewState} = msg:set_ack(PumpTyp_Pid, switchOff, State),
	{noreply,{Host, NewState, PumpTyp_Pid, PipeInst_Pid}};

handle_cast(switchOn,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{ok, NewState} = msg:set_ack(PumpTyp_Pid, switchOn, State),
	{noreply,{Host, NewState, PumpTyp_Pid, PipeInst_Pid}}.

handle_call({isOn,_Ref},_From,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{ok, Answer} = msg:get(PumpTyp_Pid, isOn, State), 
	{reply,Answer,{Host, State, PumpTyp_Pid, PipeInst_Pid}};

handle_call({get_type,_Ref},_From,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{reply,PumpTyp_Pid,{Host, State, PumpTyp_Pid, PipeInst_Pid}};

handle_call({get_flow_influence,_Ref},_From,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{ok, InfluenceFn} = msg:get(PumpTyp_Pid, flow_influence, State),
	{reply,InfluenceFn,{Host, State, PumpTyp_Pid, PipeInst_Pid}};

handle_call({OtherMessage,_Ref},_From,{Host, State, PumpTyp_Pid, PipeInst_Pid})->
	{ok,Answer} = msg:get(PipeInst_Pid,OtherMessage),
	{reply,Answer,{Host, State, PumpTyp_Pid, PipeInst_Pid}}.

terminate(Reason,_)->
	RegisteredAtom = whereis(self()),
	if(RegisteredAtom==undefined)->
		{ok,Reason};
	true->
		unregister(self()),
		{ok,Reason}
	end.

switch_off(PumpInst_Pid) ->
	gen_server:cast(PumpInst_Pid,switchOff).
	%PumpInst_Pid ! switchOff. 

switch_on(PumpInst_Pid) ->
	gen_server:cast(PumpInst_Pid,switchOn).
	%PumpInst_Pid ! switchOn. 

is_on(PumpInst_Pid) -> 
	msg:get(PumpInst_Pid, isOn).

flow_influence(PumpInst_Pid) -> 
	msg:get(PumpInst_Pid, get_flow_influence).