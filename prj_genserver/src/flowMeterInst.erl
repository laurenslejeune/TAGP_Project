-module(flowMeterInst).
-behaviour(gen_server).
-export([create/4, init/1, estimate_flow/1, measure_flow/1]).
-export([handle_call/3, handle_cast/2]).
% -export([commission/1, activate/1]).
% -export([deactivate/1, decommission/1]).

% FlowMeter is a pipe and possibly a more complex resource;  
% this resource instance is passed to the create function.
% RealWorldCmdFn is a function to read out the real-world flowMeter. 

create(Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn) ->
	gen_server:start_link(?MODULE,[Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn],[]).
	%{ok, spawn(?MODULE, init, [Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn])}.

init([Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn]) -> 
	{ok, State} = apply(resource_type, get_initial_state, [FlowMeterTyp_Pid, self(),     [ResInst_Pid, RealWorldCmdFn]]),
									%  get_initial_state  (ResTyp_Pid,       ThisResInst, TypeOptions) 
	survivor:entry({ flowMeterInst_created, State }),
	{ok,{Host, State, FlowMeterTyp_Pid, ResInst_Pid}}.
	%loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid).

handle_cast(_,State)->
	{noreply,State}.

estimate_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, estimate_flow). 

measure_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, measure_flow).  

handle_call({measure_flow,_Ref},_From,{Host, State, FlowMeterTyp_Pid, ResInst_Pid})->
	{ok, Answer} = msg:get(FlowMeterTyp_Pid, measure_flow, State), 
	{reply,Answer,{Host, State, FlowMeterTyp_Pid, ResInst_Pid}};

handle_call({estimate_flow,_Ref},_From,{Host, State, FlowMeterTyp_Pid, ResInst_Pid})->
	{ok, InfluenceFn} = msg:get(FlowMeterTyp_Pid, estimate_flow, State),
	{reply,InfluenceFn,{Host, State, FlowMeterTyp_Pid, ResInst_Pid}};

handle_call({get_type,_Ref},_From,{Host, State, FlowMeterTyp_Pid, ResInst_Pid})->
	{reply,FlowMeterTyp_Pid,{Host, State, FlowMeterTyp_Pid, ResInst_Pid}};

handle_call({OtherMessage,_Ref},_From,{Host, State, FlowMeterTyp_Pid, ResInst_Pid})->
	{ok,Answer} = msg:get(ResInst_Pid,OtherMessage),
	{reply,Answer,{Host, State, FlowMeterTyp_Pid, ResInst_Pid}}.