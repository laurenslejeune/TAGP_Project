-module(heatExchangerTyp).
-behaviour(gen_server).
-export([create/0, init/1]).
-export([handle_call/3, handle_cast/2]).


create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%{ok, spawn(?MODULE, init, [])}.

init([]) -> 
	survivor:entry(heatExchangerTyp_created),
	{ok,[]}.
	%loop().

handle_cast(_,_)->
	{noreply,[]}.

handle_call({initial_state, [ResInst_Pid, PipeInst_Pid],_Ref},_From,[])->
	{reply,#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid},[]}.