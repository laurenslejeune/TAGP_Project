-module(heatExchangerTyp).
-behaviour(gen_server).
-export([create/0, createRegister/0, init/1]).
-export([handle_call/3, handle_cast/2,terminate/2]).


create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%{ok, spawn(?MODULE, init, [])}.

createRegister()->
	Exists = whereis(?MODULE),
	if(Exists==undefined)->
		gen_server:start_link({local,?MODULE},?MODULE,[],[]);
	true->
		unregister(?MODULE),
		gen_server:start_link({local,?MODULE},?MODULE,[],[])
	end.

init([]) -> 
	survivor:entry(heatExchangerTyp_created),
	{ok,[]}.
	%loop().

handle_cast(_,_)->
	{noreply,[]}.

handle_call({initial_state, [ResInst_Pid, PipeInst_Pid],_Ref},_From,[])->
	{reply,#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid},[]}.

terminate(Reason,_)->
	% RegisteredAtom = whereis(self()),
	% if(RegisteredAtom==undefined)->
	% 	{ok,Reason};
	% true->
	% 	unregister(self()),
	% 	{ok,Reason}
	% end.
	{ok,Reason}.