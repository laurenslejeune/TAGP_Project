-module(heatExchangerInst).
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2,terminate/2]).
-export([create/4, create/5, init/1, temp_influence/1]).
-include_lib("eunit/include/eunit.hrl").
% HeatExchanger is a pipe and more; this pipe instance is passed to the create function.
% HeatExchangers have a HE_link to, typically, another HeatExchanger. The link provides 
% a function that models the mutual effect on the temperature of the flows on either side. 

create(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec) -> 
	gen_server:start_link(?MODULE,[Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec],[]).
	%{ok, spawn(?MODULE, init, [Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec])}.

create(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec, Number) -> 
	Atom = testFunctions:numberToAtom(heatExchangerInst,Number),
	Exist = whereis(Atom),
	if(Exist==undefined)->
		gen_server:start_link({local,Atom},?MODULE,[Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec],[]);
	true->
		unregister(Atom),
		gen_server:start_link({local,Atom},?MODULE,[Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec],[])
	end.

init([Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec]) -> 
	{ok, State} = apply(resource_type, get_initial_state, [HeatExchangerTyp_Pid, self(), PipeInst_Pid]),
									%  get_initial_state  (ResTyp_Pid,  ResInst_Pid, TypeOptions) 
	survivor:entry({ pumpInst_created, State }),
	{ok,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec}}.
	%loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec).

handle_call({get_type,_Ref},_From,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec})->
	{reply,HeatExchangerTyp_Pid,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec}};

handle_call({get_temp_influence,_Ref},_From,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec})->
	{reply,heatExchangeLink:get_temp_influence(HE_link_spec),{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec}};

handle_call({OtherMessage,_Ref},_From,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec})->
	%Maybe this has to be a cast instead of a call
	{ok,Answer} = msg:get(PipeInst_Pid,OtherMessage),
	{reply,Answer,{Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec}}.

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

temp_influence(HeatExchangerInst_Pid) -> 
	msg:get(HeatExchangerInst_Pid, get_temp_influence).