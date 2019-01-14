-module(fluidumInst).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-export([create/2, create/3, init/1, get_resource_circuit/1]).
-export([handle_call/3, handle_cast/2,terminate/2]).

create(Root_ConnectorPid, ResTyp_Pid) -> 
	gen_server:start_link(?MODULE,[Root_ConnectorPid, ResTyp_Pid],[]).
	%{ok, spawn(?MODULE, init, [Root_ConnectorPid, ResTyp_Pid])}.

create(Root_ConnectorPid, ResTyp_Pid,Number) ->
	Atom = testFunctions:numberToAtom(fluidumInst,Number),
	Exists = whereis(Atom),
	if(Exists==undefined)->
		gen_server:start_link({local,Atom},?MODULE,[Root_ConnectorPid, ResTyp_Pid],[]);
	true->
		unregister(Atom),
		gen_server:start_link({local,Atom},?MODULE,[Root_ConnectorPid, ResTyp_Pid],[])
	end.

init([Root_ConnectorPid, ResTyp_Pid]) -> 
	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), [Root_ConnectorPid, plain_water]]),
	survivor:entry({ fluidInst_created, State }),
	{ok,{Root_ConnectorPid, State, ResTyp_Pid}}.
	%loop(Root_ConnectorPid, State, ResTyp_Pid).

get_resource_circuit(ResInstPid) ->
	%?debugFmt("Geraak ik in de fluidumInst?~n",[]),
	msg:get(ResInstPid, get_resource_circuit). 

handle_call({get_locations,_Ref},_From,{Root_ConnectorPid, State, ResTyp_Pid})->
	{ok, L_List} = resource_type:get_locations_list(ResTyp_Pid, State),
	{reply,L_List,{Root_ConnectorPid, State, ResTyp_Pid}};

handle_call({get_type,_Ref},_From,{Root_ConnectorPid, State, ResTyp_Pid})->
	{reply,ResTyp_Pid,{Root_ConnectorPid, State, ResTyp_Pid}};

handle_call({get_resource_circuit,_Ref},_From,{Root_ConnectorPid, State, ResTyp_Pid})->
	{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
	{reply,C,{Root_ConnectorPid, State, ResTyp_Pid}}.

handle_cast(_,State)->
	{noreply,State}.

terminate(Reason,_)->
	% RegisteredAtom = whereis(self()),
	% if(RegisteredAtom==undefined)->
	% 	{ok,Reason};
	% true->
	% 	unregister(self()),
	% 	{ok,Reason}
	% end.
	{ok,Reason}.