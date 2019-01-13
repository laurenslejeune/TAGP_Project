-module(connector).
-behaviour(gen_server).

-export([create/2, connect/2, disconnect/1, discard/1]).
-export([get_connected/1, get_ResInst/1, get_type/1]).
-export([handle_call/3, handle_cast/2]).
-export([init/1, test/0]). % for internal use only. 

create(ResInst_Pid, ConnectTyp_Pid) -> 
	gen_server:start_link(?MODULE,[ResInst_Pid,ConnectTyp_Pid],[]).
	%spawn(?MODULE, init, [ResInst_Pid, ConnectTyp_Pid]).


init([ResInst_Pid, ConnectTyp_Pid]) -> 
	survivor:entry(connector_created), 
	{ok,{ResInst_Pid,disconnected,ConnectTyp_Pid}}.
	%%loop(ResInst_Pid, disconnected, ConnectTyp_Pid).

connect(Connector_Pid, C_Pid) ->
	gen_server:cast(Connector_Pid,{connect,C_Pid}).
	%Connector_Pid ! {connect, C_Pid}.

disconnect(Connector_Pid) ->
	gen_server:cast(Connector_Pid,disconnect).
	%Connector_Pid ! disconnect.

get_connected(Connector_Pid) ->
	msg:get(Connector_Pid, get_connected).

get_ResInst(Connector_Pid) ->
	msg:get(Connector_Pid, get_ResInst).
	
get_type(Connector_Pid) ->
	msg:get(Connector_Pid, get_type ).
		
discard(Connector_Pid) -> 
	gen_server:stop(Connector_Pid).
	%Connector_Pid ! discard. 
	
test() -> 
	C1_Pid = create(self(), dummy1_pid),
	C2_Pid = create(self(), dummy2_pid),
	connect(C1_Pid, C2_Pid),
	{C1_Pid, C2_Pid}.

handle_cast({connect, C_Pid}, {ResInst_Pid, _Connected_Pid, ConnectTyp_Pid})->
	{noreply,{ResInst_Pid,C_Pid,ConnectTyp_Pid}};

handle_cast(disconnect, {ResInst_Pid, _Connected_Pid, ConnectTyp_Pid})->
	{noreply,{ResInst_Pid,disconnected,ConnectTyp_Pid}}.
	
handle_call({get_connected, _Ref}, _From,{ResInst_Pid, Connected_Pid, ConnectTyp_Pid}) -> % {...} = State
	{reply,Connected_Pid, {ResInst_Pid, Connected_Pid, ConnectTyp_Pid}};

handle_call({get_ResInst, _Ref}, _From, {ResInst_Pid, Connected_Pid, ConnectTyp_Pid}) -> % {...} = State
	{reply, ResInst_Pid, {ResInst_Pid, Connected_Pid, ConnectTyp_Pid}};

handle_call({get_type, _Ref}, _From, {ResInst_Pid, Connected_Pid, ConnectTyp_Pid}) -> % {...} = State
	{reply, ConnectTyp_Pid, {ResInst_Pid, Connected_Pid, ConnectTyp_Pid}}.