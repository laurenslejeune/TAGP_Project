-module(singleSystemController).
-behaviour(gen_server).
-export([create/3,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([switchOnPumps/0,switchOffPumps/0,getSystemFlow/0,getSystemTemp/0]).

create(N_pipes,N_pumps,N_hex)->
    gen_server:start_link({local,?MODULE},?MODULE,[N_pipes,N_pumps,N_hex],[]).

init([N_pipes,N_pumps,N_hex])->
    {ok,{N_pipes,N_pumps,N_hex}}.

getSystemFlow()->
    msg:get(?MODULE,get_flow).

getSystemTemp()->
    msg:get(?MODULE,get_temp).

handle_call({get_flow,_Ref},_,State)->
    {ok,{N,Flow}} = systemFlow:getSystemFlow(systemFlow_0),
    {reply,{N,Flow},State};

handle_call({get_temp,_Ref},_,State)->
    {ok,{N,Temp}} = systemTemp:getSystemTemp(systemTemp_0),
    {reply,{N,Temp},State}.

handle_cast(switch_on,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pipes),
    testFunctions:switchOnAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}};

handle_cast(switch_off,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pipes),
    testFunctions:switchOffAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}}.

switchOnPumps()->
    gen_server:cast(?MODULE,switch_on).

switchOffPumps()->
    gen_server:cast(?MODULE,switch_off).