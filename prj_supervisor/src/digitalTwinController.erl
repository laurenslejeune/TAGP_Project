-module(digitalTwinController).
-behaviour(gen_server).
-export([create/4,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([getSystemFlow/0,getSystemTemp/0,switchOnPumps/0,switchOffPumps/0]).

create(N_pipes,N_pumps,N_hex,IsRelevant)->
    gen_server:start_link({local,?MODULE},?MODULE,[N_pipes,N_pumps,N_hex,IsRelevant],[]).

init([N_pipes,N_pumps,N_hex,IsRelevant])->
    collectDataTwin:create(6,IsRelevant),
    {ok,{N_pipes,N_pumps,N_hex}}.

getSystemFlow()->
    msg:get(?MODULE,get_flow).

getSystemTemp()->
    msg:get(?MODULE,get_temp).

handle_call({get_flow,_},_,State)->
    {ok,{N1,Flow1}} = systemFlow:getSystemFlow(systemFlow_0),
    {ok,{N2,Flow2}} = systemFlow:getSystemFlow(systemFlow_1),
    {reply,[{N1,Flow1},{N2,Flow2}],State};

handle_call({get_temp,_},_,State)->
    {ok,{N1,Temp1}} = systemTemp:getSystemTemp(systemTemp_0),
    {ok,{N2,Temp2}} = systemTemp:getSystemTemp(systemTemp_1),
    {reply,[{N1,Temp1},{N2,Temp2}],State}.

handle_cast(switch_on,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(N_pipes+1,2*N_pipes),
    testFunctions:switchOnAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}};

handle_cast(switch_off,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(N_pipes+1,2*N_pipes),
    testFunctions:switchOffAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}}.

switchOnPumps()->
    gen_server:cast(?MODULE,switch_on).

switchOffPumps()->
    gen_server:cast(?MODULE,switch_off).