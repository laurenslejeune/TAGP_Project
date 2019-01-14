-module(singleSystemController).
-behaviour(gen_server).
-export([create/4,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([switchOnPumps/0,switchOffPumps/0,getSystemFlow/0,getSystemTemp/0,switchOffRandomPump/0,switchOnRandomPump/0]).
-export([crashRandomProcess/0]).
create(N_pipes,N_pumps,N_hex,IsRelevant)->
    gen_server:start_link({local,?MODULE},?MODULE,[N_pipes,N_pumps,N_hex,IsRelevant],[]).

init([N_pipes,N_pumps,N_hex,IsRelevant])->
    collectDataSingle:create(6,IsRelevant),
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
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pumps),
    testFunctions:switchOnAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}};

handle_cast(switch_off_random_pump,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pumps),
    PumpsThatAreOn = testFunctions:getAllPumpsOn(Pumps),
    if(length(PumpsThatAreOn)>=1)->
        RandomIndex = rand:uniform(length(PumpsThatAreOn)), %Provides an integer between 1 and length(PumpsThatAreOn)
        RandomPump = lists:nth(RandomIndex,PumpsThatAreOn),
        testFunctions:switchOffAllPumps([RandomPump]),
        {noreply,{N_pipes,N_pumps,N_hex}};
    true->
        {noreply,{N_pipes,N_pumps,N_hex}}
    end;


handle_cast(switch_on_random_pump,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pumps),
    PumpsThatAreOff = testFunctions:getAllPumpsOff(Pumps),
    if(length(PumpsThatAreOff)>=1)->
        RandomIndex = rand:uniform(length(PumpsThatAreOff)), %Provides an integer between 1 and length(PumpsThatAreOff)
        RandomPump = lists:nth(RandomIndex,PumpsThatAreOff),
        testFunctions:switchOnAllPumps([RandomPump]),
        {noreply,{N_pipes,N_pumps,N_hex}};
    true->
        {noreply,{N_pipes,N_pumps,N_hex}}
    end;

handle_cast(switch_off,{N_pipes,N_pumps,N_hex})->
    Pumps = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pumps),
    testFunctions:switchOffAllPumps(Pumps),
    {noreply,{N_pipes,N_pumps,N_hex}};

handle_cast(crash,{N_pipes,N_pumps,N_hex})->
    PumpList = pumpFlowmeterHESupervisor:generateNPumpIds(1,N_pumps),
    HexList  = pumpFlowmeterHESupervisor:generateNHEXIds(1,N_hex),
    PipeList = fluidumSupervisor:generateNPipesIds(1,N_pipes),
    TotalList = PumpList++HexList++PipeList,
    %Randomize list:
    %Source : https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-randomly-re-arrange-list-elements
    RandomList = [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- TotalList])],
    RandomProcess = lists:nth(1,RandomList),
    msg:get(RandomProcess,amedoela),
    {noreply,{N_pipes,N_pumps,N_hex}}.

switchOnPumps()->
    gen_server:cast(?MODULE,switch_on).

switchOffPumps()->
    gen_server:cast(?MODULE,switch_off).

switchOffRandomPump()->
    gen_server:cast(?MODULE,switch_off_random_pump).

switchOnRandomPump()->
    gen_server:cast(?MODULE,switch_on_random_pump).

crashRandomProcess()->
    gen_server:cast(?MODULE,crash).
