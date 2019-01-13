-module(systemTemp).
-behaviour(gen_server).
-export([create/3,init/1]).
-export([handle_call/3,handle_cast/2,terminate/2]).
-export([update/1,getSystemTemp/1,stopSystemTemp/1]).

create(ListOfHeatEx,SystemFlow,Delay)->
    gen_server:start_link(?MODULE,[{ListOfHeatEx,SystemFlow,Delay}],[]).

init([{ListOfHeatEx,SystemFlow,Delay}]) ->
    survivor:entry(systemFlow_created),
    {ok,UpdaterPid} = updater:create(self(),Delay,systemTemp),
    {ok,{ListOfHeatEx,SystemFlow,0,0,UpdaterPid}}.
    %loop({ListOfHeatEx,SystemFlow,0,0,UpdaterPid}).

getSystemTemp(Pid)->
    msg:get(Pid,get_temp).

stopSystemTemp(Pid)->
    gen_server:stop(Pid).

update(Pid)->
    gen_server:cast(Pid,update).

handle_cast(update,{ListOfHeatEx,SystemFlow,Time,CurrentTemp,UpdaterPid})->
    {ok,{_,Flow}} = systemFlow:getSystemFlow(SystemFlow),
    NewTemp = updateTemp(ListOfHeatEx,Flow,CurrentTemp),
    {noreply,{ListOfHeatEx,SystemFlow,Time+1,NewTemp,UpdaterPid}}.

handle_call({get_temp,_Ref},_,{ListOfHeatEx,SystemFlow,Time,CurrentTemp,UpdaterPid})->
    {reply,{Time,CurrentTemp},{ListOfHeatEx,SystemFlow,Time,CurrentTemp,UpdaterPid}}.

terminate(Reason,{_,_,_,_,UpdaterPid})->
    UpdaterPid ! stop,
    {ok,Reason}.

updateTemp([HeatEx],Flow,CurrentTemp) ->
    {ok, {ok,Influence}} = heatExchangerInst:temp_influence(HeatEx),
    {ok, NewTemp} = Influence(Flow,CurrentTemp),
    NewTemp;

updateTemp([HeatEx|OtherHeatEx],Flow,CurrentTemp) ->
    {ok, {ok,Influence}} = heatExchangerInst:temp_influence(HeatEx),
    {ok, NewTemp} = Influence(Flow,CurrentTemp),
    updateTemp(OtherHeatEx,Flow,NewTemp).