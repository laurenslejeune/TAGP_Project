-module(systemTemp).
-export([create/3,init/1]).


create(ListOfHeatEx,GetSystemFlowPid,RegisterPid)->
    {ok,spawn(?MODULE,init,[{ListOfHeatEx,GetSystemFlowPid,RegisterPid}])}.

init({ListOfHeatEx,GetSystemFlowPid,RegisterPid}) ->
    survivor:entry(systemFlow_created),
    RegisterPid ! {new_temp,{0,0}},
    loop({ListOfHeatEx,GetSystemFlowPid,0,0,RegisterPid,10}).

loop({ListOfHeatEx,GetSystemFlowPid,Time,CurrentTemp,RegisterPid,Delay})->
    receive 
        stop ->
            ok;
        {change_delay,NewDelay} ->
            io:format("Switched temp update delay to ~p~n",[NewDelay]),
            loop({ListOfHeatEx,GetSystemFlowPid,Time,CurrentTemp,RegisterPid,NewDelay})
    after Delay ->
        {ok,{_,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
        NewTemp = updateTemp(ListOfHeatEx,Flow,CurrentTemp),
        RegisterPid ! {new_temp,{Time+1,NewTemp}},
        %io:format("~p|Temp=~p~n",[Time+1,NewTemp]),
        loop({ListOfHeatEx,GetSystemFlowPid,Time+1,NewTemp,RegisterPid,Delay})
    end.

updateTemp([HeatEx],Flow,CurrentTemp) ->
    {ok, {ok,Influence}} = heatExchangerInst:temp_influence(HeatEx),
    {ok, NewTemp} = Influence(Flow,CurrentTemp),
    NewTemp;

updateTemp([HeatEx|OtherHeatEx],Flow,CurrentTemp) ->
    {ok, {ok,Influence}} = heatExchangerInst:temp_influence(HeatEx),
    {ok, NewTemp} = Influence(Flow,CurrentTemp),
    updateTemp(OtherHeatEx,Flow,NewTemp).