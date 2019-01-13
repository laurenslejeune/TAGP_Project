-module(updater).
-export([create/3,init/3]).

create(Pid,Delay,Identifier)->
    {ok,spawn(?MODULE,init,[Pid,Delay,Identifier])}.

init(Pid,Delay,Identifier)->
    survivor:entry({updater_creater,Pid,Delay,Identifier}),
    loop(Pid,Delay,Identifier).

loop(Pid,Delay,systemFlow)->
    receive stop->
        ok
    after Delay ->
        systemFlow:update(Pid),
        loop(Pid,Delay,systemFlow)
    end;

loop(Pid,Delay,systemTemp)->
    receive stop->
        ok
    after Delay ->
        systemTemp:update(Pid),
        loop(Pid,Delay,systemTemp)
    end.