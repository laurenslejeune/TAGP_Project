-module(getSystemTemp).
-export([create/0,init/0]).
-export([getSystemTemp/1,stopSystemTemp/1,setSystemTempDelay/2]).

create()->
    {ok,spawn(?MODULE,init,[])}.

init()->
    loop({0,0}).

loop(CurrentTemp)->
    receive 
        {new_temp,NewTemp} ->
            loop(NewTemp);
        {get_temp,ReplyFn} ->
            ReplyFn(CurrentTemp),
            loop(CurrentTemp);
        {stop,ReplyFn} ->
            ReplyFn(ok)
    end.

setSystemTempDelay(Pid,NewDelay)->
    Pid ! {change_delay,NewDelay}.

getSystemTemp(Pid)->
    msg:get(Pid,get_temp).

stopSystemTemp(Pid)->
    msg:get(Pid,stop).