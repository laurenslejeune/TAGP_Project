-module(getSystemFlow).
-export([create/0,init/0]).
-export([getSystemFlow/1,stopSystemFlow/1,setSystemFlowDelay/2]).

create() ->
    {ok,spawn(?MODULE,init,[])}.

init() ->
    survivor:entry(getSystemFlow_created),
    loop({0,0}).

loop(Flow)->
    receive
        {new_flow,NewFlow} ->
            %io:format("Received new flow~p~n",[NewFlow]),
            loop(NewFlow);
        {get_flow,ReplyFn} ->
            ReplyFn(Flow),
            loop(Flow);
        {stop,ReplyFn} ->
            ReplyFn(ok)            
    end.

setSystemFlowDelay(Pid,NewDelay)->
    Pid ! {change_delay,NewDelay}.

getSystemFlow(Pid)->
    %io:format("required process exists ~p, getSystemFlow ",[erlang:is_process_alive(Pid)]),
    Answer = msg:get(Pid,get_flow),
    %io:format("~p~n",[Answer]),
    Answer.

stopSystemFlow(Pid)->
    msg:get(Pid,stop).