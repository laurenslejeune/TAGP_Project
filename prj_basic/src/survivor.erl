-module(survivor).
-export([start/0, entry/1, init/0]). 
-include_lib("eunit/include/eunit.hrl").

%General remark: Adding ?debugFmt to the survivor may cause it to terminate too slowly

start() ->
	(whereis(survivor) =:= undefined) orelse unregister(survivor), 
	register(survivor, spawn(?MODULE, init, [])).

entry(Data)-> 
	%io:format("Survivor ~p prints data ~p~n",[self(),Data]),
	%?debugFmt("Start survivor~n",[]),
	ets:insert(logboek, {{now(), self()}, Data}). 

init() -> 
	(ets:info(logboek) =:= undefined) orelse ets:delete(logboek),
	ets:new(logboek, [named_table, ordered_set, public]),
	loop().

loop() -> 
	receive
		stop -> %?debugFmt("Stop survivor~n",[]),
				%ets:delete(logboek), 
				ok
	end. 