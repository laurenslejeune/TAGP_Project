-module(survivor).
-export([start/0, entry/1, init/0]). 

start() ->
	register(survivor, spawn(?MODULE, init, [])).

entry(Data)-> 
	%io:format("Survivor ~p prints data ~p~n",[self(),Data]),
	ets:insert(logboek, {{now(), self()}, Data}). 

init() -> 
	ets:new(logboek, [named_table, ordered_set, public]),
	loop().

loop() -> 
	receive
		stop -> ets:delete(logboek), ok
	end. 

