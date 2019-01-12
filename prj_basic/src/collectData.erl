-module(collectData).
-export([create/5,init/1]).

create(GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,Delay) ->
    {ok,spawn(?MODULE,init,[{GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,Delay}])}.

init({GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,Delay})->
    ets:new(stored_data, [named_table, ordered_set, public]),
    %io:format("Created stored__data table~n",[]),
    loop({GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,0,Delay}).

loop({GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,N,Delay})->
    if(N<1000)->
        %{ok,{_,Flow1}} = getSystemFlow:getSystemFlow(GetSystemFlow1),
        %{ok,{_,Flow2}} = getSystemFlow:getSystemFlow(GetSystemFlow2),
        %{ok,{_,Temp1}} = getSystemTemp:getSystemTemp(GetSystemTemp1),
        %{ok,{_,Temp2}} = getSystemTemp:getSystemTemp(GetSystemTemp2),
        {ok,{{_,Flow1},{_,Flow2},{_,Temp1},{_,Temp2}}} = digitalTwin:getDigitalTwinData(),
        Data = {Flow1,Flow2,Temp1,Temp2},
        %Data= {N+1,N-1,N*2,N/2},
        ets:insert(stored_data, {N, Data}),
        timer:sleep(Delay),
        loop({GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2,N+1,Delay});
    true->
        storeData(N)
    end.

storeData(N)->
    {ok,File}=file:open("data.csv",write),
    file:write(File,"Flow1,Flow2,Temp1,Temp2\n"),
    writeLine(N,0,File),
    file:close(File),
    io:format("Data has been stored to file data.csv~n",[]),
    ets:delete(stored_data).

writeLine(N,CurrentN,File) when N==(CurrentN+1)->
    [{CurrentN,{GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2}}]=ets:lookup(stored_data,CurrentN),
    A = string:concat(io_lib:format("~.2f", [GetSystemFlow1*1.0]),","),
    B = string:concat(io_lib:format("~.2f", [GetSystemFlow2*1.0]),","),
    C = string:concat(io_lib:format("~.2f", [GetSystemTemp1*1.0]),","),
    D = io_lib:format("~.2f", [GetSystemTemp2*1.0]),
    AB = string:concat(A,B),
    ABC = string:concat(AB,C),
    String = string:concat(ABC,D),
    file:write(File,String);

writeLine(N,CurrentN,File) ->
    %io:format("Element ~p is:~p~n",[CurrentN,ets:lookup(stored_data,CurrentN)]),
    [{CurrentN,{GetSystemFlow1,GetSystemFlow2,GetSystemTemp1,GetSystemTemp2}}]=ets:lookup(stored_data,CurrentN),
    A = string:concat(io_lib:format("~.2f", [GetSystemFlow1*1.0]),","),
    B = string:concat(io_lib:format("~.2f", [GetSystemFlow2*1.0]),","),
    C = string:concat(io_lib:format("~.2f", [GetSystemTemp1*1.0]),","),
    D = string:concat(io_lib:format("~.2f", [GetSystemTemp2*1.0]),"\n"),
    AB = string:concat(A,B),
    ABC = string:concat(AB,C),
    String = string:concat(ABC,D),
    file:write(File,String),
    writeLine(N,CurrentN+1,File).