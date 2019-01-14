-module(collectData).
-export([create/1,init/1]).

-spec create(non_neg_integer())-> {'ok',pid()}.
create(Delay) ->
    {ok,spawn(?MODULE,init,[{Delay}])}.

-spec init({non_neg_integer()})->'true'.
init({Delay})->
    ets:new(stored_data, [named_table, ordered_set, public]),
    %io:format("Created stored__data table~n",[]),
    loop({0,Delay}).

-spec loop({non_neg_integer(),non_neg_integer()}) -> 'true'.
loop({N,Delay})->
    if(N<10000)->
        %{ok,{_,Flow1}} = getSystemFlow:getSystemFlow(GetSystemFlow1),
        %{ok,{_,Flow2}} = getSystemFlow:getSystemFlow(GetSystemFlow2),
        %{ok,{_,Temp1}} = getSystemTemp:getSystemTemp(GetSystemTemp1),
        %{ok,{_,Temp2}} = getSystemTemp:getSystemTemp(GetSystemTemp2),
        {ok,{{_,Flow1},{_,Flow2},{_,Temp1},{_,Temp2}}} = digitalTwin:getDigitalTwinData(),
        Data = {Flow1,Flow2,Temp1,Temp2},
        %Data= {N+1,N-1,N*2,N/2},
        ets:insert(stored_data, {N, Data}),
        timer:sleep(Delay),
        loop({N+1,Delay});
    true->
        storeData(N)
    end.

-spec storeData(non_neg_integer())->'true'.
storeData(N)->
    %% Remark: Dialyzer complains when setting 'write' instead of '[write]'
    {ok,File}=file:open("data.csv",[write]),
    file:write(File,"Flow1,Flow2,Temp1,Temp2\n"),
    writeLine(N,0,File),
    file:close(File),
    io:format("Data has been stored to file data.csv~n",[]),
    ets:delete(stored_data).


-spec writeLine(non_neg_integer(),non_neg_integer(),file:io_device())-> 'ok' | {'error',atom()}.
writeLine(N,CurrentN,File) when ((CurrentN)==(N-1))->
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