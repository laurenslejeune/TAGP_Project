-module(prop_systemTemp_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(EPS,0.00000000000000000001).

prop_test_system_temperature()->
    
    ?FORALL({N_pipes,N_pumps,N_he},{integer(5,20),integer(1,5),integer(1,5)},test_system_temp(N_pipes,N_pumps,N_he)).

test_system_temp(N_pipes,N_pumps,N_he)->
    DifList = testFunctions:generateDifList(N_he,[]),
    {_,Pumps,FlowMeterInst,HeatExchangers} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_he,true,DifList),
    
    prop_base:switchOnAllPumps(Pumps),
    {ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok, SystemFlowPid} = systemFlow:create(Pumps,FlowMeterInst,GetSystemFlowPid),

    {ok, GetSystemTempPid} = getSystemTemp:create(),
    {ok, SystemTempPid} = systemTemp:create(HeatExchangers,GetSystemFlowPid,GetSystemTempPid),

    RequiredTime = rand:uniform(20)+2,
    %io:format("Testing temperature for required time ~p~n",[RequiredTime]),
    Result1 = checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList),
    if(Result1 == false) ->
        %io:format("not converged yet"),
        Result2 = checkTempForPeriod(RequiredTime+100,GetSystemTempPid,GetSystemFlowPid,DifList);
    true ->
        Result2 = Result1
    end,
    SystemFlowPid ! stop,
	SystemTempPid ! stop,
	getSystemFlow:stopSystemFlow(GetSystemFlowPid),
	getSystemTemp:stopSystemTemp(GetSystemTempPid),
	buildSystem:stop(),

    Result2.


checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList)->
    {ok,{Time,Temp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    %io:format("{~p,~p} ",[Time,Temp]),
    checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{Time,Temp}).


checkTempForPeriod(1,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})->
    {ok,{CurrentTime,CurrentTemp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    if(CurrentTime>PrevTime)->
        {ok,{_,Flow}}=getSystemFlow:getSystemFlow(GetSystemFlowPid),
        RequiredNeWTemperature = calculateNewTemp(PrevTemp,Flow,DifList),
        TempA = testFunctions:round(RequiredNeWTemperature,2),
        TempB = testFunctions:round(CurrentTemp,2),
        if(abs(TempA-TempB) < 0.1) ->
            true;
        true->
            io:format("testing temperatures are (~p,~p)",[TempA,TempB]),
            false
        end;
    true->
        checkTempForPeriod(1,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})
    end;

checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})->
    {ok,{CurrentTime,CurrentTemp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    if(CurrentTime>PrevTime)->
        {ok,{_,Flow}}=getSystemFlow:getSystemFlow(GetSystemFlowPid),
        %io:format("{~p,~p} ",[CurrentTime,CurrentTemp]),
        RequiredNeWTemperature = calculateNewTemp(PrevTemp,Flow,DifList),
        TempA = testFunctions:round(RequiredNeWTemperature,2),
        TempB = testFunctions:round(CurrentTemp,2),
        if(abs(TempA-TempB) < 0.1)->
            checkTempForPeriod(RequiredTime-1,GetSystemTempPid,GetSystemFlowPid,DifList,{CurrentTime,CurrentTemp});
        true->
            io:format("Testing temperatures are (~p,~p)~n",[TempA,TempB]),
            false
        end;
    true->
        checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})
    end.

calculateNewTemp(TempIn,Flow,[Dif])->
    _FinalTemp = TempIn + (Dif/(Flow+?EPS));
    % if((FinalTemp < 100) and (FinalTemp > 0)) ->
    %     FinalTemp;
    % true->
    %     if(FinalTemp > 100)->
    %         100;
    %     true->
    %         0
    %     end
    % end;

calculateNewTemp(TempIn,Flow,[Dif|OtherDifs])->
     NewTemp = TempIn + (Dif/(Flow+?EPS)),
     calculateNewTemp(NewTemp,Flow,OtherDifs).
