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
    {Result1,Dif1} = checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList),
    if(Result1 == false) ->
        %io:format("not converged yet"),
        {Result2,Dif2} = checkTempForPeriodAgain(RequiredTime+100,GetSystemTempPid,GetSystemFlowPid,DifList,Dif1),
        if(Dif2 > Dif1)->
            io:format("No convergence of temperature(~p->~p)~n",[Dif1,Dif2]);
        true->
            ok
        end;
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
            {true,abs(TempA-TempB)};
        true->
            %io:format("testing temperatures are (~p,~p)",[TempA,TempB]),
            {false,abs(TempA-TempB)}
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
            %io:format("Testing temperatures are (~p,~p)~n",[TempA,TempB]),
            {false,abs(TempA-TempB)}
        end;
    true->
        checkTempForPeriod(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})
    end.

checkTempForPeriodAgain(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,PrevDif)->
    {ok,{Time,Temp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    %io:format("{~p,~p} ",[Time,Temp]),
    checkTempForPeriodAgain(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{Time,Temp},PrevDif).


checkTempForPeriodAgain(1,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp},PrevDif)->
    {ok,{CurrentTime,CurrentTemp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    if(CurrentTime>PrevTime)->
        {ok,{_,Flow}}=getSystemFlow:getSystemFlow(GetSystemFlowPid),
        RequiredNeWTemperature = calculateNewTemp(PrevTemp,Flow,DifList),
        TempA = testFunctions:round(RequiredNeWTemperature,2),
        TempB = testFunctions:round(CurrentTemp,2),
        if(abs(TempA-TempB) < 0.1) ->
            {true,abs(TempA-TempB)};
        true->
            %io:format("testing temperatures are (~p,~p)",[TempA,TempB]),
            {false,abs(TempA-TempB)}
        end;
    true->
        checkTempForPeriodAgain(1,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp},PrevDif)
    end;

checkTempForPeriodAgain(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp},PrevDif)->
    {ok,{CurrentTime,CurrentTemp}} = getSystemTemp:getSystemTemp(GetSystemTempPid),
    if(CurrentTime>PrevTime)->
        {ok,{_,Flow}}=getSystemFlow:getSystemFlow(GetSystemFlowPid),
        %io:format("{~p,~p} ",[CurrentTime,CurrentTemp]),
        RequiredNeWTemperature = calculateNewTemp(PrevTemp,Flow,DifList),
        TempA = testFunctions:round(RequiredNeWTemperature,2),
        TempB = testFunctions:round(CurrentTemp,2),
        if
        (abs(TempA-TempB) < 0.1)->
            checkTempForPeriodAgain(RequiredTime-1,GetSystemTempPid,GetSystemFlowPid,DifList,{CurrentTime,CurrentTemp},PrevDif);
        (abs(TempA-TempB)<PrevDif)->
            checkTempForPeriodAgain(RequiredTime-1,GetSystemTempPid,GetSystemFlowPid,DifList,{CurrentTime,CurrentTemp},PrevDif);
        true->
            %io:format("Testing temperatures are (~p,~p)~n",[TempA,TempB]),
            {false,abs(TempA-TempB)}
        end;
    true->
        checkTempForPeriodAgain(RequiredTime,GetSystemTempPid,GetSystemFlowPid,DifList,{PrevTime,PrevTemp})
    end.


calculateNewTemp(TempIn,0,[Dif])->
    _FinalTemp = TempIn + (Dif*?EPS);

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

calculateNewTemp(TempIn,0,[Dif|OtherDifs])->
     NewTemp = TempIn + (Dif*?EPS),
     calculateNewTemp(NewTemp,0,OtherDifs);

calculateNewTemp(TempIn,Flow,[Dif|OtherDifs])->
     NewTemp = TempIn + (Dif/(Flow+?EPS)),
     calculateNewTemp(NewTemp,Flow,OtherDifs).
