-module(prop_systemFlow_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_calculate_flow() ->
    ?FORALL(Time,integer(0,200),setup_system_flow_until(Time)).

setup_system_flow_until(Time) ->
    {ok, {_,_,_,_,_,_,Tasks}} = buildSystem:start_3pipes_water_pump_flowmeter_heatex(true),
    [PumpInst,_,FlowMeterInst,_,_,_] = Tasks,
    pumpInst:switch_on(PumpInst),
    {ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create([PumpInst],FlowMeterInst,GetSystemFlowPid),
    
    Result = test_system_flow_until(Time,GetSystemFlowPid),

    SystemFlowPid ! stop,
    getSystemFlow:stopSystemFlow(GetSystemFlowPid),
    %buildSystem:stop(),
    Result.

test_system_flow_until(Time,GetSystemFlowPid)->
    {ok,{N,Flow}} = getSystemFlow:getSystemFlow(GetSystemFlowPid),
    %io:format("Inquiry for Time=~p, current time is ~p~n",[Time,N]),
    CorrectFlow = testFunctions:flowForBasicSituation(N),
    Condition = (testFunctions:round(Flow,5)==testFunctions:round(CorrectFlow,5)),
    if (Condition==true)->
        if(N>=Time)->
            true;
        true->
                test_system_flow_until(Time,GetSystemFlowPid)
        end;
    true-> 
        false
    end.