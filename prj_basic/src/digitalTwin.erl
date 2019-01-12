-module(digitalTwin).

-export([create/3]).
-export([init/1]).
-export([getDigitalTwinData/0,stopDigitalTwin/0]).
-export([startPumps/0,stopPumps/0]).

-spec create(pos_integer(),pos_integer(),pos_integer())-> tuple().
%In this implementation, a pump can also be an heat-exchangers and vice-versa
create(N_pipes, N_pumps, N_Hex) when (N_pumps > N_pipes) or (N_Hex > N_pipes)->
    {error,too_many_pums_and_hex};

create(N_pipes, N_pumps, N_Hex) ->
    Pid = spawn(?MODULE,init,[{N_pipes, N_pumps, N_Hex}]),
    register(digital_twin,Pid),
    {ok,Pid}.
    %We generate a random system with the given conditions:
   

init({N_pipes, N_pumps, N_Hex})->
    DifList = testFunctions:generateDifList(N_Hex,[]),
    {Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_Hex,false,DifList),
	{ok, GetSystemFlowPid1} = getSystemFlow:create(),
    {ok,SystemFlowPid1} = systemFlow:create(Pumps1,FlowMeterInst1,GetSystemFlowPid1),
	getSystemFlow:setSystemFlowDelay(SystemFlowPid1,800),
    {ok, GetSystemTempPid1} = getSystemTemp:create(),
    {ok, SystemTempPid1} = systemTemp:create(HeatExchangers1,GetSystemFlowPid1,GetSystemTempPid1),
    getSystemTemp:setSystemTempDelay(SystemTempPid1,1000),
    System1 = {{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{GetSystemFlowPid1,GetSystemTempPid1},{SystemFlowPid1,SystemTempPid1}},

	{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2} = buildSystem:generateDigitalTwin({Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1,GetSystemFlowPid1,DifList}),
	{ok, GetSystemFlowPid2} = getSystemFlow:create(),
    {ok,SystemFlowPid2} = systemFlow:create(Pumps2,FlowMeterInst2,GetSystemFlowPid2),
	getSystemFlow:setSystemFlowDelay(SystemFlowPid2,800),
    {ok, GetSystemTempPid2} = getSystemTemp:create(),
    {ok, SystemTempPid2} = systemTemp:create(HeatExchangers2,GetSystemFlowPid2,GetSystemTempPid2),
    getSystemTemp:setSystemTempDelay(SystemTempPid2,1000),
    System2 = {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{GetSystemFlowPid2,GetSystemTempPid2},{SystemFlowPid2,SystemTempPid2}},

    collectData:create(SystemFlowPid1,SystemFlowPid2,SystemTempPid1,SystemTempPid2,10),

    loop({System1,System2}).

loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{GetSystemFlowPid1,GetSystemTempPid1},{SystemFlowPid1,SystemTempPid1}},
    {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{GetSystemFlowPid2,GetSystemTempPid2},{SystemFlowPid2,SystemTempPid2}}}) ->
    receive 
        {get_data,ReplyFn} ->
            {ok,{N_Flow_1,Flow1}} = getSystemFlow:getSystemFlow(GetSystemFlowPid1),
            {ok,{N_Flow_2,Flow2}} = getSystemFlow:getSystemFlow(GetSystemFlowPid2),
            {ok,{N_Temp_1,Temp1}} = getSystemTemp:getSystemTemp(GetSystemTempPid1),
            {ok,{N_Temp_2,Temp2}} = getSystemTemp:getSystemTemp(GetSystemTempPid2),
            ReplyFn({{N_Flow_1,Flow1},{N_Flow_2,Flow2},{N_Temp_1,Temp1},{N_Temp_2,Temp2}}),
            loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{GetSystemFlowPid1,GetSystemTempPid1},{SystemFlowPid1,SystemTempPid1}},
                {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{GetSystemFlowPid2,GetSystemTempPid2},{SystemFlowPid2,SystemTempPid2}}});
        start_pumps ->
            switchOnAllPumps(Pumps2),
            loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{GetSystemFlowPid1,GetSystemTempPid1},{SystemFlowPid1,SystemTempPid1}},
                {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{GetSystemFlowPid2,GetSystemTempPid2},{SystemFlowPid2,SystemTempPid2}}});
        stop_pumps ->
            switchOffAllPumps(Pumps2),
            loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{GetSystemFlowPid1,GetSystemTempPid1},{SystemFlowPid1,SystemTempPid1}},
                {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{GetSystemFlowPid2,GetSystemTempPid2},{SystemFlowPid2,SystemTempPid2}}});
        stop ->
            SystemFlowPid1 ! stop,
            SystemFlowPid2 ! stop,
            getSystemFlow:stopSystemFlow(GetSystemFlowPid1),
            getSystemFlow:stopSystemFlow(GetSystemFlowPid2),
            SystemTempPid1 ! stop,
            SystemTempPid2 ! stop,
            getSystemTemp:stopSystemTemp(GetSystemTempPid1),
            getSystemTemp:stopSystemTemp(GetSystemTempPid2),
            unregister(digital_twin)
    end.
startPumps()->
    digital_twin ! start_pumps.

stopPumps()->
    digital_twin ! stop_pumps.

getDigitalTwinData()->
    msg:get(digital_twin,get_data).

stopDigitalTwin()->
    digital_twin ! stop.

switchOnAllPumps([Pump])->
	pumpInst:switch_on(Pump);

switchOnAllPumps([Pump|OtherPumps])->
	pumpInst:switch_on(Pump),
	switchOnAllPumps(OtherPumps).

switchOffAllPumps([Pump])->
	pumpInst:switch_off(Pump);

switchOffAllPumps([Pump|OtherPumps])->
	pumpInst:switch_off(Pump),
	switchOffAllPumps(OtherPumps).