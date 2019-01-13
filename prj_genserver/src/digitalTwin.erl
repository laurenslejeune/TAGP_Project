-module(digitalTwin).
-behaviour(gen_server).
-export([create/3]).
-export([init/1]).
-export([handle_call/3,handle_cast/2,terminate/2]).
-export([getDigitalTwinData/0,stopDigitalTwin/0]).
-export([startPumps/0,stopPumps/0]).

-spec create(pos_integer(),pos_integer(),pos_integer())-> tuple().
%In this implementation, a pump can also be an heat-exchangers and vice-versa
create(N_pipes, N_pumps, N_Hex) when (N_pumps > N_pipes) or (N_Hex > N_pipes)->
    {error,too_many_pums_and_hex};

create(N_pipes, N_pumps, N_Hex) ->
    {ok,Pid} = gen_server:start_link(?MODULE,[{N_pipes, N_pumps, N_Hex}],[]),
    register(digital_twin,Pid),
    {ok,Pid}.
    %We generate a random system with the given conditions:
   

init([{N_pipes, N_pumps, N_Hex}])->
    DifList = testFunctions:generateDifList(N_Hex,[]),
    
    {Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_Hex,false,DifList),
    {ok,SystemFlowPid1} = systemFlow:create(Pumps1,FlowMeterInst1,100),
    {ok, SystemTempPid1} = systemTemp:create(HeatExchangers1,SystemFlowPid1,120),
    System1 = {{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},

	{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2} = buildSystem:generateDigitalTwin({Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1,SystemFlowPid1,DifList}),
    {ok,SystemFlowPid2} = systemFlow:create(Pumps2,FlowMeterInst2,100),
    {ok, SystemTempPid2} = systemTemp:create(HeatExchangers2,SystemFlowPid2,120),
    System2 = {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}},

    collectData:create(6),
    {ok,{System1,System2}}.
    %loop({System1,System2}).

handle_call({get_data,_Ref},_,{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
                              {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}})->
    {ok,{N_Flow_1,Flow1}} = systemFlow:getSystemFlow(SystemFlowPid1),
    {ok,{N_Flow_2,Flow2}} = systemFlow:getSystemFlow(SystemFlowPid2),
    {ok,{N_Temp_1,Temp1}} = systemTemp:getSystemTemp(SystemTempPid1),
    {ok,{N_Temp_2,Temp2}} = systemTemp:getSystemTemp(SystemTempPid2),
    {reply,{{N_Flow_1,Flow1},{N_Flow_2,Flow2},{N_Temp_1,Temp1},{N_Temp_2,Temp2}},{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
                                                                                 {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}}}.

handle_cast(start_pumps,{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
                        {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}})->
    switchOnAllPumps(Pumps2),
    {noreply,{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
    {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}}};

handle_cast(stop_pumps,{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
                        {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}})->
    switchOffAllPumps(Pumps2),
    {noreply,{{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
    {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}}}.



% loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
%     {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}}) ->
%     receive 
%         {get_data,ReplyFn} ->
%             {ok,{N_Flow_1,Flow1}} = systemFlow:getSystemFlow(SystemFlowPid1),
%             {ok,{N_Flow_2,Flow2}} = systemFlow:getSystemFlow(SystemFlowPid2),
%             {ok,{N_Temp_1,Temp1}} = systemTemp:getSystemTemp(SystemTempPid1),
%             {ok,{N_Temp_2,Temp2}} = systemTemp:getSystemTemp(SystemTempPid2),
%             ReplyFn({{N_Flow_1,Flow1},{N_Flow_2,Flow2},{N_Temp_1,Temp1},{N_Temp_2,Temp2}}),
%             loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
%                 {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}});
%         start_pumps ->
%             switchOnAllPumps(Pumps2),
%             loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
%                 {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}});
%         stop_pumps ->
%             switchOffAllPumps(Pumps2),
%             loop({{{Pipes1,Pumps1,FlowMeterInst1,HeatExchangers1},{SystemFlowPid1,SystemTempPid1}},
%                 {{Pipes2,Pumps2,FlowMeterInst2,HeatExchangers2},{SystemFlowPid2,SystemTempPid2}}});
%         stop ->
%             systemFlow:stopSystemFlow(SystemFlowPid1),
%             systemFlow:stopSystemFlow(SystemFlowPid2),
%             systemTemp:stopSystemTemp(SystemTempPid1),
%             systemTemp:stopSystemTemp(SystemTempPid2),
%             unregister(digital_twin)
%     end.

terminate(Reason,{{{_,_,_,_},{SystemFlowPid1,SystemTempPid1}},{{_,_,_,_},{SystemFlowPid2,SystemTempPid2}}})->
    systemFlow:stopSystemFlow(SystemFlowPid1),
    systemFlow:stopSystemFlow(SystemFlowPid2),
    systemTemp:stopSystemTemp(SystemTempPid1),
    systemTemp:stopSystemTemp(SystemTempPid2),
    unregister(digital_twin),
    {ok,Reason}.

startPumps()->
    gen_server:cast(digital_twin,start_pumps).

stopPumps()->
    gen_server:cast(digital_twin,stop_pumps).

getDigitalTwinData()->
    msg:get(digital_twin,get_data).

stopDigitalTwin()->
    gen_server:stop(digital_twin).

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