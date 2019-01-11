-module(digitalTwin1).

-export([create_system/3]).

%In this implementation, a pump can also be an heat-exchangers and vice-versa
create_system(N_pipes, N_pumps, N_Hex) when (N_pumps > N_pipes) or (N_Hex > N_pipes)->
    {error,too_many_pums_and_hex};

create_system(N_pipes, N_pumps, N_Hex) ->
    survivor:start(),
    %We generate a random system with the given conditions:
    {Pipes,Pumps,FlowMeterInst,HeatExchangers} = buildSystem:generateRandomSystem(N_pipes,N_pumps,N_Hex,false),
    {ok, GetSystemFlowPid} = getSystemFlow:create(),
    {ok,SystemFlowPid} = systemFlow:create(Pumps,FlowMeterInst,GetSystemFlowPid),
    
    {ok,digital_twin_created}.