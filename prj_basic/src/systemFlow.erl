-module(systemFlow).
-export([create/3,init/1,loop/1]).

create(ListOfPumpInstances,FlowMeter,RegisterPid)->
    {ok,spawn(?MODULE,init,[{ListOfPumpInstances,FlowMeter,RegisterPid}])}.

init({ListOfPumpInstances,FlowMeter,RegisterPid}) ->
    survivor:entry(systemFlow_created),
    RegisterPid ! {new_flow,{0,0}},
    loop({ListOfPumpInstances,FlowMeter,0,0,RegisterPid}).

loop({ListOfPumpInstances,FlowMeter,Time,CurrentFlow,RegisterPid})->
    
    receive
        stop -> ok
        %We calculate the current flow every 1ms
        after 1 ->
            %CurrentPumpForce = getCurrentPumpForce(CurrentFlow,),
            %Calculate the influence of the pipes on the current flow
            PipeInfluence = getPipeInfluence(CurrentFlow,FlowMeter),

            %Calculate the new flow generated by the pumps
            %First calculate the total maximum force that can be delivered by all pumps together:
            MaxForce = getMaxForce(ListOfPumpInstances),
            NewFlow = getNewFlow((CurrentFlow+PipeInfluence),ListOfPumpInstances,0,MaxForce),
            
            %io:format("Current flow is ~p, Pipe influence is ~p, NewFlow is ~p~n",[CurrentFlow,PipeInfluence,NewFlow]),

            %Store the new flow
            RegisterPid ! {new_flow,{Time+1,NewFlow}},

            loop({ListOfPumpInstances,FlowMeter,Time+1,NewFlow,RegisterPid})
    end.

getCurrentPumpForce(Flow,[NextPump|Others],Force) ->
    %Get the flow influence for this pump
    {ok,InfluenceFn} = pumpInst:flow_influence(NextPump),
    %Calculate the force corresponding this influence
    AdditionalForce = InfluenceFn(Flow),
    getCurrentPumpForce(Flow,Others,Force+AdditionalForce);

getCurrentPumpForce(_,[],Force)->
    Force.


getNewFlow(Flow,[NextPump|Others],NewFlow,MaxForce) ->
    %Get the flow influence for this pump
    {ok,InfluenceFn} = pumpInst:flow_influence(NextPump),
    
    %Calculate this pumps contribution in the total flow
    PumpFlow = InfluenceFn(0) / MaxForce * Flow,
    
    %Calculate the force corresponding this influence
    AdditionalForce = InfluenceFn(PumpFlow),
    %Calculate the flow generated using this force
    AdditionalFlow = getFlow(AdditionalForce),
    %io:format("Additional force is ~p, additional flow is ~p~n",[AdditionalForce,AdditionalFlow]),
    getNewFlow(Flow,Others,NewFlow+AdditionalFlow,MaxForce);

getNewFlow(_,[],NewFlow,_)->
    NewFlow.

getPipeInfluence(Flow,FlowMeter)->
    {ok,Influence}=flowMeterInst:estimate_flow(FlowMeter,{0,Flow}),
    %io:format("Flow is ~p, influence is ~p~n",[Flow,Influence]),
    Influence.

getFlow(Force)->
    %Derivation refer to .xlsx file
    10 -((-5/4)+(math:sqrt(2025-8*Force)/4)).

getMaxForce(Pumps)->
    getMaxForce(Pumps,0).

getMaxForce([P],Force)->
    {ok,InfluenceFn} = pumpInst:flow_influence(P),
    Force + InfluenceFn(0);

getMaxForce([P|Pumps],Force)->
    {ok,InfluenceFn} = pumpInst:flow_influence(P),
    getMaxForce(Pumps,Force+InfluenceFn(0)).