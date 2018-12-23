-module(flowMeterTyp).
-behaviour(gen_server).
-export([handle_call/3,handle_cast/2]).
-export([create/0, init/1]).
% -export([dispose/2, enable/2, new_version/2]).
% -export([get_initial_state/3, get_connections_list/2]). % use resource_type
% -export([update/3, execute/7, refresh/4, cancel/4, update/7, available_ops/2]). 
-include_lib("eunit/include/eunit.hrl").

create() -> 
	gen_server:start_link(?MODULE,[],[]).
	%{ok, spawn(?MODULE, init, [])}.

init([]) -> 
	survivor:entry(flowMeterTyp_created),
	{ok,[]}.
	%loop().

handle_call({initial_state, [MeterInst_Pid, [ResInst_Pid, RealWorldCmdFn]],_Ref},_From,[])->
	{ok, [L | _ ] } = resource_instance:list_locations(ResInst_Pid),
	{ok, Fluidum} = location:get_Visitor(L),
	State = #{meterInst => MeterInst_Pid, resInst => ResInst_Pid, fluidum => Fluidum, rw_cmd => RealWorldCmdFn},
	{reply,State,[]};

handle_call({measure_flow,State,_Ref},_From,[])->
	#{rw_cmd := ExecFn} = State,
	{reply,ExecFn(),[]};

handle_call({estimate_flow,State,_Ref},_From,[])->
	#{fluidum := F} = State,
	{ok, C} = fluidumInst:get_resource_circuit(F),
	{reply,computeFlow(C),[]};

handle_call({isOn,State,_Ref},_From,[])->
	#{on_or_off := OnOrOff} = State,
	{reply,OnOrOff,[]}.

handle_cast(_,[])->
	{noreply,[]}.

computeFlow(ResCircuit) -> 
 	Interval = {0, 10}, % ToDo >> discover upper bound for flow.
	%Remark:
	%maps:next(maps:iterator(ResCircuit)) takes the first Key-Value pair
	%in the newly created iterator
	{ok, InfluenceFnCircuit} = influence(maps:next(maps:iterator(ResCircuit)), []),
	compute(Interval, InfluenceFnCircuit).

influence({C, _ , Iter }, Acc) ->
		{ok, InflFn} = apply(resource_instance, get_flow_influence, [C]),
		influence(maps:next(Iter), [ InflFn | Acc ] );

influence(none, Acc) -> {ok, Acc}. 

compute({Low, High}, _InflFnCircuit) when (High - Low) < 1 -> 
	%Todo convergentiewaarde instelbaar maken. 
	(Low + High) / 2 ;
	
compute({Low, High}, InflFnCircuit) ->
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	Mid = (H + L) / 2, M = eval(Mid, InflFnCircuit, 0),
	if 	M > 0 -> 
			compute({Low, Mid}, InflFnCircuit);
        true -> % works as an 'else' branch
            compute({Mid, High}, InflFnCircuit)
    end.

	
eval(Flow, [Fn | RemFn] , Acc) ->
	eval(Flow, RemFn, Acc + Fn(Flow));

eval(_Flow, [], Acc) -> Acc. 