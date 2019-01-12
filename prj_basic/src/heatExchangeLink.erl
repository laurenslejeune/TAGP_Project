-module(heatExchangeLink).
-export([get_temp_influence/1]).
%Added EPS to avoid division by 0
-define(EPS,0.00000000000000000001).
-include_lib("eunit/include/eunit.hrl").
% dummy module - replace by realistic model
% e.g. the outgoing temperature provided by get_temp_influence
% will saturate when approching the inTemp on the other side
% of the link. 


get_temp_influence(HE_link_spec) ->
    %Original code:
    {ok, fun(Flow, InTemp) -> #{delta := Difference} = HE_link_spec, {ok, InTemp + (Difference/(Flow+?EPS))} end}.
    

    %Added additional safety: The temperature cannot exceed 100, or subceed 0 degrees celcius
    %This does not really matter for a dummy model, but it has been done anyway
% {ok, fun(Flow, InTemp) when (InTemp < 100) and (InTemp) > 0 -> #{delta := Difference} = HE_link_spec, {ok, InTemp + (Difference/(Flow+?EPS))};
%         (_,    InTemp) when InTemp > 100 -> {ok,100};
%         (_,    InTemp) when InTemp < 0.1 -> {ok,0.1}
%      end}.