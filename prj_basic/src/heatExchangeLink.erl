-module(heatExchangeLink).
-export([get_temp_influence/1]).
-include_lib("eunit/include/eunit.hrl").
% dummy module - replace by realistic model
% e.g. the outgoing temperature provided by get_temp_influence
% will saturate when approching the inTemp on the other side
% of the link. 


get_temp_influence(HE_link_spec) ->
    %Fun = fun(Flow, InTemp) -> 
    %    #{delta := Difference} = HE_link_spec, 
    %    {ok, InTemp + (Difference/Flow)} 
    %end,
    %Result = Fun(1,2),
    %?debugFmt("Test the created function~p~n",[Result]),
    %{ok,Fun}.
{ok, fun(Flow, InTemp) -> #{delta := Difference} = HE_link_spec, {ok, InTemp + (Difference/Flow)} end}.