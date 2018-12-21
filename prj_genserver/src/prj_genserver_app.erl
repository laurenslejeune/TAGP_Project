%%%-------------------------------------------------------------------
%% @doc prj_genserver public API
%% @end
%%%-------------------------------------------------------------------

-module(prj_genserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    prj_genserver_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
