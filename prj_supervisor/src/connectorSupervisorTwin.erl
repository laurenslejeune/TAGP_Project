-module(connectorSupervisorTwin).
-behaviour(supervisor).

-export([create/1,init/1]).


create({Begin_N,End_N})->
    supervisor:start_link({local,?MODULE},?MODULE,[{Begin_N,End_N}]).

init([{Begin_N,End_N}])->
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},

    %Generate a list of all pipes for the given information
    io:format("Connecting the pipes~n"),
    Pipes = fluidumSupervisor:generateNPipesIds(Begin_N,End_N),
    buildSystem:connectPipesCircle(Pipes),

    {ok,{SupFlags,[]}}.
