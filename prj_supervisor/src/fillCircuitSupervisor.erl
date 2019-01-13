-module(fillCircuitSupervisor).
-behaviour(supervisor).

-export([create/3,init/1]).

create(N_Begin,N_End,FluidumInst)->
    supervisor:start_link({local,?MODULE},?MODULE,[N_Begin,N_End,FluidumInst]).

init([N_Begin,N_End,FluidumInst]) ->
    
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},
    io:format("Filling the pipes with fluid ~p~n",[FluidumInst]),
    Pipes = fluidumSupervisor:generateNPipesIds(N_Begin,N_End),
    buildSystem:fillPipesWithFluid(Pipes,FluidumInst),
    {ok,{SupFlags,[]}}.

