-module(digitalTwin1).

-export([create_system/3]).

create_system(N_pipes, N_pumps, N_Hex) when (N_pumps+N_Hex) > N_pipes->
    {error,too_many_pums_and_hex};

create_system(N_pipes, N_pumps, N_Hex) ->
    %Generate N 
    %Pipes = buildSystem:generateNpies
    {error,too_many_pums_and_hex}.