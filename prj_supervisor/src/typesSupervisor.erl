-module(typesSupervisor).
-behaviour(supervisor).
-export([create/0,init/1]).
-export([getChildren/0]).
create()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([])->
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},
    PipeTyp = #{id => pipetyp_child,
                start => {pipeTyp, createRegister, []},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => [pipeTyp]},
    PipeInst = #{id => pipeinst_child0,
                start => {pipeInst, create, [self(),pipeTyp,0]},
                modules => [pipeInst]},
    ChildSpecs = [PipeTyp,PipeInst],
    {ok, {SupFlags, ChildSpecs}}.

                % #{id => pipeInst,
                % start => {pipeinst, create, [self(),pipetyp]},
                % modules => [pipeInst]}

getChildren()->
    supervisor:which_children(?MODULE).