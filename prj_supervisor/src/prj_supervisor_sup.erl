%%%-------------------------------------------------------------------
%% @doc prj_supervisor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(prj_supervisor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    survivor:start(),
    Double = false,
    N_pipes = 7,
    N_pumps = 6,
    N_hex = 3,
    SystemSupervisorSpec = #{id => systemsupervisor_child,
                        start => {systemSupervisor, create, [N_pipes,N_pumps,N_hex,Double]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [systemSupervisor]},
    SingleSystemControllerSpec = #{id => singlesystemcontroller_child,
                                start => {singleSystemController, create, [N_pipes,N_pumps,N_hex,not Double]},
                                restart => permanent,
                                shutdown => brutal_kill,
                                type => worker,
                                modules => [singleSystemController]},
    DigitalTwinControllerSpec = #{id => digitaltwincontroller_child,
                                start => {digitalTwinController, create, [N_pipes,N_pumps,N_hex,Double]},
                                restart => permanent,
                                shutdown => brutal_kill,
                                type => worker,
                                modules => [digitalTwinController]},
    ChildSpecs = [SystemSupervisorSpec,SingleSystemControllerSpec,DigitalTwinControllerSpec],
    {ok, {{one_for_all, 2, 1}, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
