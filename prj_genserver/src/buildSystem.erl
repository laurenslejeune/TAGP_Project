-module(buildSystem).
-export([start_3pipes/0, start_3pipes_water/0, start_3pipes_water_pump/0, stop/0]).
-export([start_3pipes_water_pump_flowmeter/0,start_3pipes_water_pump_flowmeter_heatex/0]).
-export([start_Npipes_circle/1]).
-export([connectPipesCircle/1, generateNpipes/3, getAllConnectors/1]).
-include_lib("eunit/include/eunit.hrl").


start_Npipes_circle(N) ->
	%This module strives to:
	%1) Create N pipe instances
	%2) Create a network containing all N pipes, connecting them in a circle
	survivor:start(),
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	if N >1 ->
		Pipes = generateNpipes(N,[], PipeTypePID),
		ok = connectPipesCircle(Pipes),
		{n_connected_circle,N,Pipes};
	true ->
		{error,"N was smaller than 1"}
	end.
	
connectPipesCircle([PipeRoot|OtherPipes]) ->
	connectPipesCircle(PipeRoot,PipeRoot,OtherPipes).
	
connectPipesCircle(Root, LastAdded, [Pipe|OtherPipes]) ->
	{ok,[_P1C1,P1C2]} = resource_instance:list_connectors(LastAdded),
	{ok,[P2C1,_P2C2]} = resource_instance:list_connectors(Pipe),
	connector:connect(P1C2,P2C1),
	connectPipesCircle(Root, Pipe, OtherPipes);
	
connectPipesCircle(Root, LastAdded, []) ->
	{ok,[P1C1,_P1C2]} = resource_instance:list_connectors(Root),
	{ok,[_P2C1,P2C2]} = resource_instance:list_connectors(LastAdded),
	connector:connect(P1C1,P2C2),
	ok.
	
generateNpipes(N,[], _PipeTypePID) when N =< 0 ->
	[];
	
generateNpipes(1, List, PipeTypePID) ->
	{ok,PipeInstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	List ++[PipeInstPID]; 

generateNpipes(N, List, PipeTypePID) ->
	{ok,PipeInstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	NewList = List ++[PipeInstPID],
	generateNpipes(N-1, NewList, PipeTypePID).
	
start_3pipes() ->
	survivor:start(),
	
	%systemSupervisor:start_link(),
	
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	{ok,Pipe1InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe2InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe3InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,[P1C1,P1C2]} = resource_instance:list_connectors(Pipe1InstPID),
	{ok,[P2C1,P2C2]} = resource_instance:list_connectors(Pipe2InstPID),
	{ok,[P3C1,P3C2]} = resource_instance:list_connectors(Pipe3InstPID),
	

	{ok,[Location1]} = resource_instance:list_locations(Pipe1InstPID),
	{ok,[Location2]} = resource_instance:list_locations(Pipe2InstPID),
	{ok,[Location3]} = resource_instance:list_locations(Pipe3InstPID),
	
	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),

	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations}}.

start_3pipes_water() ->
	survivor:start(),
	
	%Create a pyramid network with 3 pipes
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	{ok,Pipe1InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe2InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe3InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,[P1C1,P1C2]} = resource_instance:list_connectors(Pipe1InstPID),
	{ok,[P2C1,P2C2]} = resource_instance:list_connectors(Pipe2InstPID),
	{ok,[P3C1,P3C2]} = resource_instance:list_connectors(Pipe3InstPID),
	

	{ok,[Location1]} = resource_instance:list_locations(Pipe1InstPID),
	{ok,[Location2]} = resource_instance:list_locations(Pipe2InstPID),
	{ok,[Location3]} = resource_instance:list_locations(Pipe3InstPID),
	
	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),
	
	%Put water in the network
	FluidumType = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	%{Root_ConnectorPid, Circuit, ResTyp_Pid} = Fluid,
	
	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid}}.	

start_3pipes_water_pump() ->
	survivor:start(),
	%?debugFmt("Started survivor for 3pipes water pump~n",[]),
	
	%Create a pyramid network with 3 pipes
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	{ok,Pipe1InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe2InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe3InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,[P1C1,P1C2]} = resource_instance:list_connectors(Pipe1InstPID),
	{ok,[P2C1,P2C2]} = resource_instance:list_connectors(Pipe2InstPID),
	{ok,[P3C1,P3C2]} = resource_instance:list_connectors(Pipe3InstPID),
	
	{ok,[Location1]} = resource_instance:list_locations(Pipe1InstPID),
	{ok,[Location2]} = resource_instance:list_locations(Pipe2InstPID),
	{ok,[Location3]} = resource_instance:list_locations(Pipe3InstPID),
	
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create an actual pump
	{ok,PumpInst} = pumpInst:create(self(), PumpTypPID, Pipe1InstPID, Fun),
	%This pump is created on top of an existing pipe! That means the underlying
	%pipe still needs to be connected to other pipes

	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),
	
	%Put water in the network
	FluidumType = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	%{Root_ConnectorPid, Circuit, ResTyp_Pid} = Fluid,
	
	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,PumpInst,PumpTypPID}}.

start_3pipes_water_pump_flowmeter() ->
	survivor:start(),
	%?debugFmt("Started survivor for 3pipes water pump~n",[]),
	
	%Create a pyramid network with 3 pipes
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	{ok,Pipe1InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe2InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe3InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,[P1C1,P1C2]} = resource_instance:list_connectors(Pipe1InstPID),
	{ok,[P2C1,P2C2]} = resource_instance:list_connectors(Pipe2InstPID),
	{ok,[P3C1,P3C2]} = resource_instance:list_connectors(Pipe3InstPID),
	
	{ok,[Location1]} = resource_instance:list_locations(Pipe1InstPID),
	{ok,[Location2]} = resource_instance:list_locations(Pipe2InstPID),
	{ok,[Location3]} = resource_instance:list_locations(Pipe3InstPID),
	
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create an actual pump
	{ok,PumpInst} = pumpInst:create(self(), PumpTypPID, Pipe1InstPID, Fun),
	%This pump is created on top of an existing pipe! That means the underlying
	%pipe still needs to be connected to other pipes

	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),
	
	%Put water in the network
	FluidumType = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%Create a flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,Pipe2InstPID, FlowMeterCMD),
	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	Tasks = [PumpInst,PumpTypPID,FlowMeterInst,FlowMeterTyp],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}}.

start_3pipes_water_pump_flowmeter_heatex() ->
	survivor:start(),
	%?debugFmt("Started survivor for 3pipes water pump~n",[]),
	
	%Create a pyramid network with 3 pipes
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	{ok,Pipe1InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe2InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,Pipe3InstPID} = resource_instance:create(pipeInst,[self(),PipeTypePID]),
	{ok,[P1C1,P1C2]} = resource_instance:list_connectors(Pipe1InstPID),
	{ok,[P2C1,P2C2]} = resource_instance:list_connectors(Pipe2InstPID),
	{ok,[P3C1,P3C2]} = resource_instance:list_connectors(Pipe3InstPID),
	
	{ok,[Location1]} = resource_instance:list_locations(Pipe1InstPID),
	{ok,[Location2]} = resource_instance:list_locations(Pipe2InstPID),
	{ok,[Location3]} = resource_instance:list_locations(Pipe3InstPID),
	
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create an actual pump
	{ok,PumpInst} = pumpInst:create(self(), PumpTypPID, Pipe1InstPID, Fun),
	%This pump is created on top of an existing pipe! That means the underlying
	%pipe still needs to be connected to other pipes

	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),
	
	%Put water in the network
	FluidumType = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%Create a flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,Pipe2InstPID, FlowMeterCMD),
	
	%Create heat exchanger
	%?debugFmt("Creating heat exchanger~n",[]),
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	%?debugFmt("Heat Exchanger Type ~p~n",[HeatExTyp]),
	Difference = 0.9,
	HE_link_spec = #{delta => Difference},
	{ok, HeatEx} = heatExchangerInst:create(self(), HeatExTyp, Pipe3InstPID, HE_link_spec),
	%?debugFmt("Heat Exchanger Inst ~p~n",[HeatEx]),


	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	Tasks = [PumpInst,PumpTypPID,FlowMeterInst,FlowMeterTyp,HeatExTyp,HeatEx],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}}.

stop() ->
	survivor ! stop,
	%gen_server:call(resource_instance,stop),
	%pipeTyp:stop(),
	{ok, stopped}.
	
getAllConnectors(Pipes) ->
	getAllConnectors(Pipes,[]).
	
getAllConnectors([Pipe|OtherPipes],Connectors) ->
	{ok,Cs} = resource_instance:list_connectors(Pipe),
	getAllConnectors(OtherPipes,Connectors++Cs);
	
getAllConnectors([],Connectors) ->
	Connectors.