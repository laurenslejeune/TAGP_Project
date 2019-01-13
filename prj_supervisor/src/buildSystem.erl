-module(buildSystem).
-export([start_3pipes/1, start_3pipes_water/1, start_3pipes_water_pump/1, stop/0]).
-export([start_3pipes_water_pump_flowmeter/1,start_3pipes_water_pump_flowmeter_heatex/1]).
-export([start_Npipes_circle/1]).
-export([connectPipesCircle/1, generateNpipes/3, getAllConnectors/1,fillPipesWithFluid/2]).
-export([generateRandomSystem/4,generateRandomSystem/5,generateDigitalTwin/1]).
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
	%io:format("Connectiong pipes ~p and ~p~n",[LastAdded,Pipe]),
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
	
start_3pipes(true) ->
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
	{ok, {PipeTypePID,Pipes,Connectors,Locations}};

start_3pipes(false) ->	
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

start_3pipes_water(true) ->
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
	{ok,FluidumType} = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid}};

start_3pipes_water(false) ->	
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
	{ok,FluidumType} = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid}}.

start_3pipes_water_pump(false) ->
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
	
	%Put water in the network
	{ok,FluidumType} = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%{Root_ConnectorPid, Circuit, ResTyp_Pid} = Fluid,
	
	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,PumpInst,PumpTypPID}};


start_3pipes_water_pump(true) ->
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
	
	connector:connect(P2C2,P3C1),
	connector:connect(P1C1,P3C2),
	connector:connect(P1C2,P2C1),

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
	
	%Put water in the network
	{ok,FluidumType} = fluidumTyp:create(),
	{ok, Fluid} = fluidumInst:create(P1C1,FluidumType),
	location:arrival(Location1,Fluid),
	location:arrival(Location2,Fluid),
	location:arrival(Location3,Fluid),

	%{Root_ConnectorPid, Circuit, ResTyp_Pid} = Fluid,
	
	%Output data sorting
	Pipes = [Pipe1InstPID,Pipe2InstPID,Pipe3InstPID],
	ConnectorsPipe1 = [P1C1,P1C2],
	ConnectorsPipe2 = [P2C1,P2C2],
	ConnectorsPipe3 = [P3C1,P3C2],
	Connectors = [ConnectorsPipe1,ConnectorsPipe2,ConnectorsPipe3],
	Locations = [Location1, Location2, Location3],
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,PumpInst,PumpTypPID}}.


start_3pipes_water_pump_flowmeter(true) ->
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
	{ok,FluidumType} = fluidumTyp:create(),
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
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}};

start_3pipes_water_pump_flowmeter(false) ->
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
	{ok,FluidumType} = fluidumTyp:create(),
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

start_3pipes_water_pump_flowmeter_heatex(true) ->
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
	{ok,FluidumType} = fluidumTyp:create(),
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
	{ok, {PipeTypePID,Pipes,Connectors,Locations,FluidumType,Fluid,Tasks}};

start_3pipes_water_pump_flowmeter_heatex(false) ->
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
	{ok,FluidumType} = fluidumTyp:create(),
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

generateRandomSystem(N_pipes,N_pumps,N_he,true)->
	survivor:start(),
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	%First generate the N required pipes
	Pipes = generateNpipes(N_pipes,[],PipeTypePID),
	%io:format("Generated list of pipes ~p~n",[Pipes]),
	%Next, connect those pipes
	%The pipes will be connected in one, large circle
	ok = connectPipesCircle(Pipes),

	%We then put water in the network (in all the pipes)
	{ok,FluidumType} = fluidumTyp:create(),
	
	[RandomPipe|_] = Pipes,
	{ok,[Connector,_]} = resource_instance:list_connectors(RandomPipe),
	{ok, Fluid} = fluidumInst:create(Connector,FluidumType),
	fillPipesWithFluid(Pipes,Fluid),

	%Next, the required pumps are generated
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create the actual pumps:
	Pumps = generateNPumps(N_pumps,PumpTypPID,Fun,Pipes),
	%io:format("Generated list of pumps ~p~n",[Pumps]),
	%We create a single flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	%Select a random pipe to put the flowmeter on:
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	%io:format("Taking random pipe, index = ~p,",[RandomIndex]),
	RandomPipeInst = lists:nth(RandomIndex,Pipes),
	%io:format(", pid =  ~p~n",[RandomPipeInst]),
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,RandomPipeInst, FlowMeterCMD),

	%Finally, generate the N_he heat exchangers
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	HeatExchangers = generateNHeatExchangers(N_he,HeatExTyp,Pipes),
	{Pipes,Pumps,FlowMeterInst,HeatExchangers};

generateRandomSystem(N_pipes,N_pumps,N_he,false)->
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	%First generate the N required pipes
	Pipes = generateNpipes(N_pipes,[],PipeTypePID),
	
	%Next, connect those pipes
	%The pipes will be connected in one, large circle
	ok = connectPipesCircle(Pipes),

	%We then put water in the network (in all the pipes)
	{ok,FluidumType} = fluidumTyp:create(),
	
	[RandomPipe|_] = Pipes,
	{ok,[Connector,_]} = resource_instance:list_connectors(RandomPipe),
	{ok, Fluid} = fluidumInst:create(Connector,FluidumType),
	fillPipesWithFluid(Pipes,Fluid),

	%Next, the required pumps are generated
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create the actual pumps:
	Pumps = generateNPumps(N_pumps,PumpTypPID,Fun,Pipes),
	%io:format("Generated list of pumps ~p~n",[Pumps]),
	%We create a single flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	%Select a random pipe to put the flowmeter on:
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe2 = lists:nth(RandomIndex,Pipes),
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,RandomPipe2, FlowMeterCMD),

	%Finally, generate the N_he heat exchangers
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	HeatExchangers = generateNHeatExchangers(N_he,HeatExTyp,Pipes),
	{Pipes,Pumps,FlowMeterInst,HeatExchangers}.

generateRandomSystem(N_pipes,N_pumps,N_he,false,DifList)->
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	%First generate the N required pipes
	Pipes = generateNpipes(N_pipes,[],PipeTypePID),
	
	%Next, connect those pipes
	%The pipes will be connected in one, large circle
	ok = connectPipesCircle(Pipes),

	%We then put water in the network (in all the pipes)
	{ok,FluidumType} = fluidumTyp:create(),
	
	[RandomPipe|_] = Pipes,
	{ok,[Connector,_]} = resource_instance:list_connectors(RandomPipe),
	{ok, Fluid} = fluidumInst:create(Connector,FluidumType),
	fillPipesWithFluid(Pipes,Fluid),

	%Next, the required pumps are generated
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create the actual pumps:
	Pumps = generateNPumps(N_pumps,PumpTypPID,Fun,Pipes),
	%io:format("Generated list of pumps ~p~n",[Pumps]),
	%We create a single flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	%Select a random pipe to put the flowmeter on:
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe2 = lists:nth(RandomIndex,Pipes),
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,RandomPipe2, FlowMeterCMD),

	%Finally, generate the N_he heat exchangers
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	HeatExchangers = generateNHeatExchangersWithDifList(N_he,HeatExTyp,Pipes,DifList),
	{Pipes,Pumps,FlowMeterInst,HeatExchangers};

generateRandomSystem(N_pipes,N_pumps,N_he,true,DifList)->
	survivor:start(),

	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	%First generate the N required pipes
	Pipes = generateNpipes(N_pipes,[],PipeTypePID),
	
	%Next, connect those pipes
	%The pipes will be connected in one, large circle
	ok = connectPipesCircle(Pipes),

	%We then put water in the network (in all the pipes)
	{ok,FluidumType} = fluidumTyp:create(),
	
	[RandomPipe|_] = Pipes,
	{ok,[Connector,_]} = resource_instance:list_connectors(RandomPipe),
	{ok, Fluid} = fluidumInst:create(Connector,FluidumType),
	fillPipesWithFluid(Pipes,Fluid),

	%Next, the required pumps are generated
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	%Instantiate the real world command function
	Fun = fun(on) ->
			{ok,on};
			(off)->
			{ok,off}
		end,
	%Now create the actual pumps:
	Pumps = generateNPumps(N_pumps,PumpTypPID,Fun,Pipes),
	%io:format("Generated list of pumps ~p~n",[Pumps]),
	%We create a single flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,real_flow}
				end,
	%Select a random pipe to put the flowmeter on:
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe2 = lists:nth(RandomIndex,Pipes),
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,RandomPipe2, FlowMeterCMD),

	%Finally, generate the N_he heat exchangers
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	HeatExchangers = generateNHeatExchangersWithDifList(N_he,HeatExTyp,Pipes,DifList),
	{Pipes,Pumps,FlowMeterInst,HeatExchangers}.

generateDigitalTwin({RefPipes,RefPumps,RefFlowMeter,RefHeatExchangers,RefGetSystemFlowPid,DifList})->
	%In order to accurately represent the system, the structure needs to exactly the same as the given on:

	%First, generate lists with pumps/heat exhangers on the same index as the pipe they represent:
	%TODO: no functions to get the pipe a pump/HE is built on

	%We now generate an equivalent system:
	{ok,PipeTypePID} = resource_type:create(pipeTyp,[]),
	%First generate the N required pipes
	Pipes = generateNpipes(length(RefPipes),[],PipeTypePID),
	
	%Next, connect those pipes
	%The pipes will be connected in one, large circle
	ok = connectPipesCircle(Pipes),

	%We then put water in the network (in all the pipes)
	{ok,FluidumType} = fluidumTyp:create(),
	
	[RandomPipe|_] = Pipes,
	{ok,[Connector,_]} = resource_instance:list_connectors(RandomPipe),
	{ok, Fluid} = fluidumInst:create(Connector,FluidumType),
	fillPipesWithFluid(Pipes,Fluid),

	%Next, the required pumps are generated
	%Create a pump type
	{ok,PumpTypPID} = pumpTyp:create(),
	
	%Now create the actual pumps:
	Pumps = generateNPumpsDigitalTwin(length(RefPumps),PumpTypPID,RefPumps,Pipes),
	%io:format("Generated list of pumps ~p~n",[Pumps]),
	%We create a single flowmeter
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	FlowMeterCMD = fun()->
					{ok,{N,Flow}} = systemFlow:getSystemFlow(RefGetSystemFlowPid),
					{N,Flow}
				end,
	%Select a random pipe to put the flowmeter on:
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe2 = lists:nth(RandomIndex,Pipes),
	{ok, FlowMeterInst} = flowMeterInst:create(self(),FlowMeterTyp,RandomPipe2, FlowMeterCMD),

	%Finally, generate the N_he heat exchangers
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	HeatExchangers = generateNHeatExchangersWithDifList(length(RefHeatExchangers),HeatExTyp,Pipes,DifList),
	{Pipes,Pumps,FlowMeterInst,HeatExchangers}.


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

fillPipesWithFluid([Pipe|OtherPipes],Fluid)->
	{ok, [Location]} = resource_instance:list_locations(Pipe),
	location:arrival(Location,Fluid),
	fillPipesWithFluid(OtherPipes,Fluid);

fillPipesWithFluid([],_)->
	ok.

generateNPumps(N,PumpTypPID,Fun,Pipes)->
	generateNPumps(N,PumpTypPID,Fun,[],Pipes).

generateNPumps(0,_,_,Pumps,_)->
	Pumps;

generateNPumps(N,PumpTypPID,Fun,Pumps,Pipes)->
	%Select a random pipe from the list of pipes
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),
	
	%Create pump on this randomly selected pipe
	{ok,PumpInst} = pumpInst:create(self(), PumpTypPID, RandomPipe, Fun),
	
	%Remove the pipe from the list, so it cannot be chosen again
	lists:delete(RandomPipe,Pipes),
	generateNPumps(N-1,PumpTypPID,Fun,Pumps++[PumpInst],Pipes).

generateNPumpsDigitalTwin(N,PumpTypPID,RefPumps,Pipes)->
	generateNPumpsDigitalTwin(N,PumpTypPID,RefPumps,[],Pipes).

generateNPumpsDigitalTwin(0,_,_,Pumps,_)->
	Pumps;

generateNPumpsDigitalTwin(N,PumpTypPID,[RefPump|RefPumps],Pumps,Pipes)->
	%Select a random pipe from the list of pipes
	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),
	
	%Connect the pump to create with it digital twin
	Fun = fun(on) ->
			pumpInst:switch_on(RefPump),
			{ok,on};
			(off)->
			pumpInst:switch_off(RefPump),
			{ok,off}
		end,
	%Create pump on this randomly selected pipe
	{ok,PumpInst} = pumpInst:create(self(), PumpTypPID, RandomPipe, Fun),
	
	%Remove the pipe from the list, so it cannot be chosen again
	lists:delete(RandomPipe,Pipes),
	generateNPumpsDigitalTwin(N-1,PumpTypPID,RefPumps,Pumps++[PumpInst],Pipes).

generateNHeatExchangersWithDifList(N_he,HeatExTyp,Pipes,DifList)->
	generateNHeatExchangersWithDifList(N_he,HeatExTyp,Pipes,DifList,[]).

generateNHeatExchangersWithDifList(0,_,_,_,HE) ->
	HE;

generateNHeatExchangersWithDifList(N_he,HeatExTyp,Pipes,[Dif|DifList],HE)->
	HE_link_spec = #{delta => Dif},

	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

	{ok, HeatEx} = heatExchangerInst:create(self(), HeatExTyp, RandomPipe, HE_link_spec),

	lists:delete(RandomPipe,Pipes),
	generateNHeatExchangersWithDifList(N_he-1,HeatExTyp,Pipes,DifList,HE++[HeatEx]).



generateNHeatExchangers(N,HeatExTyp,Pipes)->
	generateNHeatExchangers(N,HeatExTyp,Pipes,[]).

generateNHeatExchangers(0,_,_,HE)->
	HE;

generateNHeatExchangers(N,HeatExTyp,Pipes,HE)->
	Difference = (1.0 - rand:uniform()), %rand:uniform() generates a number X: 0.0<=X<1
	HE_link_spec = #{delta => Difference},

	RandomIndex = rand:uniform(length(Pipes)), %Provides an integer between 1 and length(Pipes)
	RandomPipe = lists:nth(RandomIndex,Pipes),

	{ok, HeatEx} = heatExchangerInst:create(self(), HeatExTyp, RandomPipe, HE_link_spec),

	lists:delete(RandomPipe,Pipes),
	generateNHeatExchangers(N-1,HeatExTyp,Pipes,HE++[HeatEx]).