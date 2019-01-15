%Erlang code op github zetten en URL mailen naar valckenaers

-module(pipes).
-export([start/0]).
-export([createPipeTyp/0, createPipeInstance/1]).
-export([connectPipe/2, connectPipes/1]).
-export([createPipes/1, createPipes/3]).
-export([pumpCmd/1, flowCmd/0]).
-export([switchPump/1, pumpState/0, getFlow/0]).

start() ->
	survivor:start(),
	%PumpTyp = pumpTyp:create(),
	%FlowTyp = flowMeterTyp:create(),

	{ok, PumpRes} = resource_type:create(pumpTyp, []),
	{ok, FlowRes} = resource_type:create(flowMeterTyp, []),
	{ok, FluidRes} = resource_type:create(fluidumTyp, []),

	io:fwrite("PumpResPid: ~p~n", [PumpRes]),
	io:fwrite("FlowResPid: ~p~n", [FlowRes]),
	io:fwrite("FluidResPid: ~p~n", [FluidRes]),

	{ok, Pipes} = createPipes(12),
	printPipes(Pipes),

	{ok, PumpInstPid} = pumpInst:create(self(), PumpRes, lists:nth(1, Pipes), fun pumpCmd/1),
	{ok, FlowInstPid} = flowMeterInst:create(self(), FlowRes, lists:nth(6, Pipes), fun flowCmd/0),

	FlowMeterAdded = lists:sublist(Pipes,5) ++ [FlowInstPid] ++ lists:nthtail(6,Pipes),
	FullCircuit = [PumpInstPid] ++ lists:nthtail(1,FlowMeterAdded),

	connectPipes(FullCircuit),
	io:fwrite("Pipes connected~n"),
	{ok, [_, RootConn]} = resource_instance:list_connectors(lists:nth(1, FullCircuit)),

	{ok, FluidInstPid} = fluidumInst:create(RootConn, FluidRes),

	register(pumpInst, PumpInstPid),
	register(flowInst, FlowInstPid),
	register(fluidInst, FluidInstPid),
	
	io:fwrite("PumpInstPid: ~p~n", [PumpInstPid]),
	io:fwrite("FlowInstPid: ~p~n", [FlowInstPid]),
	io:fwrite("FluidInstPid: ~p~n", [FluidInstPid]),
	%survivor ! stop.
	ok.
	
createPipeTyp() -> 
	{ok, P} = resource_type:create(pipeTyp, []),
	P.
	
createPipeInstance(P) ->
	{ok, PI} = pipeInst:create(self(), P),
	PI.

connectPipe(Pipe0, Pipe1) ->
	{ok, [_, Conn0Out]} = resource_instance:list_connectors(Pipe0),
	{ok, [Conn1In, _]}  = resource_instance:list_connectors(Pipe1),
	io:fwrite("Connecting ~p to ~p~n", [Pipe0, Pipe1]),
	connector:connect(Conn0Out, Conn1In).

connectPipes([P0|[P1|List]]) ->
	connectPipe(P0, P1),
	connectPipes([P1|List], P0).
connectPipes([P0|[P1|List]], First) -> 
	connectPipe(P0, P1),
	connectPipes([P1|List], First);
connectPipes([Last], First) -> 
	connectPipe(Last, First),
	ok.

createPipes(N) -> createPipes([], createPipeTyp(), N).
createPipes(List, P, N) when N > 0 ->
	createPipes([createPipeInstance(P)|List], P, N-1);
createPipes(List, _, _) -> {ok, List}.

printPipes(List) -> printPipes(List, 1).
printPipes([H|List], N) ->
	io:format("Pijp ~p: ~p~n", [N, H]),
	printPipes(List, N+1);
printPipes([], _) -> ok.

pumpCmd(on) ->
	io:fwrite("Pump on~n"),
  	ok;

pumpCmd(off) ->
	io:fwrite("Pump off~n"),
  	ok.

%real world measurement
flowCmd() ->
	io:fwrite("Measuring flow~n"),
	0.

%functions used by cowboy
switchPump(on) ->
	pumpInst:switch_on(pumpInst);

switchPump(off) ->
	pumpInst:switch_off(pumpInst).

pumpState() ->
	{ok, On} = pumpInst:is_on(pumpInst),
	On.

getFlow() ->
	{ok, F} = flowMeterInst:estimate_flow(flowInst),
	F.