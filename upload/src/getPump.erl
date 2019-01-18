%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(getPump).

-export([init/2]).

init(Req0, Opts) ->
	io:fwrite("pump state requested~n"),
	{ok, PumpState} = pipes:pumpState(),
	PumpStateText = integer_to_list(PumpState),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, PumpStateText, Req0),
	{ok, Req, Opts}.
