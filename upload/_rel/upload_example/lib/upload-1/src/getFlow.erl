%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(getFlow).

-export([init/2]).

init(Req0, Opts) ->
	io:fwrite("flow requested~n"),
	Flow = pipes:getFlow(),
	io:fwrite("Flow = ~p~n", [Flow]),
	FlowText = float_to_list(Flow, [{decimals, 3}]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, FlowText, Req0),
	{ok, Req, Opts}.
