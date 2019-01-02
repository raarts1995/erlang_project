%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(getFlow).

-export([init/2]).

init(Req0, Opts) ->
	io:fwrite("flow requested~n"),
	Flow = pipes:getFlow(),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, Flow, Req0),
	{ok, Req, Opts}.
