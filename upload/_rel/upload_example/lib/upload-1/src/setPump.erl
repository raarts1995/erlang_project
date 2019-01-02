%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(setPump).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{on := On} = cowboy_req:match_qs([{on, [], undefined}], Req0),
	io:fwrite("On: ~p~n", [On]),
	pump(On),
	Req = response(Method, pipes:pumpState(), Req0),
	{ok, Req, Opts}.

pump(<<"1">>) ->
	io:fwrite("Switching pump on~n"),
	pipes:switchPump(on);
pump(<<"0">>) ->
	io:fwrite("Switching pump off~n"),
	pipes:switchPump(off);
pump(_) ->
	io:fwrite("Switching pump... undefined request~n"),
	undef.

response(<<"GET">>, on, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, <<"1">>, Req);

response(<<"GET">>, off, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, <<"0">>, Req);

response(_, _, Req) ->
	cowboy_req:reply(405, Req).