-module(xaptum_client_stats_http).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-include("dds.hrl").

-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% Respond only to / 
handle('GET',[], _Req) ->
    Priv = xaptum_client_app:priv_dir(),
    IF = filename:join(Priv, "index.html"),
    {ok, File} = file:read_file(IF),
    {ok, [], File};

handle('GET', [<<"stat">>], _Req) ->
    {T, S, R} = get_stat(),
    L = io_lib:format("{ \"type\": \"~s\", \"sent\": ~p, \"received\": ~p }", [T,S,R]),
    B = iolist_to_binary(L),
    {ok, [], B};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.


get_stat() ->
    case whereis(?DEVICE) of
	undefined ->
	    case whereis(?SUBSCRIBER) of
		undefined ->
		    {<<"">>, 0, 0};
		Pid ->
		    xaptum_client:get_message_count(Pid)
	    end;
	Pid ->
	    xaptum_client:get_message_count(Pid)
    end.
