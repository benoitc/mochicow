-module(hello_cowboy).
-export([start/0, stop/0]).

-define(LOOP, {mochi_handler, loop}).

start() ->
    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
            {'_', [{[<<"mochi">>,'_'], mochi_handler, [{loop, ?LOOP}]},
                   {'_', cowboy_handler, []}]}
    ],
    application:start(cowboy),
    application:start(cowboy_revproxy),

    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).

stop() ->
    application:stop(cowboy).
