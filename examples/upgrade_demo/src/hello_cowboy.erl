-module(hello_cowboy).
-export([start/0, stop/0]).

-define(LOOP, {mochi_handler, loop}).

start() ->
    {ok, _} = application:ensure_all_started(cowboy),
     Dispatch = cowboy_router:compile([

        {'_', [{"/", cowboy_hello_handler, []},
         	   {"/echo", mochi_echo_handler, [{loop, {mochi_echo_handler, loop}}]},
        	   {"/keepalive/[...]", mochi_keepalive_handler, [{loop, {mochi_keepalive_handler, loop}}]},

        	   {'_', mochi_hello_handler, [{loop, {mochi_hello_handler, loop}}]}
        	   ]}
    ]),

    cowboy:start_http(http, 100,  [{port, 8080}], 
                      [{env, [{dispatch, Dispatch}]} ]).

stop() ->
    application:stop(cowboy).
