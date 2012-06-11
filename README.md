# mochicow

mochicow is a mochiweb adapter for [cowboy](http://github.com/extend/cowboy).

There are 2 ways to use mochicow:

- as a cowboy protocol: It will use the socket acceptor pool of cowboy
  instead of the mochiweb one.

- as a protocol upgrade. Like websockets you can upgrade a cowboy
  handler to use a mochiweb loop. It allows you to use both your new
code with cowboy and old code with mochiweb.


## Use the cowboy socket pool with mochiweb

To use mochiweb with the cowboy socketpool, you just need to use the
`mochicow_protocol` module as the prococol when you start a cowboy
listener. You pass the mochiweb `lopp` in the protocol options via the
`loop` property.

Ex:

    -module(hello).
    -export([start/0, stop/0, loop/1]).

    -define(LOOP, {?MODULE, loop}).


    start() ->
        application:start(cowboy),
        cowboy:start_listener(http, 100,
                              cowboy_tcp_transport, [{port, 8000}],
                              mochicow_protocol, [{loop, ?LOOP}]).

    stop() ->
        application:stop(cowboy).


    loop(Req) ->
        Req:respond({200, [{"Content-Type", "text/html"}],
                     <<"Hello world">>});.


## Upgrade the protocol

You can use mochicow to quietly migrate your code from mochiweb to
cowboy or use both ath the sametime. To do that you will need to use the
upgrqde the protocol using `mochicow_upgrade` as the protocol. This
handler that you upgrade need to have the `loop` property to use
a mochiweb loop.

### Ex to start the cowboy_http_protocol:

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


### The mochiweb handler:


    -module(mochi_handler).
    -export([init/3, loop/1]).

    init({tcp, http}, _Req, _Opts) ->
        {upgrade, protocol, mochicow_upgrade}.

    loop(Req) ->
        Req:respond({200, [{"Content-Type", "text/html"}],
                     <<"Hello from mochiweb">>}).



See more usage examples in the `examples` for the usage.
