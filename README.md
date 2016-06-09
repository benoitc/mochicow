# mochicow

mochicow is a mochiweb adapter for [cowboy](http://github.com/extend/cowboy).

There are 2 ways to use mochicow:

- as a ranch protocol: It will use the socket acceptor pool of ranch
  instead of the mochiweb one.

- as a protocol upgrade. Like websockets you can upgrade a cowboy
  handler to use a mochiweb loop. It allows you to use both your new
code with cowboy and old code with mochiweb.


## Use the ranch socket pool with mochiweb

To use mochiweb with the ranch socketpool, you just need to use the
`mochicow_protocol` module as the prococol when you start a cowboy
listener. You pass the mochiweb `lopp` in the protocol options via the
`loop` property.

Ex:

```erlang
-module(hello).

-export([start/0, stop/0, loop/1]).
-define(LOOP, {?MODULE, loop}).


start() ->
    {ok, _} = application:ensure_all_started(ranch),
    ranch:start_listener(http, 100,
                          ranch_tcp, [{port, 8000}],
                          mochicow_protocol, [{loop, ?LOOP}]).

stop() ->
  application:stop(ranch).


loop(Req) ->
    Path = Req:get(path),
    Resource = case string:str(Path, "?") of
        0 -> Path;
        N -> string:substr(Path, 1, length(Path) - (N + 1))
    end,
    handle_request(Resource, Req).


handle_request("/hello", Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}], <<"Hello to you as well">>});

handle_request(Path, Req) ->
    Get = Req:parse_qs(),
    Post = Req:parse_post(),
    User_agent = Req:get_header_value("user-agent"),
    erlang:display({get, Get}),
    erlang:display({post, Post}),
    erlang:display({user_agent, User_agent}),
    erlang:display({path, Path}),
    Req:respond({200, [{"Content-Type", "text/html"}], <<"Hello World!">>}).
```


## Upgrade the protocol

You can use mochicow to quietly migrate your code from mochiweb to
cowboy or use both at the sametime. To do that you will need to use the
upgrqae the protocol using `mochicow_upgrade` as the protocol and compile 
your code and the following option added to the erlang compiler flags:

```erlang
{parse_transform, mochicow}
```

Alternately, you can add it to the module you wish to use with mochiweb:

```erlang
-compile([{parse_transform, mochicow}]).
```

> the mochicow parse_transform will replace at compilation any call to mochiweb_request by calls to mochicow_request. 



### Ex to start the cowboy_http_protocol:

```erlang
-module(hello_cowboy).
-export([start/0, stop/0]).

-define(LOOP, {mochi_handler, loop}).

start() ->
    {ok, _} = application:ensure_all_started(cowboy),
     Dispatch = cowboy_router:compile([

        {'_', [
               {'_', mochi_hello_handler, [{loop, {mochi_hello_handler, loop}}]}
             ]}
    ]),

    cowboy:start_http(http, 100,  [{port, 8080}], 
                      [{env, [{dispatch, Dispatch}]} ]).

stop() ->
    application:stop(cowboy).
```


### The mochiweb handler:

```erlang
-module(mochi_hello_handler).
-export([init/3, loop/1]).

init(_, _, _) ->
    {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}],
                 <<"Hello from mochiweb">>}).
```


See more usage examples in the `examples` for the usage.
