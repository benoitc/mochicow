-module(mochi_hello_handler).


-export([init/3, loop/1]).


init(_, _, _) ->
    {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}],
                 <<"Hello from mochiweb">>}).
