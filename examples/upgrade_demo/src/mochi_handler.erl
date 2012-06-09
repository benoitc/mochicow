-module(mochi_handler).


-export([init/3, loop/1]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}],
                 <<"Hello from mochiweb">>}).
