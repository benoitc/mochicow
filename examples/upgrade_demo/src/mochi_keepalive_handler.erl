-module(mochi_keepalive_handler).

-export([init/3, loop/1]).

%% internal export (so hibernate can reach it)
-export([resume/2]).

-define(LOOP, {?MODULE, loop}).


init(_, _, _) ->
    {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
    Path = Req:get(path),
    case lists:prefix("/keepalive", Path) of
        true ->
            %% the "reentry" is a continuation -- what @mochiweb_http@
            %% needs to do to start its loop back at the top
            Reentry = mochiweb_http:reentry(?LOOP),

            %% here we could send a message to some other process and hope
            %% to get an interesting message back after a while.  for
            %% simplicity let's just send ourselves a message after a few
            %% seconds
            erlang:send_after(2000, self(), "honk honk"),

            %% since we expect to wait for a long time before getting a
            %% reply, let's hibernate.  memory usage will be minimized, so
            %% we won't be wasting memory just sitting in a @receive@
            proc_lib:hibernate(?MODULE, resume, [Req,  Reentry]),

            %% we'll never reach this point, and this function @loop/1@
            %% won't ever return control to @mochiweb_http@.  luckily
            %% @resume/3@ will take care of that.
            io:format("not gonna happen~n", []);

        _Other ->
            ok(Req, io_lib:format("some other page: ~p", [Path]))
    end,

    io:format("restarting loop normally in ~p~n", [Path]),
    ok.

%% this is the function that's called when a message arrives.
resume(Req, Reentry) ->
    receive
        Msg ->
            Text = io_lib:format("wake up message: ~p~n", [Msg]),
            ok(Req, Text)
    end,

    %% if we didn't call @Reentry@ here then the function would finish and the
    %% process would exit.  calling @Reentry@ takes care of returning control
    %% to @mochiweb_http@
    io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).