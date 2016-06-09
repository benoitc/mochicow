%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.


-module(mochicow_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([remove_connection/1]).
-export([init/4]).

-export([mochiweb_socket/2]).
-export([ensure_started/1]).

%% @doc Start a mochiweb process
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    ok = ensure_started(mochiweb_clock),
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

%% @doc remove the connection
remove_connection(MochiReq) ->
    Ref = proplists:get_value(ranch_ref, MochiReq:get(opts)),
    ranch:remove_connection(Ref).


%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok | none().
init(Ref, Socket, Transport, Opts) ->
    {loop, HttpLoop} = proplists:lookup(loop, Opts),
    ok = ranch:accept_ack(Ref),
    mochiweb_http:loop(mochiweb_socket(Transport, Socket), [{ranch_ref, Ref} |Opts], HttpLoop).


mochiweb_socket(ranch_ssl, Socket) -> {ssl, Socket};
mochiweb_socket(_Transport, Socket) -> Socket.

ensure_started(M) ->
    case M:start() of  
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.