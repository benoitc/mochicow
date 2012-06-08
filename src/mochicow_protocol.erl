%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.


-module(mochicow_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]).
-export([init/4]).

-export([loop/1]).
-export([after_response/2, reentry/1]).
-export([new_request/3, call_body/2]).

-include_lib("cowboy/include/http.hrl").
-include("mochicow.hrl").

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).


%% @doc Start a mochiweb process
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.


%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok | none().
init(ListenerPid, Socket, Transport, Opts) ->
    {loop, HttpLoop} = proplists:lookup(loop, Opts),
    ok = cowboy:accept_ack(ListenerPid),
    loop(#hstate{socket = Socket,
                 transport = Transport,
                 loop = HttpLoop}).

loop(#hstate{transport=Transport, socket=Socket}=State) ->
    ok = Transport:setopts(Socket, [{packet, http}]),
    request(State).

request(#hstate{transport=Transport, socket=Socket}=State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, {http_request, Method, Path, Version}}
               when Protocol == http orelse Protocol == ssl ->
            ok = Transport:setopts(Socket, [{packet, httph}]),
            headers(State, {Method, Path, Version}, [], 0);
        {Protocol, _, {http_error, "\r\n"}}
                when Protocol == http orelse Protocol == ssl ->
            request(State);
        {Protocol, _, {http_error, "\n"}}
                when Protocol == http orelse Protocol == ssl ->
            request(State);
        {tcp_closed, _} ->
            Transport:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            Transport:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(State)
    after ?REQUEST_RECV_TIMEOUT ->
        Transport:close(Socket),
        exit(normal)
    end.

headers(#hstate{transport=Transport, socket=Socket}=State, Request,
        Headers, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = Transport:setopts(Socket, [{packet, raw}]),
    handle_invalid_request(State, Request, Headers);
headers(#hstate{transport=Transport, socket=Socket, loop=Loop}=State, Request,
        Headers, HeaderCount) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, http_eoh}
                when Protocol == http orelse Protocol == ssl ->
            Req = new_request(State, Request, Headers),
            call_body(Loop, Req),
            ?MODULE:after_response(Loop, Req);
        {Protocol, _, {http_header, _, Name, _, Value}}
                when Protocol == http orelse Protocol == ssl ->
            headers(State, Request, [{Name, Value} | Headers],
                    1 + HeaderCount);
        {tcp_closed, _} ->
            Transport:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(State, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        Transport:close(Socket),
        exit(normal)
    end.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

-spec handle_invalid_request(term()) -> no_return().
handle_invalid_request(State) ->
    handle_invalid_request(State, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

-spec handle_invalid_request(term(), term(), term()) -> no_return().
handle_invalid_request(#hstate{transport=Transport, socket=Socket}=State,
                       Request, RevHeaders) ->
    Req = new_request(mochiweb_socket(State), Request, RevHeaders),
    Req:respond({400, [], []}),
    Transport:close(Socket),
    exit(normal).

new_request(#hstate{transport=Transport, socket=Socket}=State,
            Request, RevHeaders) ->
    Transport:setopts(Socket, [{packet, raw}]),
    mochiweb:new_request({mochiweb_socket(State), Request,
                          lists:reverse(RevHeaders)}).


reentry(Body) ->
    fun (Req) ->
            ?MODULE:after_response(Body, Req)
    end.

after_response(Body, Req) ->
    {Transport, Socket} = case Req:get(socket) of
        {ssl, S} ->
            {cowboy_ssl_transport, S};
        S ->
            {cowboy_tcp_transport, S}
    end,

    case Req:should_close() of
        true ->
            mochiweb_socket:close(Socket),
            exit(normal);
        false ->
            Req:cleanup(),
            erlang:garbage_collect(),
            ?MODULE:loop(#hstate{transport=Transport,
                                 socket=Socket,
                                 loop=Body})
    end.


mochiweb_socket(#hstate{transport=Transport, socket=Socket}) ->
    case Transport of
        cowboy_transport_ssl ->
            {ssl, Socket};
        _ ->
            Socket
    end.
