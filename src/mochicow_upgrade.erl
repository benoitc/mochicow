%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.
%%%
-module(mochicow_upgrade).

-export([upgrade/4]).

-include_lib("cowboy/include/http.hrl").

upgrade(_ListenerPid, _Handler, Opts, Req) ->
    {loop, HttpLoop} = proplists:lookup(loop, Opts),
    #http_req{socket=Socket,
              transport=Transport,
              method=Method,
              version=Version,
              raw_path=Path,
              raw_qs=QS,
              headers=Headers,
              raw_host=Host,
              port=Port} = Req,

    MochiSocket = mochiweb_socket(Transport, Socket),
    DefaultPort = default_port(Transport:name()),
    MochiHost = case Port of
        DefaultPort ->
            Port;
        _ ->
            %% fix raw host
            binary_to_list(Host) ++ ":" ++ integer_to_list(Port)
    end,

    MochiHeaders = lists:foldl(fun
                ({'Host'=K, _V}, T) ->
                    mochiweb_headers:insert(K, MochiHost, T);
                ({K, V}, T) when is_binary(K) ->
                    mochiweb_headers:insert(binary_to_list(K),
                                         binary_to_list(V), T);
                ({K, V}, T) ->
                    mochiweb_headers:insert(K, binary_to_list(V), T)
            end, mochiweb_headers:empty(), Headers),

    %% fix raw path
    Path1 = case Path of
        <<>> ->
            <<"/">>;
        _ ->
            Path
    end,
    RawPath = case QS of
        <<>> ->
            Path1;
        _ ->
            << Path1/binary, "?", QS/binary >>
    end,
    MochiReq = mochiweb_request:new(MochiSocket,
                                    Method,
                                    binary_to_list(RawPath),
                                    Version,
                                    MochiHeaders),
    call_body(HttpLoop, MochiReq),
    after_response(Req, MochiReq).

mochiweb_socket(cowboy_transport_ssl, Socket) ->
    {ssl, Socket};
mochiweb_socket(_Transport, Socket) ->
    Socket.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

after_response(Req, MochiReq) ->
    Connection =MochiReq:get_header_value("connection"),
    Req2 = Req#http_req{connection = list_to_connection(Connection),
                        resp_state = done,
                        body_state = done,
                        buffer = <<>> },

    case MochiReq:should_close() of
        true ->
            {ok, Req2#http_req{connection=close}};
        false ->
            MochiReq:cleanup(),
            erlang:garbage_collect(),
            {ok, Req2}
    end.

list_to_connection(Connection) when is_binary(Connection) ->
    list_to_connection(binary_to_list(Connection));
list_to_connection(Connection) when is_atom(Connection) ->
    Connection;
list_to_connection("keep-alive") ->
    keepalive;
list_to_connection(_) ->
    close.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.
