%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.
%%%
-module(mochicow_upgrade).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4]).

-define(SAVE_BODY, mochiweb_request_body).

upgrade(Req, Env, _Handler, Opts) ->
    ok = mochicow_protocol:ensure_started(mochiweb_clock),
    {_, HttpLoop} = lists:keyfind(loop, 1, Opts),
    {_, Ref} = lists:keyfind(listener, 1, Env),
    ranch:remove_connection(Ref),

    [Transport,
     Socket,
     Host,
     Port,
     Method,
     Path,
     Version,
     Headers,
     Qs,
     Buffer] = cowboy_req:get([transport, socket, host, port, method, path, version, headers, qs, buffer], Req),


    MochiSocket = mochicow_protocol:mochiweb_socket(Transport, Socket),
    DefaultPort = default_port(Transport:name()),
    MochiHost = case Port of
        DefaultPort ->
            binary_to_list(Host);
        _ ->
            %% fix raw host
            binary_to_list(Host) ++ ":" ++ integer_to_list(Port)

    end,

    MochiHeaders = lists:foldl(fun({Key, Val}, Acc) ->
                                       Key2 = to_list(Key),
                                       case string:to_lower(Key2) of
                                           "host" -> mochiweb_headers:insert(Key2, MochiHost, Acc);
                                           _ -> mochiweb_headers:insert(Key2, to_list(Val), Acc)
                                       end
                               end, mochiweb_headers:empty(), Headers),

    put(mochicow_buffer, Buffer),

    MochiReq = mochicow_request:new(MochiSocket,
                                    [{ranch_ref, Ref}| Opts],
                                    to_atom(Method),
                                    raw_path(Path, Qs),
                                    Version,
                                    MochiHeaders),



    call_body(HttpLoop, MochiReq),
    mochiweb_http:after_response(HttpLoop, MochiReq).

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).


raw_path(<<>>, QS) -> raw_path(<<"/">>, QS);
raw_path(Path, <<>>) -> binary_to_list(Path);
raw_path(Path, QS) -> lists:flatten(binary_to_list(<< Path/binary, "?", QS/binary >>)).

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_list(V) -> V.

to_atom(V) when is_binary(V) -> binary_to_atom(V, latin1);
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_atom(V) -> V.



-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.
