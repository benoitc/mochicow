-module(mochicow_upgrade).

-export([upgrade/4]).

-include_lib("cowboy/include/http.hrl").
-include("mochicow.hrl").

upgrade(_ListenerPid, _Handler, Opts, Req) ->
    {loop, HttpLoop} = proplists:lookup(loop, Opts),
    #http_req{socket=Socket,
              transport=Transport,
              method=Method,
              version=Version,
              raw_path=Path,
              headers=Headers,
              raw_host=Host,
              port=Port,
              buffer=Buffer} = Req,

    MochiSocket = mochiweb_socket(Transport, Socket),
    MochiHeaders = mochiweb_headers:make(Headers),
    MochiHost = binary_to_list(Host) ++ ":" ++ integer_to_list(Port),
    MochiHeaders1 = mochiweb_headers:enter('Host', MochiHost,
                MochiHeaders),

    MochiReq = mochiweb_request:new(MochiSocket,
                                    Method,
                                    binary_to_list(Path),
                                    Version,
                                    MochiHeaders1),

    erlang:put(mochiweb_request_body, Buffer),
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
    Req2 = Req#http_req{connection=list_to_connection(Connection),
                        resp_state=done},

    case MochiReq:should_close() of
        true ->
            close;
        _ ->
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
list_to_connection("close") ->
    close.
