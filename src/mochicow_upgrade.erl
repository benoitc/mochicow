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
              buffer=Buffer} = Req,

    Transport:setopts(Socket, [{packet, raw}]),
    MochiSocket = mochiweb_socket(Transport, Socket),
    MochiHeaders = mochiweb_headers:from_list(Headers),

    MochiReq = mochiweb_request:new(MochiSocket,
                                    Method,
                                    Path,
                                    Version,
                                    MochiHeaders),

    put(mochiweb_request_body, Buffer),
    mochicow_protocol:call_body(HttpLoop, MochiReq).

mochiweb_socket(cowboy_transport_ssl, Socket) ->
    {ssl, Socket};
mochiweb_socket(_Transport, Socket) ->
    Socket.
