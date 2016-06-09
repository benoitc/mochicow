-module(mochi_echo_handler).


-export([init/3, loop/1]).


init(_, _, _) ->
    {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
	case Req:get(method) of
		'GET' -> 
    		Req:respond({200, [{"Content-Type", "text/html"}],
                 					<<"Hello from mochiweb">>});
    	'POST' ->
    		Body =  Req:recv_body(),
    		Req:respond({200, [{"Content-Type", "text/html"}], Body});
    	_Else -> 
            Req:respond({405, [{"Content-Type", "text/html"}], 
                       <<"unsupported method">>})
    end.

