-module(mochicow).

-export([parse_transform/2]).

%% @internal
-export([recv/3]).

%% @private
parse_transform(AST, _Options) ->
    walk_ast([], AST).

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    walk_ast([{function, Line, Name, Arity,
                walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc],
                 T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H)|Acc], T).

transform_statement({call, Line, {remote, Line1, {atom, Line2, mochiweb_socket},
            {atom, Line3, recv}}, Arguments}) ->

    {block, Line,
     [
            {call, Line, {remote, Line1, {atom, Line2, mochicow},
                                        {atom, Line3, recv}}, Arguments}
     ]};

transform_statement({call, Line, {remote, Line1, {atom, Line2, mochiweb_request},
            {atom, Line3, Fun}}, Arguments}) ->

    {block, Line,
     [
            {call, Line, {remote, Line1, {atom, Line2, mochicow_request},
                                        {atom, Line3, Fun}}, Arguments}
     ]};

transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

recv(Socket, Length, Timeout) ->
    case erlang:erase(mochicow_buffer) of
      undefined -> recv1(Socket, Length, Timeout);
      <<>> -> recv1(Socket, Length, Timeout);
      Buffer ->
        Sz = byte_size(Buffer),
        if
          Sz >= Length ->
            << Data:Sz/binary, Rest/binary >> = Buffer,
            erlang:put(mochicow_buffer, Rest),
            {ok, Data};
          true ->
            case recv1(Socket, Length-Sz, Timeout) of
              {ok, Data} -> {ok, << Buffer/binary, Data/binary>>};
              _Error -> {ok, Buffer}
            end
        end
    end.

recv1({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv1(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).