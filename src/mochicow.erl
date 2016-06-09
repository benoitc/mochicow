-module(mochicow).

-export([parse_transform/2]).

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