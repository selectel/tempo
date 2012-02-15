%%% @private
%%% @doc This module contains function to glue atoms in compile time.
%%%      The idea of it belongs to Zed from this SO question:
%%%      http://stackoverflow.com/questions/1423054/
%%%      erlang-is-there-an-equivalent-to-the-c-preprocessor-directive

-module(ecirca_pt).
-export([parse_transform/2]).
-include("ecirca.hrl").

parse_transform(AST, _Options) ->
    [findfun(T, fun (Tree) ->
                        erl_syntax_lib:map(fun replace_concat/1, Tree)
                end) || T <- AST].

findfun({function, _, _, _, _} = Tree, OnFun) -> OnFun(Tree);
findfun(Tree, _) -> Tree.

replace_concat(Tree) ->
    Stx = try
              infix_expr = erl_syntax:type(Tree),
              Op = erl_syntax:infix_expr_operator(Tree),
              Left = erl_syntax:infix_expr_left(Tree),
              Right = erl_syntax:infix_expr_right(Tree),
              '++' = erl_syntax:operator_name(Op),
              atom = erl_syntax:type(Left),
              atom = erl_syntax:type(Right),
              erl_syntax:atom(erl_syntax:atom_literal(Left)
                              ++ "_"
                              ++  erl_syntax:atom_literal(Right))
          catch
              error:{badmatch, _} -> Tree
          end,
    erl_syntax:revert(Stx).
