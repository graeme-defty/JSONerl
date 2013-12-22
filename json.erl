-module(json).
-export([val/1, array/1, array/2, object/1, object/2, format/1, format/2]).

-include_lib("eunit/include/eunit.hrl").

% ----------- Simple values ----------
val(null)                   -> "null";
val(true)                   -> "true";
val(false)                  -> "false";
val(V) when is_integer(V)   ->  integer_to_list(V);
val(V) when is_float(V)     ->  mochinum:digits(V);
val({YYYY, MM, DD}) when	is_integer(YYYY)
                  andalso   is_integer(MM)
                  andalso   is_integer(DD)
          ->  io_lib:format("\"~4..0b-~2..0b-~2..0b\"", [YYYY,MM,DD]);

% ----------- Strings and atoms ----------
val(V) when is_list(V)      ->  val_str(V, "\"");
val(V) when is_atom(V)      ->  val_str(atom_to_list(V), "\"").

val_str([], Acc)      ->  lists:reverse([$"|Acc]);  %" Dopy comment for the dopey highlighter
val_str([$"|T], Acc)  ->  val_str(T, [$",$\\|Acc]);
val_str([$\\|T], Acc) ->  val_str(T, [$\\,$\\|Acc]);
val_str([$\/|T], Acc) ->  val_str(T, [$/,$\\|Acc]);
val_str([$\b|T], Acc) ->  val_str(T, [$b,$\\|Acc]);
val_str([$\f|T], Acc) ->  val_str(T, [$f,$\\|Acc]);
val_str([$\n|T], Acc) ->  val_str(T, [$n,$\\|Acc]);
val_str([$\r|T], Acc) ->  val_str(T, [$r,$\\|Acc]);
val_str([$\t|T], Acc) ->  val_str(T, [$t,$\\|Acc]);
val_str([H|T], Acc)   ->  val_str(T, [H|Acc]).

% ----------- Arrays ----------
array(X)        ->  array(X, " ").

array([], _Fmt) ->  "[ ]";
array(L, Fmt)   ->  array(tl(L), [hd(L), Fmt, "["], Fmt).

array([], Acc, Fmt) ->
    lists:reverse(["]", Fmt | Acc]);

array([H|T], Acc, Fmt) ->
    array(T, [H, Fmt, "," | Acc], Fmt).


% ----------- Objects ----------
object(X)        ->  object(X, $\n).

object([], _Fmt) ->  "{ }";
object(L, Fmt)   ->  object(tl(L), [pair(hd(L)), Fmt, "{"], Fmt).

object([], Acc, Fmt) ->
    lists:reverse(["}", Fmt | Acc]);

object([H|T], Acc, Fmt) ->
    object(T, [pair(H), Fmt, "," | Acc], Fmt).

pair({Key,Val}) ->  val(Key) ++ " : " ++ Val.

% ----------- Format ----------
format(J) ->
    format(lists:flatten(J),1).

format(J, Nudge) ->
    format(lists:flatten(J),[],0,Nudge).

format([],Acc,Indent,Nudge)     ->  lists:reverse(Acc);
format([$"|T],Acc,Indent,Nudge) ->  format_string(T,[$"|Acc],Indent,Nudge);
format([$[|T],Acc,Indent,Nudge) ->  format(T, [$[|Acc],Indent+Nudge,Nudge);
format([$]|T],Acc,Indent,Nudge) ->  format(T, [$]|Acc],max(Indent-Nudge,0),Nudge);
format([$\n|T],Acc,Indent,Nudge)->  format(T, [lists:duplicate(Indent,$\s),$\n|Acc],Indent,Nudge);
format([H|T],Acc,Indent,Nudge)  ->  format(T,[H|Acc],Indent,Nudge).

format_string([$\\,X|T],Acc,Indent,Nudge)-> format_string(T,[X,$\\|Acc],Indent,Nudge);
format_string([$"|T],Acc,Indent,Nudge)   -> format(T,[$"|Acc],Indent,Nudge);
format_string([H|T],Acc,Indent,Nudge)    -> format_string(T,[H|Acc],Indent,Nudge).

max(X,Y)-> case X>Y of true -> X; false ->Y end.

% ---------------------- tests -----------------------

parse_test_() ->
  [
    ?_assertEqual("23", val(23)),
%    ?_assertEqual("234.5", val(234.5)),
    ?_assertEqual("\"abcde\"", val("abcde")),
    ?_assertEqual("\"53/12/19\"", lists:flatten(val({53,12,19}))),
    ?_assertEqual("[ abc, def, jkl ]", lists:flatten(array(["abc","def","jkl"]))),
    ?_assertEqual("[ abc, def, jkl ]", lists:flatten(array(["abc","def","jkl"],$\s))),
    ?_assertEqual("[\nabc,\ndef,\njkl\n]", lists:flatten(array(["abc","def","jkl"],$\n))),
    ?_assertEqual("{\nabc : xyz,\ndef : tuv,\njkl : pqr\n}", lists:flatten(object([{"abc","xyz"},{"def","tuv"},{"jkl","pqr"}]))),
    ?_assertEqual("{ abc : xyz, def : tuv, jkl : pqr }", lists:flatten(object([{"abc","xyz"},{"def","tuv"},{"jkl","pqr"}],$\s))),
    ?_assertEqual("{\nabc : xyz,\ndef : tuv,\njkl : pqr\n}", lists:flatten(object([{"abc","xyz"},{"def","tuv"},{"jkl","pqr"}],$\n))),
    ?_assertEqual("abc", format("abc")),
    ?_assertEqual("abc\n[\n def\n ]ghi", lists:flatten(format("abc\n[\ndef\n]ghi"))),
    ?_assert(true)
  ].
