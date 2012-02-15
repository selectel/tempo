-module(ecirca_props).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([initial_state/0, initial_state/4, command/1,
         precondition/2, postcondition/3, next_state/3]).

-define(SERVER, ecirca).
-define(TYPES, [last, max, min, avg, sum]).
-define(VALUE_SIZES, [small, medium, large]).

-record(state, {ecirca     = undefined       :: ecirca:ecirca() | undefined,
                elements   = array:new()     :: array(),
                size       = 0               :: pos_integer(),
                value_size = medium          :: ecirca:ecirca_value_size(),
                type       = last            :: ecirca:ecirca_type(),
                atoms      = [{empty, weak}] :: [{atom(), weak | strong}],
                saved_bin  = undefined       :: binary(),
                saved_els  = undefined       :: ecirca:ecirca() | undefined}).

ecirca_type() -> elements(?TYPES).
value_size() -> elements(?VALUE_SIZES).
atom_list() -> ?LET(L, integer(0, 14),
                    vector(L, tuple([readable_atom(3, 7),
                                     oneof([weak, strong])]))).
readable_atom(LengthMin, LengthMax) ->
    ?LET(L, integer(LengthMin, LengthMax),
         ?LET(Lst, vector(L, choose($a, $z)),
              list_to_atom(Lst))).
ecirca_size(short) -> integer(1, 10);
ecirca_size(full) ->
    {ok, M} = ecirca:max_size(),
    integer(1, M).
value(S) ->
    Max = ecirca:max_value(S#state.value_size),
    union([integer(0, Max)]
          ++ [X || {X, _} <- S#state.atoms]
          ++ [empty]).
position(S) ->  integer(1, S#state.size).
ecirca(S) -> S#state.ecirca.
ecirca_bin(S) -> S#state.saved_bin.
ecirca_val_size(S) -> S#state.value_size.
push_count(S) -> integer(1, S#state.size * 2).

initial_state() ->
    initial_state(1000, last, medium, []).

initial_state(Size, Type, ValueSize, Atoms) ->
    #state{elements   = array:new([{default, empty}]),
           size       = Size,
           value_size = ValueSize,
           type       = Type,
           atoms      = Atoms}.

command(#state{ecirca = undefined}=S) ->
    frequency(
      [{4, {call, ?SERVER, new, [S#state.size, S#state.type,
                                 S#state.value_size, S#state.atoms]}},
       {1, {call, ?SERVER, new, [S#state.size, S#state.type,
                                 S#state.value_size]}},
       {1, {call, ?SERVER, new, [S#state.size, S#state.type]}}]);
command(S) ->
    BinDefined = S#state.saved_bin /= undefined,
    frequency(
      [{60, {call, ?SERVER, slice, [ecirca(S), position(S), position(S)]}},
       {60, {call, ?SERVER, update, [ecirca(S), position(S), value(S)]}},
       {50, {call, ?SERVER, set, [ecirca(S), position(S), value(S)]}},
       {50, {call, ?SERVER, get, [ecirca(S), position(S)]}},
       {50, {call, ?SERVER, push, [ecirca(S), value(S)]}},
       {50, {call, ?SERVER, push_many, [ecirca(S), push_count(S), value(S)]}},
       {10, {call, ?SERVER, size, [ecirca(S)]}},
       {10, {call, ?SERVER, save, [ecirca(S)]}},
       {5, {call, ?SERVER, max_slice, []}},
       {5, {call, ?SERVER, max_size, []}}]
      ++ [{30, {call, ?SERVER, load, [ecirca_bin(S), ecirca_val_size(S)]}}
          || BinDefined]).

precondition(_S, {call, _, slice, [_, Start, End]}) ->
    abs(End - Start) =< ecirca:max_slice();
precondition(#state{type=sum, elements=Els, value_size=ValSize},
             {call, _, update, [_, Pos, Val]}) ->
    OldVal = array:get(Pos - 1, Els),
    (is_atom(OldVal) orelse
     is_atom(Val) orelse
     (OldVal + Val) =< ecirca:max_value(ValSize));
precondition(_, _) -> true.

next_state(S, V, {call, Mod, new, [Size, Type]}) ->
    next_state(S, V, {call, Mod, new, [Size, Type, medium, []]});
next_state(S, V, {call, Mod, new, [Size, Type, ValueSize]}) ->
    next_state(S, V, {call, Mod, new, [Size, Type, ValueSize, []]});
next_state(S, V, {call, _Mod, new, [_Size, _Type, ValueSize, Atoms]}) ->
    S#state{ecirca     = {call, erlang, element, [2, V]},
            value_size = ValueSize,
            atoms      = Atoms};
next_state(S, _V, {call, _Mod, set, [_, Pos, Val]}) ->
    Update = fun (X) ->
                     S#state{elements = array:set(Pos - 1, X,
                                                  S#state.elements)}
             end,
    case S#state.type of
        avg when is_atom(Val)    -> Update(Val);
        avg when is_integer(Val) -> Update([Val]);
        _                        -> Update(Val)
    end;
next_state(#state{type=Type}=S, _V, {call, _Mod, update, [_, Pos, Val]}) ->
    maybe_update(S, Pos, Val, Type);
next_state(S, V, {call, Mod, push, [Ecirca, Val]}) ->
    Lst = [Val],
    next_state(S, V, {call, Mod, push_list, [Ecirca, Lst]});
next_state(S, V, {call, Mod, push_many, [Ecirca, Count, Val]}) ->
    Lst = lists:duplicate(Count, Val),
    next_state(S, V, {call, Mod, push_list, [Ecirca, Lst]});
next_state(S, _V, {call, _Mod, push_list, [_, PushLst]}) ->
    PushLstEscaped = case S#state.type of
                         avg -> [if is_atom(X)    -> X;
                                    is_integer(X) -> [X]
                                 end || X <- PushLst];
                         _ -> PushLst
                     end,
    Lst = PushLstEscaped ++ array:to_list(S#state.elements),
    LstTrimmed = lists:sublist(Lst, S#state.size),
    S#state{elements = array:from_list(LstTrimmed, empty)};
next_state(S, V, {call, _Mod, save, [_]}) ->
    S#state{saved_bin = {call, erlang, element, [2, V]},
            saved_els = S#state.elements};
next_state(S, V, {call, _Mod, load, [_, _]}) ->
    S#state{saved_bin = undefined,
            saved_els = undefined,
            elements  = S#state.saved_els,
            ecirca    = {call, erlang, element, [2, V]}};
next_state(S, _, _) -> S.

%% returned value should be equal to one that we've passed
postcondition(S, {call, _Mod, get, [_, Pos]}, Res) ->
    Res =:= {ok, get_element(S, Pos)};
%% returned ecirca should have properties that was requested
postcondition(_S, {call, _Mod, new, [Size, _Type, ValueSize]}, Res) ->
    {ok, Ecirca} = Res,
    {ecirca, _Ref, _Resource, Pid, ValueSize} = Ecirca,
    {ok, RealSize} = ecirca:size(Ecirca),
    (Size == RealSize andalso
     Pid == self());
%% slice should be right slice
postcondition(S, {call, _Mod, slice, [_, Pos1, Pos2]}, Res) ->
    {ok, Slice} = Res,
    Idxs = lists:seq(min(Pos1, Pos2), max(Pos1, Pos2)),
    PreRefSlice = [get_element(S, I) || I <- Idxs],
    RefSlice = case Pos2 < Pos1 of
                   true  -> lists:reverse(PreRefSlice);
                   false -> PreRefSlice
               end,
    RefSlice =:= Slice;
%% value returned by set should be {ok, {OldValue, NewValue}}
postcondition(S, {call, _Mod, set, [_, Pos, Val]}, Res) ->
    Res =:= {ok, {get_element(S, Pos), Val}};
postcondition(_S, {call, _Mod, push, [_, _]}, Res) ->
    Res =:= ok;
postcondition(S, {call, _Mod, size, [_]}, Res) ->
    {ok, Size} = Res,
    Size == S#state.size;
postcondition(_S, {call, _Mod, max_slice, []}, Res) ->
    {ok, SliceSize} = Res,
    is_integer(SliceSize);
postcondition(_S, {call, _Mod, max_size, []}, Res) ->
    {ok, MaxSize} = Res,
    is_integer(MaxSize);
postcondition(_, _, _) -> true.

prop_main() ->
    ?FORALL({Size, Type, ValueSize, Atoms},
            {ecirca_size(short),
             noshrink(ecirca_type()),
             noshrink(value_size()),
             atom_list()},
            ?FORALL(Cmds, commands(?MODULE,
                                   initial_state(Size, Type,
                                                 ValueSize,
                                                 Atoms)),
                    begin
                        {H,S,Res} = run_commands(?MODULE, Cmds),
                        ?WHENFAIL(
                           proper_report:report(Cmds, H, S, Res),
                           aggregate(command_names(Cmds), Res =:= ok))
                    end)).

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(ecirca_props, [{to_file, user},
                                                    {numtests, 1000}]))}.

%%% Internal functions
get_element(State, Position) ->
    Element = array:get(Position - 1, State#state.elements),
    if is_list(Element) -> round(avg(Element));
       true             -> Element
    end.

maybe_update(S, Pos, Val, Type) ->
    OldVal = array:get(Pos - 1, S#state.elements),
    Update = fun (NewVal) ->
                     S#state{elements = array:set(Pos - 1, NewVal,
                                                  S#state.elements)}
             end,
    case should_update(S, OldVal, Val, Type) of
        true ->
            case Type of
                max  -> Update(Val);
                min  -> Update(Val);
                last -> Update(Val);
                sum  -> Update(case (is_integer(Val) andalso
                                     is_integer(OldVal)) of
                                   true  -> Val + OldVal;
                                   false -> Val
                               end);
                avg -> Update(if
                                  is_atom(Val) -> Val;
                                  is_atom(OldVal) -> [Val];
                                  is_list(OldVal) -> [Val | OldVal];
                                  true -> exit({bad_value, Val, OldVal})
                              end)
            end;
        false -> S
    end.

-spec should_update(#state{},
                    integer() | atom(),
                    integer() | atom(),
                    last | min | max | avg | sum) -> true | false.
should_update(_S, empty, _Val, _T) -> true;
should_update(_S, _OldVal, empty, _T) -> false;
should_update(S, OldVal, Val, _T) when is_atom(OldVal)->
    case atom_or_int(S, OldVal) of
        strong -> case atom_or_int(S, Val) of
                      strong -> true;
                      _      -> false
                  end;
        weak   -> case atom_or_int(S, Val) of
                      weak -> false;
                      _    -> true
                  end
    end;
should_update(S, _OldVal, Val, _T) when is_atom(Val) ->
    case atom_or_int(S, Val) of
        strong -> true;
        weak   -> false
    end;
should_update(_S, OldVal, Val, T) when is_integer(Val),
                                       (is_integer(OldVal) orelse
                                        is_list(OldVal)) ->
    case T of
        last -> true;
        avg  -> true;
        sum  -> true;
        max  -> OldVal < Val;
        min  -> OldVal > Val
    end.

-spec atom_or_int(#state{}, integer() | atom()) -> strong | weak | int.
atom_or_int(_, empty) -> weak;
atom_or_int(#state{atoms=Atoms}, Val) when is_atom(Val) ->
    {Val, Type} = proplists:lookup(Val, Atoms),
    Type;
atom_or_int(_, Val) when is_integer(Val) -> int;
atom_or_int(#state{type=avg}, Val) when is_list(Val) -> int.

avg(Lst) -> sum(Lst) / length(Lst).
sum(Lst) -> lists:foldl(fun (X, A) -> X + A end, 0, Lst).
