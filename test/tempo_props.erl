-module(tempo_props).
-compile({no_auto_import, [now/0]}).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(TYPES, [unix, now, datetime]).
-define(ISO8601, "%Y-%m-%dT%H:%M:%S%z").
-define(EXTRASEQS, ["%a", "%A", "%b", "%B", "%c", "%d", "%F", "%H",
                    "%j", "%m", "%M", "%p", "%S", "%U", "%w",
                    "%W", "%x", "%X", "%y", "%Y"]).
-define(SEPARATORS, [" ", "-", "_", "/"]).
-define(STANDARTS, [iso8601, rfc1123, rfc2822]).

%% Generators.

standard_format() -> oneof(?STANDARTS).

extra_part() ->
    ?LET(N, integer(1, 15),
         vector(N, oneof(?EXTRASEQS))).

unambiguous_format() ->
    ?LET({Extra, Separator},
         {extra_part(), oneof(?SEPARATORS)},
         begin
             Separated = [Separator | string:join(Extra, Separator)],
             list_to_binary(lists:flatten(?ISO8601 ++ Separated))
         end).

datetime() ->
    ?LET({Year, Month, Day, Hour, Minute, Second},
         {integer(0, 2042), integer(1, 12), integer(1, 28),
          integer(0, 23), integer(0, 59), integer(0, 59)},
         {{Year, Month, Day}, {Hour, Minute, Second}}).

now() ->
    ?LET({MegaSecs, Secs, MicroSecs},
         {integer(0, 5000), integer(0, 999999), integer(0, 999999)},
         {MegaSecs, Secs, MicroSecs}).

%% Properties.

prop_format_parse() ->
    ?FORALL({Time, Format}, {integer(), unambiguous_format()},
            try
                {ok, Buffer}  = tempo:format(Format, {unix, Time}),
                {ok, NewTime} = tempo:parse(Format, Buffer, unix),
                {ok, NewBuffer} = tempo:format(Format, {unix, NewTime}),
                NewBuffer == Buffer
            catch
                _:_ -> false
            end).

prop_format_parse_now() ->
    ?FORALL({{MegaSecs, Secs, _}=Now, Format}, {now(), standard_format()},
            try
                {ok, Buffer} = tempo:format(Format, Now, now),
                {ok, {MegaSecs, Secs, 0}} == tempo:parse(Format, Buffer, now)
            catch
                _:_ -> false
            end).

prop_format_parse_datetime() ->
    ?FORALL({Datetime, Format}, {datetime(), standard_format()},
            try
                {ok, Buffer} = tempo:format(Format, Datetime, datetime),
                {ok, Datetime} == tempo:parse(Format, Buffer, datetime)
            catch
                _:_ -> false
            end).

prop_parse_stable_on_binaries() ->
    ?FORALL({Format, Buf}, {binary(), binary()},
            valid_parse_return(Format, Buf)).

prop_parse_stable_on_anything() ->
    ?FORALL({Format, Buf}, {any(), any()},
            valid_parse_return(Format, Buf)).

prop_format_stable_on_valid_types() ->
    ?FORALL({Format, Time}, {binary(), integer()},
            valid_format_return(Format, Time)).

prop_format_stable_on_anything() ->
    ?FORALL({Format, Time}, {any(), any()},
            valid_format_return(Format, Time)).

%% Helpers.
valid_parse_return(Format, Buf) ->
    case catch tempo:parse(Format, Buf, unix) of
        {ok, T} when is_integer(T) -> true;
        {error, format_mismatch}   -> true;
        {'EXIT', {badarg, _}}      -> true;
        _                          -> false
    end.

valid_format_return(Format, Time) ->
    case catch tempo:format(Format, Time, unix) of
        {ok, T} when is_binary(T)  -> true;
        {error, invalid_time}      -> true;
        {'EXIT', {badarg, _}}      -> true;
        _                          -> false
    end.

%% Suite.

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(tempo_props, [{to_file, user},
                                                   {numtests, 5000}]))}.
