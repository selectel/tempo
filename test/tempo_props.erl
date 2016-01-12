-module(tempo_props).
-compile({no_auto_import, [now/0]}).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPES, [unix, now, datetime]).
-define(ISO8601, "%Y-%m-%dT%H:%M:%SZ").
-define(EXTRASEQS, ["%a", "%A", "%b", "%B", "%c", "%d", "%F", "%H",
                    "%j", "%m", "%M", "%p", "%S", "%U", "%w",
                    "%W", "%x", "%X", "%y", "%Y"]).
-define(SEPARATORS, [" ", "-", "_", "/"]).
-define(PREDEFINED, [iso8601, rfc1123, rfc2822]).

%% Generators.

standard_format() -> oneof(?PREDEFINED).

extra_part() ->
    %% strptime on OS X produces error when %p and %H are specified and
    %% hour > 12
    ExtraSeqs = case os:type() of
                    {unix, darwin} -> ?EXTRASEQS -- ["%p"];
                    _              -> ?EXTRASEQS
                end,
    ?LET(N, integer(1, 15),
         vector(N, oneof(ExtraSeqs))).

unambiguous_format() ->
    ?LET({Extra, Separator},
         {extra_part(), oneof(?SEPARATORS)},
         begin
             Separated = [Separator | string:join(Extra, Separator)],
             list_to_binary(lists:flatten(?ISO8601 ++ Separated))
         end).

datetime() ->
    YearMin = case os:type() of
                  {unix, darwin} -> 1902;
                  _              -> 0
              end,
    ?LET({Year, Month, Day, Hour, Minute, Second},
         {integer(YearMin, 2042), integer(1, 12), integer(1, 28),
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
                case tempo:format(Format, Now, now) of
                    {ok, Buffer} ->
                        {ok, {MegaSecs, Secs, 0}}
                            == tempo:parse(Format, Buffer, now);
                    {error, time_overflow} ->
                        case erlang:system_info(wordsize) of
                            %% 64-bit system, time_t is int64
                            8 -> false;
                            %% 32-bit system, time_t is int32
                            4 -> can_overflow_now(Now)
                        end
                end
            catch
                _:_ -> false
            end).

prop_format_parse_datetime() ->
    ?FORALL({Datetime, Format}, {datetime(), standard_format()},
            try
                case tempo:format(Format, Datetime, datetime) of
                    {ok, Buffer} ->
                        {ok, Datetime}
                            == tempo:parse(Format, Buffer, datetime);
                    {error, time_overflow} ->
                        case erlang:system_info(wordsize) of
                            %% 64-bit system, time_t is int64
                            8 -> false;
                            %% 32-bit system, time_t is int32
                            4 -> can_overflow_datetime(Datetime)
                        end
                end
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

can_overflow_now({MegaSecs, Secs, _}) ->
    MegaSecs * 1000000 + Secs > 2147483647.

can_overflow_datetime(Datetime) ->
    GregMin = calendar:datetime_to_gregorian_seconds(
                {{1901, 12, 13}, {20, 45, 52}}),
    GregMax = calendar:datetime_to_gregorian_seconds(
                {{2038, 1, 19}, {3, 14, 7}}),
    Greg = calendar:datetime_to_gregorian_seconds(Datetime),
    Greg > GregMax orelse Greg < GregMin.

%% Suite.

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(tempo_props, [{to_file, user},
                                                   {numtests, 5000}]))}.
