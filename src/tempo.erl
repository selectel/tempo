%%% @author Dmitry Groshev <groshev@selectel.ru>
%%% @author Sergey Levedev <lebedev@selectel.ru>
%%%
%%% @copyright 2012 Selectel Ltd.
%%%
%%% @doc NIF-based date and time parsing and formatting for Erlang.
%%% This module implements an interface to strptime/strftime with
%%% appropriate handling of Erlang datetime formats.
%%%
%%% All exported functions in this module can throw `badarg' if
%%% malformed input is provided.
%%%
%%% A <em>Type</em> argument, accepted by some of the exported functions
%%% should be one of the following:
%%%
%%% ```
%%% | Type     | Description                                        |
%%% |----------+----------------------------------------------------|
%%% | unix     | UNIX timestamp, a positive integer denoting number |
%%% |          | of seconds since 1 Jan 1970.                       |
%%% | now      | @see erlang:now/0                                  |
%%% | datetime | @see calendar:datetime/0                           |
%%% '''
%%%
%%% A <em>Format</em> argument to any of the exported functions is
%%% either a {@type binary()} with strptime/strftime compatible tokens or
%%% one of the following atoms: iso8601, rfc1123, rfc2822. In the latter
%%% case a predefined format will be used.
%%%
%%% *A note about 32-bit systems*
%%%
%%% Functions of "format" family can return "{error, time_overflow}" if
%%% the underlying 32-bit value overflows. This is presumably possible only
%%% on 32-bit systems. Minimum datetime for such systems is
%%% `{{1901,12,13},{20,45,52}}' and maximum is `{{2038,1,19},{3,14,7}}'.
%%%
%%% @end
%%%

-module(tempo).
-on_load(nif_init/0).

-ifdef(DEBUG).
-compile([export_all]).
-endif.

-define(STUB, not_loaded(?LINE)).
-define(MEGA, 1000000).
-define(MICRO, 0.000001).
-define(EPOCH_ZERO,
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

-export([parse/2, parse/3,
         parse_unix/2, parse_now/2, parse_datetime/2,
         format/2, format/3,
         format_unix/2, format_now/2, format_datetime/2]).

-type unix_timestamp() :: float().
-type format()         :: binary()
                        | iso8601
                        | rfc1123
                        | rfc2822.
-type datetime_type()  :: unix
                        | now
                        | datetime.
-type datetime_value() :: unix_timestamp()
                        | erlang:timestamp()
                        | calendar:datetime().

%% @doc Parses {Type, Binary} tuple according to provided format, returns
%%      ok/error tuples with datetime in format that depends on atom Type.
%%      @equiv parse(Format, DatetimeType, Binary)
%% @end
-spec parse(format(), {datetime_type(), binary()}) -> {ok, datetime_value()}
                                                    | {error, format_mismatch}.
parse(Format, {Type, Bin}) -> parse(Format, Bin, Type).

%% @doc Parses Binary according to Format and returns ok/error tuple with
%%      datetime in format that depends on atom Type.
%% @end
-spec parse(format(), binary(),
            datetime_type()) -> {ok, datetime_value()}
                              | {error, format_mismatch}.
parse(Format, Bin, Type) ->
    case Type of
        unix     -> parse_unix(Format, Bin);
        now      -> parse_now(Format, Bin);
        datetime -> parse_datetime(Format, Bin)
    end.

%% @doc Helper function similar to {@link parse/3}.
%%      @equiv parse(Format, Binary, timestamp)
%% @end
-spec parse_unix(format(), binary()) -> {ok, unix_timestamp()}
                                      | {error, format_mismatch}.
parse_unix(Format, Bin) ->
    case strptime(convert_format(Format), Bin) of
        {ok, Timestamp} ->
            {ok, round(Timestamp)};
        Err -> Err
    end.

%% @doc Helper function similar to {@link parse/3}.
%%      @equiv parse(Format, Binary, now)
%% @end
-spec parse_now(format(), binary()) -> {ok, erlang:timestamp()}
                                     | {error, format_mismatch}.
parse_now(Format, Bin) ->
    case parse_unix(Format, Bin) of
        {ok, RawTimestamp} ->
            Timestamp = trunc(RawTimestamp),
            MicroSecs = RawTimestamp - Timestamp,
            MegaSecs = Timestamp div ?MEGA,
            Secs = Timestamp rem ?MEGA,
            {ok, {MegaSecs, Secs, MicroSecs}};
        Err -> Err
    end.

%% @doc Helper function similar to {@link parse/3}.
%%      @equiv parse(Format, Binary, datetime)
%% @end
-spec parse_datetime(format(), binary()) -> {ok, calendar:datetime()}
                                          | {error, format_mismatch}.
parse_datetime(Format, Bin) ->
    case parse_unix(Format, Bin) of
        {ok, Timestamp} ->
            DT = calendar:gregorian_seconds_to_datetime(?EPOCH_ZERO +
                                                            Timestamp),
            {ok, DT};
        Err -> Err
    end.

%% @doc Formats {Type, Datetime} tuple according to Format. The way in which
%%      Datetime will be handled depends on Type.
%%      @equiv format(Format, Datetime, Type)
%% @end
-spec format(format(),
             {datetime_type(), datetime_value()}) -> {ok, binary()}
                                                   | {error, invalid_time}
                                                   | {error, time_overflow}.
format(Format, {Type, Datetime}) -> format(Format, Datetime, Type).

%% @doc Formats Datetime according to Format. The way in which
%%      Datetime will be handled depends on Type.
%% @end
-spec format(format(), datetime_value(),
             datetime_type()) -> {ok, binary()}
                               | {error, invalid_time}
                               | {error, time_overflow}.
format(Format, Datetime, Type) ->
    case Type of
        unix     -> format_unix(Format, Datetime);
        now      -> format_now(Format, Datetime);
        datetime -> format_datetime(Format, Datetime)
    end.

%% @doc Helper function similar to {@link format/3}.
%%      @equiv format(Format, Datetime, timestamp)
%% @end
-spec format_unix(format(), unix_timestamp()) -> {ok, binary()}
                                               | {error, invalid_time}
                                               | {error, time_overflow}.
format_unix(Format, Timestamp) ->
    strftime(convert_format(Format), float(Timestamp)).

%% @doc Helper function similar to {@link format/3}.
%%      @equiv format(Format, Datetime, now)
%% @end
-spec format_now(format(), erlang:timestamp()) -> {ok, binary()}
                                                | {error, invalid_time}
                                                | {error, time_overflow}.
format_now(Format, {MegaSecs, Secs, MicroSecs}) ->
    Timestamp = ?MEGA * MegaSecs + Secs + ?MICRO * MicroSecs,
    format_unix(Format, Timestamp).

%% @doc Helper function similar to {@link format/3}.
%%      @equiv format(Format, Datetime, datetime)
%% @end
-spec format_datetime(format(), calendar:datetime()) -> {ok, binary()}
                                                      | {error, invalid_time}
                                                      | {error, time_overflow}.
format_datetime(Format, Datetime) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(Datetime) - ?EPOCH_ZERO,
    format_unix(Format, Timestamp).

%% @private
%% @doc Returns a predefined format for a given "format atom".
%% @end
-spec convert_format(format()) -> binary().
convert_format(X) when is_binary(X) -> X;
convert_format(iso8601) -> <<"%Y-%m-%dT%H:%M:%SZ">>;
convert_format(rfc1123) -> <<"%a, %d %b %Y %H:%M:%S GMT">>;
convert_format(rfc2822) -> <<"%a, %d %b %Y %H:%M:%S +0000">>;
convert_format(X)       -> error(badarg, [X]).

%% @private
%% @doc This function will be replaced with NIF's strptime.
%% @end
-spec strptime(binary(), binary()) -> {ok, integer()}
                                    | {error, format_mismatch}.
strptime(_Format, _DT) -> ?STUB.

%% @private
%% @doc This function will be replaced with NIF's strftime.
%% @end
-spec strftime(binary(), integer()) -> {ok, binary()}
                                     | {error, invalid_time}.
strftime(_Format, _DT) -> ?STUB.

%% @private
%% @doc Searches for NIF in private directory of "tempo" application.
%% @end
-spec nif_init() -> ok | {error, _}.
nif_init() ->
    PrivDir = case code:priv_dir(tempo) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

%% @private
%% @doc Helper for exiting gracefully when NIF can't be loaded.
%% @end
-spec not_loaded(pos_integer()) -> ok.
not_loaded(Line) -> exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
