-module(tempo_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Generators.

format() ->
    ?LET(Chunks, proper_stdgen:subset(["%d", "%H", "%m", "%M", "%S", "%Y"]),
         list_to_binary(lists:flatten(Chunks))).

%% Properties.

prop_strftime_strptime() ->
    ?FORALL({Time, Format}, {integer(), format()},
            begin
                try
                    {ok, Buffer}  = tempo:format(Format, {timestamp, Time}),
                    {ok, NewTime} = tempo:parse(Format, Buffer, timestamp),
                    {ok, NewBuffer} = tempo:format(Format, {timestamp, NewTime}),
                    NewBuffer == Buffer
                catch
                    _:_ -> false
                end
            end).

%% Suite.

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(tempo_props, [{to_file, user},
                                                   {numtests, 5000}]))}.
