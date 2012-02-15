-include("ecirca.hrl").
-compile(export_all).
-on_load(nif_init/0).

new(_, _, _)    -> ?STUB.
set(_, _, _)    -> ?STUB.
update(_, _, _) -> ?STUB.
push(_, _)      -> ?STUB.
get(_, _)       -> ?STUB.
slice(_, _, _)  -> ?STUB.
max_size()      -> ?STUB.
max_slice()     -> ?STUB.
size(_)         -> ?STUB.
save(_)         -> ?STUB.
load(_)         -> ?STUB.

%% @private
%% @doc Loads a NIF
-spec nif_init() -> ok | {error, _}.
nif_init() ->
    PrivDir = case code:priv_dir(ecirca) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

-spec not_loaded(pos_integer()) -> ok.
not_loaded(Line) -> exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
