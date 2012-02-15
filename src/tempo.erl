-module(tempo).
-on_load(nif_init/0).
-define(STUB, not_loaded(?LINE)).

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
