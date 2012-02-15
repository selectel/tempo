-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).
-define(STUB, not_loaded(?LINE)).
-define(CHECK_PID(PID, DO), case self() == PID of
                                true -> DO;
                                false -> {error, creator_only}
                            end).

-define(WE_CLAUSE(NAME, TYPE),
        NAME({ecirca, _Ref, Res, Pid, TYPE}) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res))).

-define(WE_CLAUSE(NAME, TYPE, A),
        NAME({ecirca, _Ref, Res, Pid, TYPE}, A) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A))).

-define(WE_CLAUSE(NAME, TYPE, A, B),
        NAME({ecirca, _Ref, Res, Pid, TYPE}, A, B) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A, B))).

-define(WE_CLAUSE(NAME, TYPE, A, B, C),
        NAME({ecirca, _Ref, Res, Pid, TYPE}, A, B, C) ->
               ?CHECK_PID(Pid, (ecirca ++ TYPE):NAME(Res, A, B, C))).

-define(WITH_ECIRCA(NAME),
        ?WE_CLAUSE(NAME, small);
        ?WE_CLAUSE(NAME, medium);
        ?WE_CLAUSE(NAME, large);
        NAME(_) -> error(badarg)).

-define(WITH_ECIRCA(NAME, A),
        ?WE_CLAUSE(NAME, small, A);
        ?WE_CLAUSE(NAME, medium, A);
        ?WE_CLAUSE(NAME, large, A);
        NAME(_, _) -> error(badarg)).

-define(WITH_ECIRCA(NAME, A, B),
        ?WE_CLAUSE(NAME, small, A, B);
        ?WE_CLAUSE(NAME, medium, A, B);
        ?WE_CLAUSE(NAME, large, A, B);
        NAME(_, _, _) -> error(badarg)).


-define(WITH_ECIRCA(NAME, A, B, C),
        ?WE_CLAUSE(NAME, small, A, B, C);
        ?WE_CLAUSE(NAME, medium, A, B, C);
        ?WE_CLAUSE(NAME, large, A, B, C);
        NAME(_, _, _, _) -> error(badarg)).


-define(WVS_CLAUSE(NAME, SIZE),
        NAME(SIZE) -> (ecirca ++ SIZE):NAME()).

-define(WVS_CLAUSE(NAME, SIZE, A),
        NAME(A, SIZE) -> (ecirca ++ SIZE):NAME(A)).

-define(WVS_CLAUSE(NAME, SIZE, A, B),
        NAME(A, B, SIZE) -> (ecirca ++ SIZE):NAME(A, B)).

-define(WVS_CLAUSE(NAME, SIZE, A, B, C),
        NAME(A, B, C, SIZE) -> (ecirca ++ SIZE):NAME(A, B, C)).

-define(WITH_VALUE_SIZE(NAME),
        ?WVS_CLAUSE(NAME, small);
        ?WVS_CLAUSE(NAME, medium);
        ?WVS_CLAUSE(NAME, large)).

-define(WITH_VALUE_SIZE(NAME, A),
        ?WVS_CLAUSE(NAME, small, A);
        ?WVS_CLAUSE(NAME, medium, A);
        ?WVS_CLAUSE(NAME, large, A)).

-define(WITH_VALUE_SIZE(NAME, A, B),
        ?WVS_CLAUSE(NAME, small, A, B);
        ?WVS_CLAUSE(NAME, medium, A, B);
        ?WVS_CLAUSE(NAME, large, A, B)).

-define(WITH_VALUE_SIZE(NAME, A, B, C),
        ?WVS_CLAUSE(NAME, small, A, B, C);
        ?WVS_CLAUSE(NAME, medium, A, B, C);
        ?WVS_CLAUSE(NAME, large, A, B, C)).

-define(GEN_STUB(FUN), gen_stub_fun() -> FUN).
