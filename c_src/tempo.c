#include <string.h>
#include <inttypes.h>
#include <time.h>
#include "erl_nif.h"

#define ATOM_OK         enif_make_atom(env, "ok")
#define ATOM_ERROR      enif_make_atom(env, "error")
#define ATOM_OVERFLOW   enif_make_atom(env, "overflow")
#define BADARG          enif_make_badarg(env)
#define TUPLE2(A, B)    enif_make_tuple2(env, A, B)
#define TUPLE3(A, B, C) enif_make_tuple3(env, A, B, C)
#define TUPLE_OK(A)     TUPLE2(ATOM_OK, A)
#define TUPLE_ERROR(A)  TUPLE2(ATOM_ERROR, A)
#define INT(A)          enif_make_int(env, A)
#define ATOM(A)         enif_make_atom(env, A)

static ERL_NIF_TERM
tempo_strptime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary format, buf;
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_is_binary(env, argv[1])
        || !enif_inspect_binary(env, argv[0], &format)
        || !enif_inspect_binary(env, argv[1], &format)) {
        return BADARG;
    }

    memset(&tm, 0, sizeof(struct tm));
    strptime(format.data, buf.data, &tm);

    return TUPLE_OK(TUPLE2(TUPLE3(INT(tm.tm_year),
                                  INT(tm.tm_mon),
                                  INT(tm.tm_mday)),
                           TUPLE3(INT(tm.tm_hour),
                                  INT(tm.tm_min),
                                  INT(tm.tm_sec))));
}

static ERL_NIF_TERM
tempo_strftime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return ATOM_OK;
}

static ErlNifFunc tempo_funcs[] =
{
    {"strptime", 2, tempo_strptime},
    {"strftime", 2, tempo_strftime}
};

ERL_NIF_INIT(tempo, tempo_funcs, NULL, NULL, NULL, NULL)
