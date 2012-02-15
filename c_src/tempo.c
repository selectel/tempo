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
    char format_str[255], buf_str[255];

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_is_binary(env, argv[1])
        || !enif_inspect_binary(env, argv[0], &format)
        || !enif_inspect_binary(env, argv[1], &buf)
        || format.size >= 255
        || buf.size >= 255) {
        return BADARG;
    }

    memset(&tm, 0, sizeof(struct tm));
    memcpy(format_str, format.data, format.size);
    format_str[format.size] = 0;
    memcpy(buf_str, buf.data, buf.size);
    buf_str[buf.size] = 0;

    if (strptime((const char *) format_str,
                 (const char *) buf_str, &tm) == (char *) NULL) {
        return TUPLE_ERROR(ATOM("format_mismatch"));
    }

    return TUPLE_OK(TUPLE2(TUPLE3(INT(tm.tm_year),
                                  INT(tm.tm_mon),
                                  INT(tm.tm_mday)),
                           TUPLE3(INT(tm.tm_hour),
                                  INT(tm.tm_min),
                                  INT(tm.tm_sec))));
}

static ERL_NIF_TERM
tempo_strftime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM *dt_tuple, *d_tuple, *t_tuple, ret;
    int dt_tuple_len, d_tuple_len, t_tuple_len, buf_len;
    ErlNifBinary format, buf;
    struct tm tm;
    char format_str[255], buf_str[255];

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_inspect_binary(env, argv[0], &format)
        || format.size >= 255
        || !enif_is_tuple(env, argv[1])
        || !enif_get_tuple(env, argv[1], &dt_tuple_len, &dt_tuple)
        || dt_tuple_len != 2
        || !enif_is_tuple(env, dt_tuple[0])
        || !enif_get_tuple(env, dt_tuple[0], &d_tuple_len, &d_tuple)
        || d_tuple_len != 3
        || !enif_is_tuple(env, dt_tuple[1])
        || !enif_get_tuple(env, dt_tuple[1], &t_tuple_len, &t_tuple)
        || t_tuple_len != 3
        ) {
        return BADARG;
    }

    memset(&tm, 0, sizeof(struct tm));

    memcpy(format_str, format.data, format.size);
    format_str[format.size] = 0;

    if (!enif_get_int(env, d_tuple[0], &tm.tm_year)
        || !enif_get_int(env, d_tuple[1], &tm.tm_mon)
        || !enif_get_int(env, d_tuple[2], &tm.tm_mday)
        || !enif_get_int(env, t_tuple[0], &tm.tm_hour)
        || !enif_get_int(env, t_tuple[1], &tm.tm_min)
        || !enif_get_int(env, t_tuple[2], &tm.tm_sec)) {
        return BADARG;
    }

    buf_len = strftime(buf_str, 255, format_str, &tm);

    memcpy(enif_make_new_binary(env, buf_len, &ret),
           &buf_str, buf_len);

    return TUPLE_OK(ret);
}

static ErlNifFunc tempo_funcs[] =
{
    {"strptime", 2, tempo_strptime},
    {"strftime", 2, tempo_strftime}
};

ERL_NIF_INIT(tempo, tempo_funcs, NULL, NULL, NULL, NULL)
