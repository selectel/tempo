#include <string.h>
#include <time.h>
#include "erl_nif.h"

#define MAX_SIZE        255
#define ATOM_OK         enif_make_atom(env, "ok")
#define ATOM_ERROR      enif_make_atom(env, "error")
#define BADARG          enif_make_badarg(env)
#define TUPLE2(A, B)    enif_make_tuple2(env, A, B)
#define TUPLE3(A, B, C) enif_make_tuple3(env, A, B, C)
#define TUPLE_OK(A)     TUPLE2(ATOM_OK, A)
#define TUPLE_ERROR(A)  TUPLE2(ATOM_ERROR, A)
#define INT(A)          enif_make_int(env, A)
#define ATOM(A)         enif_make_atom(env, A)

inline unsigned enif_get_binary_str(const ErlNifBinary *bin, char *buf)
{
    unsigned ret = bin->size < MAX_SIZE;

    if (ret) {
        memcpy(buf, bin->data, bin->size);
        buf[bin->size] = 0;
    }

    return ret;
}

static ERL_NIF_TERM
tempo_strptime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary format, buf;
    char format_str[MAX_SIZE], buf_str[MAX_SIZE];
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_is_binary(env, argv[1])
        || !enif_inspect_binary(env, argv[0], &format)
        || !enif_inspect_binary(env, argv[1], &buf)
        || !enif_get_binary_str(&format, format_str)
        || !enif_get_binary_str(&buf, buf_str)) {
        return BADARG;
    }

    memset(&tm, 0, sizeof(struct tm));

    if (strptime(buf_str, format_str, &tm) == (char *) NULL) {
        return TUPLE_ERROR(ATOM("format_mismatch"));
    }

    return TUPLE_OK(INT(timegm(&tm)));
}

static ERL_NIF_TERM
tempo_strftime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM buf;
    ErlNifSInt64 clock;
    ErlNifBinary format;
    char format_str[MAX_SIZE], buf_str[MAX_SIZE];
    size_t buf_len;
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_inspect_binary(env, argv[0], &format)
        || !enif_is_number(env, argv[1])
        || !enif_get_int64(env, argv[1], &clock)
        || !enif_get_binary_str(&format, format_str)) {
        return BADARG;
    }

    memset(&tm, 0, sizeof(struct tm));

    if (gmtime_r((time_t *) &clock, &tm) == NULL) {
        return TUPLE_ERROR(ATOM("invalid_time"));
    }

    buf_len = strftime(buf_str, MAX_SIZE, format_str, &tm);
    memcpy(enif_make_new_binary(env, buf_len, &buf),
           &buf_str, buf_len);

    return TUPLE_OK(buf);
}

static ErlNifFunc tempo_funcs[] =
{
    {"strptime", 2, tempo_strptime},
    {"strftime", 2, tempo_strftime}
};

ERL_NIF_INIT(tempo, tempo_funcs, NULL, NULL, NULL, NULL)
