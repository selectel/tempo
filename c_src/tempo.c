#include <assert.h>
#include <string.h>
#include <time.h>
#include "erl_nif.h"

#define MAX_SIZE        255
#define ATOM_OK         ATOM("ok")
#define ATOM_ERROR      ATOM("error")
#define BADARG          enif_make_badarg(env)
#define TUPLE2(A, B)    enif_make_tuple2(env, A, B)
#define TUPLE_OK(A)     TUPLE2(ATOM_OK, A)
#define TUPLE_ERROR(A)  TUPLE2(ATOM_ERROR, A)
#define ATOM(A)         enif_make_atom(env, A)
#define UNUSED          __attribute__((unused))

extern char *strptime(const char *s, const char *format, struct tm *tm);
extern size_t strftime(char *s, size_t max, const char *format,
                       const struct tm *tm);


static inline unsigned
enif_get_binary_str(const ErlNifBinary *bin, char *buf)
{
    unsigned ret = bin->size < MAX_SIZE;

    if (ret) {
        memcpy(buf, bin->data, bin->size);
        buf[bin->size] = 0;
    }

    return ret;
}


static inline time_t
int64_to_time_t(ErlNifSInt64 clock, int *overflow)
{
    time_t result;
    ErlNifSInt64 diff;

    result = (time_t) clock;
    diff = clock - (ErlNifSInt64) result;
    *overflow = diff <= -1 || diff >= 1;

    return result;
}


static inline void
tempo_expand_custom_formatters(UNUSED const char *fmt_str,
                               UNUSED const struct tm *tm,
                               UNUSED const double usecs)
{
}


static ERL_NIF_TERM
tempo_strptime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary format, buf;
    ErlNifSInt64 clock = 0;
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

    /* HACK(Sergei): since 'libc' doesn't provide us with a way to properly
       initialize 'tm' struct, we default it to '0' UNIX time. */
    memset(&tm, 0, sizeof(struct tm));
    gmtime_r((const time_t *) &clock, &tm);

    if (strptime(buf_str, format_str, &tm) == NULL) {
        return TUPLE_ERROR(ATOM("format_mismatch"));
    }

    clock = timegm(&tm);
    return TUPLE_OK(enif_make_double(env, clock));
}


static ERL_NIF_TERM
tempo_strftime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM buf;
    double clock_raw;
    double usecs;
    time_t clock;
    ErlNifBinary format;
    char fmt_str[MAX_SIZE], buf_str[MAX_SIZE];
    size_t buf_len;
    int overflow;
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_inspect_binary(env, argv[0], &format)
#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 3
        || !enif_is_number(env, argv[1])
#endif
        || !enif_get_double(env, argv[1], &clock_raw)
        || !enif_get_binary_str(&format, fmt_str)) {
        return BADARG;
    }

    usecs = clock_raw - ((ErlNifSInt64) clock_raw);

    overflow = 0;
    clock = int64_to_time_t((ErlNifSInt64) clock_raw, &overflow);
    if (overflow != 0.)
        /* HACK(Sergei): even though the exact type of 'time_t' is
           unspecified, on most systems it seem to be a plain 'int'. */
        return TUPLE_ERROR(ATOM("time_overflow"));

    memset(&tm, 0, sizeof(struct tm));
    if (!gmtime_r(&clock, &tm)) {
        return TUPLE_ERROR(ATOM("invalid_time"));
    }

    /* FIXME(Sergei): stub! */
    tempo_expand_custom_formatters(fmt_str, &tm, usecs);

    buf_len = strftime(buf_str, MAX_SIZE, fmt_str, &tm);
    memcpy(enif_make_new_binary(env, buf_len, &buf),
           &buf_str, buf_len);

    return TUPLE_OK(buf);
}


static int
load(ErlNifEnv* env UNUSED,
     void** priv UNUSED,
     ERL_NIF_TERM info UNUSED)
{
    return 0;
}


static int
reload(ErlNifEnv* env UNUSED,
       void** priv UNUSED,
       ERL_NIF_TERM info UNUSED)
{
    return 0;
}


static int
upgrade(ErlNifEnv* env UNUSED,
        void** priv,
        void** old_priv,
        ERL_NIF_TERM info UNUSED)
{
    *priv = *old_priv;
    return 0;
}


static void
unload(ErlNifEnv* env UNUSED,
       void* priv)
{
    enif_free(priv);
    return;
}


static ErlNifFunc tempo_funcs[] =
{
    {"strptime", 2, tempo_strptime},
    {"strftime", 2, tempo_strftime}
};


ERL_NIF_INIT(tempo, tempo_funcs, &load, &reload, &upgrade, &unload)
