#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
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


static inline char *
expand_nseconds(const size_t n, const double usecs)
{
    char *buf = calloc(n + 1, sizeof(char));  /* +1 for the trailing '\0'. */
    char *fmt = NULL;
    double multiplier = 0.0;

    switch (n) {
    case 3:
        fmt = "%03d";
        multiplier = 1e3;
        break;
    case 6:
        fmt = "%06d";
        multiplier = 1e6;
        break;
    default:
        fmt = "%09d";
        multiplier = 1e9;
    }

    assert(sprintf(buf, fmt, (int) (usecs * multiplier)) > 0);
    return buf;
}


static inline void
expand_custom_formatters(char *fmt_str, const double usecs)
{
    char *new_fmt_str = calloc(MAX_SIZE, sizeof(char));
    size_t new_size = 0;
    char ch = 0;
    size_t ntoappend = 0;
    int should_free = 0;
    const char *ptoappend = NULL;
    const char *pin = fmt_str;

    while ((ch = *pin++) != '\0') {
        should_free = 0;

        if (ch == '%') {
            if ((ch = *pin++) == '\0') {
                /* a standalone '%', report failure? */
                break;
            } else if (isdigit(ch) && pin[0] == 'N') {
                ptoappend = expand_nseconds(ch - '0', usecs);
                ntoappend = strlen(ptoappend);
                should_free = 1;
                ++pin;
            } else {
                /* percent followed by an unknown formatter. */
                ptoappend = pin - 2;
                ntoappend = 2;
            }
        } else {
            ptoappend = pin - 1;
            ntoappend = 1;
        }

        assert(ptoappend != NULL);
        if (ntoappend > 0) {
            memcpy(new_fmt_str + new_size, ptoappend, ntoappend);
            new_size += ntoappend;
        }

        if (should_free) {
            free((char *) ptoappend);
        }
    }

    memcpy(fmt_str, new_fmt_str, new_size);
    free(new_fmt_str);
}


static ERL_NIF_TERM
tempo_strptime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res;
    ErlNifBinary fmt;
    ErlNifBinary buf;
    ErlNifSInt64 clock = 0;
    char fmt_str[MAX_SIZE] = {0};
    char buf_str[MAX_SIZE] = {0};
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_is_binary(env, argv[1])
        || !enif_inspect_binary(env, argv[0], &fmt)
        || !enif_inspect_binary(env, argv[1], &buf)
        || !fmt.size || !buf.size
        || !enif_get_binary_str(&fmt, fmt_str)
        || !enif_get_binary_str(&buf, buf_str)) {
        res = BADARG;
    } else  {
        /* HACK(Sergei): since 'libc' doesn't provide us with a way to
           properly initialize 'tm' struct, we default it to '0' UNIX
           time.
        */
        memset(&tm, 0, sizeof(struct tm));
        gmtime_r((const time_t *) &clock, &tm);

        if (strptime(buf_str, fmt_str, &tm) == NULL) {
            res = TUPLE_ERROR(ATOM("format_mismatch"));
        } else {
            clock = timegm(&tm);
            res = TUPLE_OK(enif_make_double(env, clock));
        }
    }

    return res;
}


static ERL_NIF_TERM
tempo_strftime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res;
    ERL_NIF_TERM buf;
    double clock_raw;
    double usecs;
    time_t clock;
    ErlNifBinary format;
    char fmt_str[MAX_SIZE] = {0};
    char buf_str[MAX_SIZE] = {0};
    size_t buf_len;
    int overflow;
    struct tm tm;

    if (argc != 2
        || !enif_is_binary(env, argv[0])
        || !enif_inspect_binary(env, argv[0], &format)
        || !format.size  /* disallow empty format. */
#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 3
        || !enif_is_number(env, argv[1])
#endif
        || !enif_get_double(env, argv[1], &clock_raw)
        || !enif_get_binary_str(&format, fmt_str)) {
        res = BADARG;
    } else {
        usecs = clock_raw - ((ErlNifSInt64) clock_raw);

        overflow = 0;
        clock = int64_to_time_t((ErlNifSInt64) clock_raw, &overflow);
        if (overflow != 0.) {
            /* HACK(Sergei): even though the exact type of 'time_t' is
               unspecified, on most systems it seem to be a plain 'int'.
            */
            res = TUPLE_ERROR(ATOM("time_overflow"));
        } else {
            memset(&tm, 0, sizeof(struct tm));
            if (!gmtime_r(&clock, &tm)) {
                res = TUPLE_ERROR(ATOM("invalid_time"));
            } else {
                expand_custom_formatters(fmt_str, usecs);

                buf_len = strftime(buf_str, MAX_SIZE, fmt_str, &tm);
                memcpy(enif_make_new_binary(env, buf_len, &buf),
                       &buf_str, buf_len);
                res = TUPLE_OK(buf);
            }
        }
    }

    return res;
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
