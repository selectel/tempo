

#tempo [![Build Status](https://secure.travis-ci.org/selectel/tempo.png)](http://travis-ci.org/selectel/tempo)#


__Authors:__ Dmitry Groshev ([`groshev@selectel.ru`](mailto:groshev@selectel.ru)), Sergei Levedev ([`lebedev@selectel.ru`](mailto:lebedev@selectel.ru)).


`tempo` is a library for parsing and formatting dates in
Erlang. It provides a clean and nice interface to libc's
[strptime](http://linux.die.net/man/3/strptime) and
[strftime](http://linux.die.net/man/3/strftime) functions,
which are unfortunately missing from Erlang's standard library.

###<a name="Is_it_any_good?">Is it any good?</a>##



Yes.

###<a name="How_can_I_use_it?">How can I use it?</a>##


The only two functions you have to remember are [`tempo:parse/2`](https://github.com/selectel/tempo/blob/master/doc/tempo.md#parse-2)
and [`tempo:format/2`](https://github.com/selectel/tempo/blob/master/doc/tempo.md#format-2). Here are some examples:<pre>(tempo_dev@localhost)1> {ok, Bin} = tempo:format(iso8601, {now, now()}).
{ok,<<"2012-06-01T19:06:420000">>}
(tempo_dev@localhost)2> tempo:parse(iso8601, {datetime, Bin}).
{ok,{{2012,6,1},{19,6,42}}}</pre>

As you might have noticed, both of the functions follow a common
pattern -- *Format* first, then a *Value*, tagged by its actual
or expected type. Predefined formats include: `iso8601`, `rfc1123`,
and `rfc2822`, but in fact, you can use any format, as long as it
follows libc conventions:<pre>(tempo_dev@localhost)1> {ok, Bin} = tempo:format(<<"%A, %Y-%d-%m">>, {now, now()}).
{ok,<<"Thursday, 2012-07-06">>}</pre>


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/selectel/tempo/blob/master/doc/tempo.md" class="module">tempo</a></td></tr></table>

