

#Module tempo#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


NIF-based date and time parsing and formatting for Erlang.

Copyright (c) 2012 Selectel Ltd.


__Authors:__ Dmitry Groshev ([`groshev@selectel.ru`](mailto:groshev@selectel.ru)), Sergey Levedev ([`lebedev@selectel.ru`](mailto:lebedev@selectel.ru)).<a name="description"></a>

##Description##



This module implements an interface to strptime/strftime with
appropriate handling of Erlang datetime formats.



All exported functions in this module can throw `badarg` if
malformed input is provided.



A _Type_ argument, accepted by some of the exported functions
should be one of the following:

<pre>   | Type     | Description                                        |
   |----------+----------------------------------------------------|
   | unix     | UNIX timestamp, a positive integer denoting number |
   |          | of seconds since 1 Jan 1970.                       |
   | now      | @see erlang:now/0                                  |
   | datetime | @see calendar:datetime/0                           |</pre>



A _Format_ argument to any of the exported functions is
either a `binary()` with strptime/strftime compatible tokens or
one of the following atoms: iso8601, rfc1123, rfc2822. In the latter
case a predefined format will be used.



*A note about 32-bit systems*

Functions of "format" family can return "{error, time_overflow}" if
the underlying 32-bit value overflows. This is presumably possible only
on 32-bit systems. Minimum datetime for such systems is
`{{1901,12,13},{20,45,52}}` and maximum is `{{2038,1,19},{3,14,7}}`.

<a name="types"></a>

##Data Types##




###<a name="type-datetime_type">datetime_type()</a>##



<pre>datetime_type() = unix | now | datetime</pre>



###<a name="type-datetime_value">datetime_value()</a>##



<pre>datetime_value() = <a href="#type-unix_timestamp">unix_timestamp()</a> | <a href="erlang.md#type-timestamp">erlang:timestamp()</a> | <a href="calendar.md#type-datetime">calendar:datetime()</a></pre>



###<a name="type-format">format()</a>##



<pre>format() = binary() | iso8601 | rfc1123 | rfc2822</pre>



###<a name="type-unix_timestamp">unix_timestamp()</a>##



<pre>unix_timestamp() = pos_integer()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>Formats {Type, Datetime} tuple according to Format.</td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td>Formats Datetime according to Format.</td></tr><tr><td valign="top"><a href="#format_datetime-2">format_datetime/2</a></td><td>Helper function similar to <a href="#format-3"><code>format/3</code></a>.</td></tr><tr><td valign="top"><a href="#format_now-2">format_now/2</a></td><td>Helper function similar to <a href="#format-3"><code>format/3</code></a>.</td></tr><tr><td valign="top"><a href="#format_unix-2">format_unix/2</a></td><td>Helper function similar to <a href="#format-3"><code>format/3</code></a>.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parses {Type, Binary} tuple according to provided format, returns
ok/error tuples with datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td>Parses Binary according to Format and returns ok/error tuple with
datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse_datetime-2">parse_datetime/2</a></td><td>Helper function similar to <a href="#parse-3"><code>parse/3</code></a>.</td></tr><tr><td valign="top"><a href="#parse_now-2">parse_now/2</a></td><td>Helper function similar to <a href="#parse-3"><code>parse/3</code></a>.</td></tr><tr><td valign="top"><a href="#parse_unix-2">parse_unix/2</a></td><td>Helper function similar to <a href="#parse-3"><code>parse/3</code></a>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format-2"></a>

###format/2##


<pre>format(Format::<a href="#type-format">format()</a>, X2::{<a href="#type-datetime_type">datetime_type()</a>, <a href="#type-datetime_value">datetime_value()</a>}) -> {ok, binary()} | {error, invalid_time} | {error, time_overflow}</pre>
<br></br>


Equivalent to [`format(Format, Datetime, Type)`](#format-3).

Formats {Type, Datetime} tuple according to Format. The way in which
Datetime will be handled depends on Type.<a name="format-3"></a>

###format/3##


<pre>format(Format::<a href="#type-format">format()</a>, Datetime::<a href="#type-datetime_value">datetime_value()</a>, Type::<a href="#type-datetime_type">datetime_type()</a>) -> {ok, binary()} | {error, invalid_time} | {error, time_overflow}</pre>
<br></br>


Formats Datetime according to Format. The way in which
Datetime will be handled depends on Type.<a name="format_datetime-2"></a>

###format_datetime/2##


<pre>format_datetime(Format::<a href="#type-format">format()</a>, Datetime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -> {ok, binary()} | {error, invalid_time} | {error, time_overflow}</pre>
<br></br>


Equivalent to [`format(Format, Datetime, datetime)`](#format-3).

Helper function similar to [`format/3`](#format-3).<a name="format_now-2"></a>

###format_now/2##


<pre>format_now(Format::<a href="#type-format">format()</a>, X2::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -> {ok, binary()} | {error, invalid_time} | {error, time_overflow}</pre>
<br></br>


Equivalent to [`format(Format, Datetime, now)`](#format-3).

Helper function similar to [`format/3`](#format-3).<a name="format_unix-2"></a>

###format_unix/2##


<pre>format_unix(Format::<a href="#type-format">format()</a>, Timestamp::<a href="#type-unix_timestamp">unix_timestamp()</a>) -> {ok, binary()} | {error, invalid_time} | {error, time_overflow}</pre>
<br></br>


Equivalent to [`format(Format, Datetime, timestamp)`](#format-3).

Helper function similar to [`format/3`](#format-3).<a name="parse-2"></a>

###parse/2##


<pre>parse(Format::<a href="#type-format">format()</a>, X2::{<a href="#type-datetime_type">datetime_type()</a>, binary()}) -> {ok, <a href="#type-datetime_value">datetime_value()</a>} | {error, format_mismatch}</pre>
<br></br>


Equivalent to [`parse(Format, DatetimeType, Binary)`](#parse-3).

Parses {Type, Binary} tuple according to provided format, returns
ok/error tuples with datetime in format that depends on atom Type.<a name="parse-3"></a>

###parse/3##


<pre>parse(Format::<a href="#type-format">format()</a>, Bin::binary(), Type::<a href="#type-datetime_type">datetime_type()</a>) -> {ok, <a href="#type-datetime_value">datetime_value()</a>} | {error, format_mismatch}</pre>
<br></br>


Parses Binary according to Format and returns ok/error tuple with
datetime in format that depends on atom Type.<a name="parse_datetime-2"></a>

###parse_datetime/2##


<pre>parse_datetime(Format::<a href="#type-format">format()</a>, Bin::binary()) -> {ok, <a href="calendar.md#type-datetime">calendar:datetime()</a>} | {error, format_mismatch}</pre>
<br></br>


Equivalent to [`parse(Format, Binary, datetime)`](#parse-3).

Helper function similar to [`parse/3`](#parse-3).<a name="parse_now-2"></a>

###parse_now/2##


<pre>parse_now(Format::<a href="#type-format">format()</a>, Bin::binary()) -> {ok, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>} | {error, format_mismatch}</pre>
<br></br>


Equivalent to [`parse(Format, Binary, now)`](#parse-3).

Helper function similar to [`parse/3`](#parse-3).<a name="parse_unix-2"></a>

###parse_unix/2##


<pre>parse_unix(Format::<a href="#type-format">format()</a>, Bin::binary()) -> {ok, <a href="#type-unix_timestamp">unix_timestamp()</a>} | {error, format_mismatch}</pre>
<br></br>


Equivalent to [`parse(Format, Binary, timestamp)`](#parse-3).

Helper function similar to [`parse/3`](#parse-3).