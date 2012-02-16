

#Module tempo#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


NIF-based date and time parsing and formatting for Erlang.



__Authors:__ Dmitry Groshev ([`groshev@selectel.ru`](mailto:groshev@selectel.ru)), Sergey Levedev ([`superbobry@selectel.ru`](mailto:superbobry@selectel.ru)).<a name="description"></a>

##Description##


  
This module implements an interface to strptime/strftime with   
appropriate handling of Erlang datetime formats.



All exported functions in this module can throw "badarg" if   
malformed input is provided.



Some functions in this module takes atom Type as an input. It   
encodes an exact datetime format; here are possible Type values:



* unix     - Unix timestamp (positive integer denoting number of                
seconds since 1 Jan 1970;   
* now      - erlang:now() format: tuple with 3 values, {MegaSeconds,                
Seconds, MilliSeconds}, denoting number of MegaSeconds,                
Seconds and MilliSeconds from 1 Jan 1970, respectivelly.   
* datetime - calendar:datetime() format for GMT: tuple                
{{Year, Month, Day}, {Hour, Minute, Seconds}}, denoting                
GMT date and time.

There is also Format argument, which can be a binary (in the format
similar to strptime/strftime's) or one of following atoms: iso8601,
rfc1123, rfc2822. In the latter case standard format string will be used.
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>Formats {Type, Datetime} tuple according to Format.</td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td>Formats Datetime according to Format.</td></tr><tr><td valign="top"><a href="#format_datetime-2">format_datetime/2</a></td><td>Helper function similar to format/3.</td></tr><tr><td valign="top"><a href="#format_now-2">format_now/2</a></td><td>Helper function similar to format/3.</td></tr><tr><td valign="top"><a href="#format_unix-2">format_unix/2</a></td><td>Helper function similar to format/3.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parses {Type, Binary} tuple according to provided format, returns
ok/error tuples with datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td>Parses Binary according to Format and returns ok/error tuple with
datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse_datetime-2">parse_datetime/2</a></td><td>Helper function similar to parse/3.</td></tr><tr><td valign="top"><a href="#parse_now-2">parse_now/2</a></td><td>Helper function similar to parse/3.</td></tr><tr><td valign="top"><a href="#parse_unix-2">parse_unix/2</a></td><td>Helper function similar to parse/3.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format-2"></a>

###format/2##




<pre>format(Format::<a href="#type-format">format()</a>, X2::{<a href="#type-datetime_type">datetime_type()</a>, <a href="#type-datetime_value">datetime_value()</a>}) -> {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Equivalent to [`format(Format, Datetime, Type)`](#format-3).

Formats {Type, Datetime} tuple according to Format. The way in which
Datetime will be handled depends on Type.<a name="format-3"></a>

###format/3##




<pre>format(Format::<a href="#type-format">format()</a>, Datetime::<a href="#type-datetime_value">datetime_value()</a>, Type::<a href="#type-datetime_type">datetime_type()</a>) -> {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Formats Datetime according to Format. The way in which
Datetime will be handled depends on Type.<a name="format_datetime-2"></a>

###format_datetime/2##




<pre>format_datetime(Format::<a href="#type-format">format()</a>, Datetime::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -> {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Equivalent to [`format(Format, Datetime, datetime)`](#format-3).

Helper function similar to format/3.<a name="format_now-2"></a>

###format_now/2##




<pre>format_now(Format::<a href="#type-format">format()</a>, X2::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -> {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Equivalent to [`format(Format, Datetime, now)`](#format-3).

Helper function similar to format/3.<a name="format_unix-2"></a>

###format_unix/2##




<pre>format_unix(Format::<a href="#type-format">format()</a>, Timestamp::<a href="#type-unix_timestamp">unix_timestamp()</a>) -> {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Equivalent to [`format(Format, Datetime, timestamp)`](#format-3).

Helper function similar to format/3.<a name="parse-2"></a>

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

Helper function similar to parse/3.<a name="parse_now-2"></a>

###parse_now/2##




<pre>parse_now(Format::<a href="#type-format">format()</a>, Bin::binary()) -> {ok, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>} | {error, format_mismatch}</pre>
<br></br>




Equivalent to [`parse(Format, Binary, now)`](#parse-3).

Helper function similar to parse/3.<a name="parse_unix-2"></a>

###parse_unix/2##




<pre>parse_unix(Format::<a href="#type-format">format()</a>, Bin::binary()) -> {ok, <a href="#type-unix_timestamp">unix_timestamp()</a>} | {error, format_mismatch}</pre>
<br></br>




Equivalent to [`parse(Format, Binary, timestamp)`](#parse-3).

Helper function similar to parse/3.