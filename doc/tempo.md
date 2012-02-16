

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



* timestamp - Unix timestamp (positive integer denoting number of                 
seconds since 1 Jan 1970;   
* now       - erlang:now() format: tuple with 3 values, {MegaSeconds,                 
Seconds, MilliSeconds}, denoting number of MegaSeconds,                 
Seconds and MilliSeconds from 1 Jan 1970, respectivelly.   
* datetime  - calendar:datetime() format for GMT: tuple                 
{{Year, Month, Day}, {Hour, Minute, Seconds}}, denoting                 
GMT date and time.

There is also Format argument, which can be a binary (in the format
similar to strptime/strftime's) or one of following atoms: iso8601,
rfc1123, rfc2822. In the latter case standard format string will be used.
<a name="types"></a>

##Data Types##




###<a name="type-datetime_type">datetime_type()</a>##



<pre>datetime_type() = timestamp | now | datetime</pre>



###<a name="type-datetime_value">datetime_value()</a>##



<pre>datetime_value() = [unix_timestamp()](#type-unix_timestamp) | [erlang:timestamp()](erlang.md#type-timestamp) | [calendar:datetime()](calendar.md#type-datetime)</pre>



###<a name="type-format">format()</a>##



<pre>format() = binary() | iso8601 | rfc1123 | rfc2822</pre>



###<a name="type-unix_timestamp">unix_timestamp()</a>##



<pre>unix_timestamp() = pos_integer()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>Formats {Type, Datetime} tuple according to Format.</td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td>Formats Datetime according to Format.</td></tr><tr><td valign="top"><a href="#format_datetime-2">format_datetime/2</a></td><td>@equiv format(Format, Datetime, datetime).</td></tr><tr><td valign="top"><a href="#format_now-2">format_now/2</a></td><td>@equiv format(Format, Datetime, now).</td></tr><tr><td valign="top"><a href="#format_timestamp-2">format_timestamp/2</a></td><td>@equiv format(Format, Datetime, timestamp).</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parses {Type, Binary} tuple according to provided format, returns
ok/error tuples with datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td>Parses Binary according to Format and returns ok/error tuple with
datetime in format that depends on atom Type.</td></tr><tr><td valign="top"><a href="#parse_datetime-2">parse_datetime/2</a></td><td>@equiv parse(Format, Binary, datetime).</td></tr><tr><td valign="top"><a href="#parse_now-2">parse_now/2</a></td><td>@equiv parse(Format, Binary, now).</td></tr><tr><td valign="top"><a href="#parse_timestamp-2">parse_timestamp/2</a></td><td>@equiv parse(Format, Binary, timestamp).</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format-2"></a>

###format/2##




<pre>format(Format::[format()](#type-format), X2::{[datetime_type()](#type-datetime_type), [datetime_value()](#type-datetime_value)}) -&gt; {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Equivalent to [`format(Format, Datetime, Type)`](#format-3).

Formats {Type, Datetime} tuple according to Format. The way in which
Datetime will be handled depends on Type.<a name="format-3"></a>

###format/3##




<pre>format(Format::[format()](#type-format), Datetime::[datetime_value()](#type-datetime_value), Type::[datetime_type()](#type-datetime_type)) -&gt; {ok, binary()} | {error, invalid_time}</pre>
<br></br>




Formats Datetime according to Format. The way in which
Datetime will be handled depends on Type.<a name="format_datetime-2"></a>

###format_datetime/2##




<pre>format_datetime(Format::[format()](#type-format), Datetime::[calendar:datetime()](calendar.md#type-datetime)) -&gt; {ok, binary()} | {error, invalid_time}</pre>
<br></br>




@equiv format(Format, Datetime, datetime)<a name="format_now-2"></a>

###format_now/2##




<pre>format_now(Format::[format()](#type-format), X2::[erlang:timestamp()](erlang.md#type-timestamp)) -&gt; {ok, binary()} | {error, invalid_time}</pre>
<br></br>




@equiv format(Format, Datetime, now)<a name="format_timestamp-2"></a>

###format_timestamp/2##




<pre>format_timestamp(Format::[format()](#type-format), Timestamp::[unix_timestamp()](#type-unix_timestamp)) -&gt; {ok, binary()} | {error, invalid_time}</pre>
<br></br>




@equiv format(Format, Datetime, timestamp)<a name="parse-2"></a>

###parse/2##




<pre>parse(Format::[format()](#type-format), X2::{[datetime_type()](#type-datetime_type), binary()}) -&gt; {ok, [datetime_value()](#type-datetime_value)} | {error, format_mismatch}</pre>
<br></br>




Equivalent to [`parse(Format, DatetimeType, Binary)`](#parse-3).

Parses {Type, Binary} tuple according to provided format, returns
ok/error tuples with datetime in format that depends on atom Type.<a name="parse-3"></a>

###parse/3##




<pre>parse(Format::[format()](#type-format), Bin::binary(), Type::[datetime_type()](#type-datetime_type)) -&gt; {ok, [datetime_value()](#type-datetime_value)} | {error, format_mismatch}</pre>
<br></br>




Parses Binary according to Format and returns ok/error tuple with
datetime in format that depends on atom Type.<a name="parse_datetime-2"></a>

###parse_datetime/2##




<pre>parse_datetime(Format::[format()](#type-format), Bin::binary()) -&gt; {ok, [calendar:datetime()](calendar.md#type-datetime)} | {error, format_mismatch}</pre>
<br></br>




@equiv parse(Format, Binary, datetime)<a name="parse_now-2"></a>

###parse_now/2##




<pre>parse_now(Format::[format()](#type-format), Bin::binary()) -&gt; {ok, [erlang:timestamp()](erlang.md#type-timestamp)} | {error, format_mismatch}</pre>
<br></br>




@equiv parse(Format, Binary, now)<a name="parse_timestamp-2"></a>

###parse_timestamp/2##




<pre>parse_timestamp(Format::[format()](#type-format), Bin::binary()) -&gt; {ok, [unix_timestamp()](#type-unix_timestamp)} | {error, format_mismatch}</pre>
<br></br>




@equiv parse(Format, Binary, timestamp)