#!/bin/sh

#exec cerl -debug -pa ebin deps/*/ebin -boot start_sasl \
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname tempo_dev +K true +A 10 \
    -setcookie blah
