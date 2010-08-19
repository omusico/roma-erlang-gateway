#!/bin/sh
cd `dirname $0`

rm mnesia/*

# for test
exec erl -mnesia dir '"mnesia"' -pa $PWD/ebin $PWD/deps/*/ebin -name romacd_0@127.0.0.1 -boot start_sasl +A 64 -s romacd

# for product
#erl -mnesia dir '"mnesia"' -pa $PWD/ebin $PWD/deps/*/ebin -name romacd_0@127.0.0.1 -boot start_sasl +A 64 -noshell -detached -s romacd
echo "*** started roma client proxy daemon ***"

