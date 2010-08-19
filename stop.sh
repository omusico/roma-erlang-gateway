#!/bin/sh
echo "stop roma-client-proxy-daemon ? [y/n]"
read ANS
if [ $ANS = 'y' -o $ANS = 'yes' ]; then
  cd `dirname $0`
  exec erl -name romacdman_0@127.0.0.1 -pa ebin -tnode romacd_0@127.0.0.1 -s romacd_admin stop
fi

