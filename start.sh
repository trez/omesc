#!/bin/sh
cd `dirname $0`
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname omesc \
  -eval 'io:format("*************************************** ~n"), 
         application:start(omesc)'
