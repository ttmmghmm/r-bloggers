#!/bin/sh
mserver5 --set prefix= --set exec_prefix= --dbpath /home/rstudio/r-bloggers/MonetDB/test --set mapi_port=50000 --daemon yes > /dev/null &
echo $! > /home/rstudio/r-bloggers/MonetDB/mserver5.started.from.R.pid
