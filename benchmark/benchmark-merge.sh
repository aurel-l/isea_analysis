#!/bin/bash

nIslands=21

min=1
max=33600

loop=1

trap 'printf "\033[0m\n"; rm .tmp; rm stats/$(ls -t stats | head -n 1); break' SIGINT
while true
do
  r=$(shuf -i $min-$max -n 1)
  let "nRows=$r+1"
  
  printf "Preparing benchmark with $r simulations…\n"
  
  head -n $nRows adm.batch_param_map.txt > .tmp
  
  printf "Performing benchmark…\n\033[1;34m"
  
  statFileName=$(printf "%06d" $loop)-${r}-$(wc -c <".tmp").stats
  /usr/bin/time -v -o stats/$statFileName ../R/merge.R -p adm.txt .tmp > /dev/null
  
  [ $? -ne 0 ] && exitColor="\033[1;31m" || exitColor="\033[1;32m"
  printf "${exitColor}…done\033[0m\n"
  
  let "loop++"
done

