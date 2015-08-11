#!/bin/bash

nIslands=21

min=50
max=7500
max=75

loop=1

trap 'printf "\033[0m\n"; rm .tmp2; rm stats/$(ls -t stats | head -n 1); break' SIGINT
while true
do
  r=$(shuf -i $min-$max -n 1)
  let "nRows=$nIslands*$r+1"
  
  printf "Preparing benchmark with $r simulations…\n"
  
  ../Python/db_query.py -c "batch_param.date = '2015-07-08 02:34:37' AND startingDistributionFile = 'starting_distribution_3.csv' AND poissonMean = 3.6" 2> /dev/null | head -n $nRows > .tmp2
  
  printf "Performing benchmark…\n\033[1;34m"
  
  statFileName=$(printf "%06d" $loop)-${r}-$(wc -c <".tmp2").stats
  /usr/bin/time -v -o stats/$statFileName ../R/analysis.R -rv ../Data/isea_admixture_data_for_comparison_2.csv ../Data/isea_admixture_data_for_comparison_2.csv .tmp2
  
  [ $? -ne 0 ] && exitColor="\033[1;31m" || exitColor="\033[1;32m"
  printf "${exitColor}…done\033[0m\n"
  
  let "loop++"
  break
done

