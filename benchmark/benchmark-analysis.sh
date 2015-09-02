#!/bin/bash

nIslands=21

min=200
max=33600

loop=1

trap 'printf "\033[0m\n"; rm .tmpAnalysis; rm statsAnalysis/$(ls -t statsAnalysis | head -n 1); break' SIGINT
while true
do
  r=$(shuf -i $min-$max -n 1)
  let "nRows=$nIslands*$r+1"
  
  printf "Preparing benchmark with $r simulations…\n"
  
  ../Python/db_query.py -c "batch_paramRatio.date = '2015-08-26 01:59:16'" 2> /dev/null | head -n $nRows > .tmpAnalysis
  
  printf "Performing benchmark…\n\033[1;34m"
  
  statFileName=$(printf "%06d" $loop)-${r}-$(wc -c <".tmpAnalysis").stats
  /usr/bin/time -v -o statsAnalysis/$statFileName ../R/analysis.R -rv -a 'absolute, 0.1' ../Data/isea_admixture_data_for_comparison_2.csv ../Data/isea_admixture_data_for_comparison_2.csv .tmpAnalysis
  
  [ $? -ne 0 ] && exitColor="\033[1;31m" || exitColor="\033[1;32m"
  printf "${exitColor}…done\033[0m\n"
  
  let "loop++"
done

