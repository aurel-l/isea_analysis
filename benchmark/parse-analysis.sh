#!/bin/bash

regexFileName='^0*([1-9][0-9]*)-([0-9]+)-([0-9]+)\.stats$'
regexTime1=' ([1-5]?[0-9]):([0-9]{2})\.?([0-9]{2})$'
regexTime2=' ([1-9]):([0-5]?[0-9]):([0-9]{2})$'
regexMem='([0-9]+)$'
regexExit='([0-1])$'

echo 'index, nSimulations, inputFileSize, time, maxMem, failed'

for fileName in $(ls statsAnalysis)
do
  if [[ -s statsAnalysis/$fileName ]]
  then
    [[ $fileName =~ $regexFileName ]]
    inputFileSize=$(bc -l <<< "${BASH_REMATCH[3]}/1000/1000")
    printf "${BASH_REMATCH[1]}, ${BASH_REMATCH[2]}, $inputFileSize, "
    
    time=$(grep "wall clock" statsAnalysis/$fileName)
    if [[ $time =~ $regexTime1 ]]
    then
      seconds=$(bc -l <<< "${BASH_REMATCH[1]}*60+${BASH_REMATCH[2]}")
      printf "${seconds}.${BASH_REMATCH[3]}, "
    else
      [[ $time =~ $regexTime2 ]]
      seconds=$(bc -l <<< "${BASH_REMATCH[1]}*3600+${BASH_REMATCH[2]}*60+${BASH_REMATCH[3]}")
      printf "${seconds}, "
    fi
    
    mem=$(grep "Maximum resident set size" statsAnalysis/$fileName)
    [[ $mem =~ $regexMem ]]
    megs=$(bc -l <<< "${BASH_REMATCH[1]}/1000")
    printf "${megs}, "
    
    exit=$(grep "Exit status" statsAnalysis/$fileName)
    [[ $exit =~ $regexExit ]]
    printf "${BASH_REMATCH[1]}"
    
    printf "\n"
  fi
done

