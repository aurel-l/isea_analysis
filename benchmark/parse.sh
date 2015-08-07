#!/bin/bash

regexFileName='^0*([1-9][0-9]*)-([0-9]+)-([0-9]+)\.stats$'
regexTime='([1-5]?[0-9]):([0-9]{2})\.([0-9]{2})$'
regexMem='([0-9]+)$'
regexExit='([0-1])$'

echo 'index, nSimulations, inputFileSize, time, maxMem, failed'

for fileName in $(ls stats)
do
  if [[ -s stats/$fileName ]]
  then
    [[ $fileName =~ $regexFileName ]]
    inputFileSize=$(bc -l <<< "${BASH_REMATCH[3]}/1024/1024")
    printf "${BASH_REMATCH[1]}, ${BASH_REMATCH[2]}, $inputFileSize, "
    
    time=$(grep "wall clock" stats/$fileName)
    [[ $time =~ $regexTime ]]
    seconds=$(bc -l <<< "${BASH_REMATCH[1]}*60+${BASH_REMATCH[2]}")
    printf "${seconds}.${BASH_REMATCH[3]}, "
    
    mem=$(grep "Maximum resident set size" stats/$fileName)
    [[ $mem =~ $regexMem ]]
    megs=$(bc -l <<< "${BASH_REMATCH[1]}/1024")
    printf "${megs}, "
    
    exit=$(grep "Exit status" stats/$fileName)
    [[ $exit =~ $regexExit ]]
    printf "${BASH_REMATCH[1]}"
    
    printf "\n"
  fi
done

