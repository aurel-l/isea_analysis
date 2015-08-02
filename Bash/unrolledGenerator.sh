#!/bin/bash

f=$1

table=$(tail -n +2 $f | cut -d ',' -f 6,10 | sort | uniq -c | tr -s ' ' ',' | cut -d ',' -f 2-4)

run=1
for r in $table
do
    n=$(echo $r | cut -d ',' -f 1)
    poissonMean=$(echo $r | cut -d ',' -f 2)
    marriageThres=$(echo $r | cut -d ',' -f 3)
    
    missing=$((300 - n))
    echo n $n pm $poissonMean mt $marriageThres missing $missing
    for i in $(seq 1 $missing)
    do
        echo -en "$run\t" >> unrolledParamFile.txt
        echo -en "migrationProb\t0.1," >> unrolledParamFile.txt
        echo -en "asianDefinition\t0.5," >> unrolledParamFile.txt
        echo -en "startingDistributionFile\t../data/starting_distribution_real_mainland.csv," >> unrolledParamFile.txt
        echo -en "poissonMean\t$poissonMean," >> unrolledParamFile.txt
        echo -en "initialDemeAgentNumber\t120," >> unrolledParamFile.txt
        echo -en "graphFile\t../data/isea_gcd650.xls," >> unrolledParamFile.txt
        echo -en "growthRate\t2.0E-4," >> unrolledParamFile.txt
        echo -en "marriageThres\t$marriageThres," >> unrolledParamFile.txt
        echo -e "randomSeed\t$(od -vAn -N4 -tu4 < /dev/urandom | tr -d ' ' | cut -c 1-9)" >> unrolledParamFile.txt
        run=$((run + 1))
    done
done

