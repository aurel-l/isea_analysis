#!/bin/bash

echo 'run,Label,DnaAdmixture,AutosomeAdmixture,XChrAdmixture,MitoAdmixture,YChrAdmixture'

for f
do
    tail -n +2 $f | sed -r 's/^([0-9]+),\"([^"]+)\",,\"([^"]+)\"$/\1,\2\n\1,\3/' | tr -d \"
done

