#!/bin/bash
cd ../src
find . -name "*.f90" | while read f; do wc -l $f; done | awk '{ sum += $1 } END { print "Source files have " sum " lines of codes." }'
cd -
