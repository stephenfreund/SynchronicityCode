#!/bin/bash

ANCHOR_FILES="queue-original.anchor  \
 ../../Synchronicity/benchmarks/specs/queue.anchor "

realpath() {
    python -c "import os; print(os.path.realpath('$1'))" | sed -e "s@"$HOME"@~@g"
}


echo ""
(
    printf "File LOC\n"
    printf "========================================================================== ===\n"
for file in $ANCHOR_FILES
do
  count=`cpp -w -E -P < $file | grep -v "^[ \t]*//" | sed '/^\s*$/d' | wc -l`
  printf "`realpath $file`  $count\n" 
done
) | column -t
echo ""

