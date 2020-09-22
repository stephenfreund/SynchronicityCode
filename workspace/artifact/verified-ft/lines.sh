#!/bin/bash

CIVL_FILES="verified-ft-code-only.bpl  \
 verified-ft-no-client.bpl "
ANCHOR_FILES="fasttrack-original.anchor  \
 ../../Synchronicity/benchmarks/anchor/fasttrack.anchor  \
 ../../Synchronicity/benchmarks/specs/fasttrack.anchor "

realpath() {
    python -c "import os; print(os.path.realpath('$1'))" | sed -e "s@"$HOME"@~@g"
}


echo ""
(
    printf "File LOC\n"
    printf "========================================================================== ===\n"
for file in $CIVL_FILES
do
  count=`cpp -w -E -P < $file | grep -v "^[ \t]*//" | sed '/^\s*$/d' | wc -l`
  printf "`realpath $file`  $count\n" 
done
printf " --------------------------------------------------------------------------\n"
for file in $ANCHOR_FILES
do
  count=`cpp -w -E -P < $file | grep -v "^[ \t]*//" | sed '/^\s*$/d' | wc -l`
  printf "`realpath $file`  $count\n" 
done
) | column -t
echo ""

