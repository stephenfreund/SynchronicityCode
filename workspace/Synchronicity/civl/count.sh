#!/bin/bash

for file in `ls *.float *.bpl`; 
do
  count=`cpp -w -E -P < $file | grep -v "^//" | sed '/^\s*$/d' | wc -l`
  echo "$file: $count"
 
done

