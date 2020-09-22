#!/bin/bash

cpp -w -E -P < $1 | grep -v "^//" | sed '/^\s*$/d' | wc -l
