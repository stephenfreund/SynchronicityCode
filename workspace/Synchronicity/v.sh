#!/bin/bash

cpp -C -E -W -P $1 > a.anchor
shift
anchor $* a.anchor
