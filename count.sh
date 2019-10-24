#!/bin/bash

for dir in $(find $1 -mindepth 1 -maxdepth 1 -type d)
do
	grep "SUCCESS" -l $dir/* | wc -l > $dir/count.txt
done
	
