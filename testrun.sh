#!/bin/bash

mkdir -p $1/orgLex $1/1 $1/2 $1/3 $1/4 $1/5
for i in {0..100}
do

	lein run :parent-selection :lexicase > "$1/orgLex/$i.txt"
	lein run :parent-selection :summed :k 1 > "$1/1/$i.txt"
	lein run :parent-selection :summed :k 2 > "$1/2/$i.txt"
	lein run :parent-selection :summed :k 3 > "$1/3/$i.txt"
	lein run :parent-selection :summed :k 4 > "$1/4/$i.txt"
	lein run :parent-selection :summed :k 5 > "$1/5/$i.txt"
done

