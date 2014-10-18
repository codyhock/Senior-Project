#!/bin/bash

php5 pullStats.php $1 $2

for i in $@
do
	mv build.exe $i
	cd $i
	mono build.exe $i
	mv build.exe ..
	mv $i.csv ../teams
	cd ..
done

bash eh.sh $1 $2
