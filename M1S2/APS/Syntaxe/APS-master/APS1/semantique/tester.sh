#!/bin/bash

for FILE in aps1/*.aps
do 
	echo $FILE
	./evaluer $FILE
done
