#!/bin/bash

for FILE in aps0/*.aps
do 
	echo $FILE
	./evaluer $FILE
done
