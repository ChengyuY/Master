#!/bin/bash

for FILE in aps3/*.aps
do 
	echo $FILE
	./evaluer $FILE
done
