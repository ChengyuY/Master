#!/bin/bash

for FILE in aps2/*.aps
do 
	echo $FILE
	./evaluer $FILE
done
