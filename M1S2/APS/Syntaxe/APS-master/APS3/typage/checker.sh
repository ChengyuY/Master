#! /bin/bash

for f in aps3/*.aps 
do
	echo $f ;
	( ./toProlog $f)| swipl -s typageAPS3.pl -g main_stdin
done
