#! /bin/bash

for f in aps1/*.aps 
do
	echo $f ;
	( ./toProlog $f)| swipl -s typageAPS1.pl -g main_stdin
done
