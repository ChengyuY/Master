#! /bin/bash

for f in aps0/*.aps 
do
	echo $f ;
	( ./toProlog $f)| swipl -s typageAPS0.pl -g main_stdin
done
