#! /bin/bash

for f in aps2/*.aps 
do
	echo $f ;
	( ./toProlog $f)| swipl -s typageAPS2.pl -g main_stdin
done
