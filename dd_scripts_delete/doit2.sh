#!/bin/bash

for line in `cat fix_nohtml.txt`;
do 
	echo '</html>' >> $line
done
