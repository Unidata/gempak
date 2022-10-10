#!/bin/bash

for line in `cat fix_nobody.txt`;
do 
	echo '</body>' >> $line
	echo '</html>' >> $line
done
