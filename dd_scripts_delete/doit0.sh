#!/bin/bash

for line in `cat fix_htmlplus.txt`;
do 
	sed -i 's|<!DOCTYPE htmlplus PUBLIC "-//Internet/RFC xxxx//EN">|<!DOCTYPE html>|g' $line
	sed -i 's|<htmlplus>|<html>|gi' $line
done
