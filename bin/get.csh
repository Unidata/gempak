#!/bin/csh

#  get.csh

#  Script to copy (tar) files from ~nawcm/nawips to ~nawopr


#  Verify that this script is being executed from nawopr account

if (`whoami` == 'nawopr') then

   cd  ~nawcm/nawips
   ls -aR
   echo "  "
   echo -n "Enter 'y' if these are the files to be moved to nawopr:  "
   set answer = $<

#  Copy (tar) from ~nawcm/nawips to ~nawopr

   if ($answer == 'y' ) then

      tar cvf - . | (cd; tar xvf -) 

   endif 

else

   echo "  "
   echo WARNING:
   echo This script MUST be executed from the nawopr account.
   echo "  "
 
endif
