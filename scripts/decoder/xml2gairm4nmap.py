#!/usr/bin/python

from __future__ import print_function
import argparse
import sys
import os
import re
import time
from datetime import datetime
from time import strftime, gmtime
from shutil import copyfile, rmtree
from lxml import etree as et

##
# Usage: xml2gairm4nmap.py -i <xmlfile> --ofmt <outfilename>
#          [--outhour <hh>] [--offsetiss <hours>] [--append]
#
#    where -i is the input XML document
#    where --ofmt is the format string or flat filename.
#    where --outhour is the 2-digit hour the text is valid used 
#      in formatting the output filename.
#    where --offsetiss offsets the issue time by X hours.
#      This allows for time-binned displays in nmap2.
#    where --append is used to append the output to a pre-existing file.
#
# Purpose: This script builds airm text files for snapshots, to be 
#          rendered via NMAP2 MISC/GAIRM option...
#          When managed by gairm_metwatch_processor.pl, this script
#             is used to build a single file "airmet.txt" which houses
#             the latest output from SIERRA/TANGO/ZULU hazards.
# Inputs: PreXML documents from the GFA BUFR Encoder...
# Outputs:  GEMPAK AIRMET Text files.  Formatted filenames can be
#    produced,(Example) YYYYMMDD_HH.airm. 
#
# Author:  Larry Hinson AWC, 8/06
#          Revised           4/10
#          Replaced xml2gairm4nmap.pl 3/23
##

def main():
  usageStatement = "Usage: xml2gairm4nmap.py -i <xmlfile> [--outhour <hh>]\n \
                     [--ofmt <output fmt string>] [--offsetisshr <hours>] \n \
                     [--append] [--updatenum <number>] [--bulletin <S|T|Z>]"
  xrefHazardToType = { 'IFR':'IR',
                       'MT_OBSC':'MO',
                       'ICE':'IC',
                       'TURB-HI':'TB',
                       'TURB-LO':'TB',
                       'SFC_WND':'SW',
                       'LLWS':'WS',
                       'M_FZLVL':'MFZL',
                       'FZLVL':'FZL' }
                     

  if len(sys.argv) < 2:
    print(usageStatement)
    exit(1)
  p = argparse.ArgumentParser()
  p.add_argument('-i','--input',action='store',help='Input XML File',default='Not Set')
  p.add_argument('-o','--outhour',action='store',help='Out Hour',default='Not Set')
  p.add_argument('-f','--ofmt',action='store',help='Output name format string',default='Not Set')
  p.add_argument('-s','--offsetisshr',action='store',help='Offset hours',default='Not Set')
  p.add_argument('-a','--append',action='store_true',help="append to gempak file",default='Not Set')
  p.add_argument('-u','--updatenum',action='store',help="Update Number",default='Not Set')
  p.add_argument('-b','--bulletin',action='store',help="Bulletin S|T|Z",default='Not Set')
  
  args = p.parse_args()
  inputfile = args.input
  outhour = args.outhour
  outhoursw = True
  if outhour == 'Not Set':
    outhoursw = False
  ofmt = args.ofmt
  outfilesw = True
  if ofmt == 'Not Set':
    outputfmt = ""
    outfilesw = False
  else:
    outputfmt = args.ofmt
  offsetisshr = args.offsetisshr
  if offsetisshr == 'Not Set':
    offsetisshr = 0
  appendsw = args.append
  updatenum = args.updatenum
  bulletin = args.bulletin
  cancel = correction = False
  iseqsuffix = 80
  
  if inputfile == 'Not Set' and bulletin == 'Not Set':
    print(usageStatement)
    exit(1)
    
  if re.match("%H",outputfmt) and outhoursw:
    outputfmt = outputfmt.replace("%H",outhour)

  #From Output format, build outputfile name...
  #Assume base format of form "%Y%m%d_%H.airm
  #Replace Hour with $outhour
  original_stdout=sys.stdout
  if ofmt != 'Not Set':  
    outputfile =  time.strftime(outputfmt,time.gmtime())
    if appendsw:
      outfh = open(outputfile,"a")
    else:
      outfh = open(outputfile,"w")
    sys.stdout = outfh  
  # Now parse inputfile...
  
  g = et.parse(inputfile)
  issueTime = g.find("hdr/issueTime").text
  untilTime = g.find("hdr/untilTime").text
  snaps = g.findall("smear")
  for snap in snaps:
    hazard = snap.find("hazard").text
    tag = snap.find("tag").text
    airstype = xrefHazardToType[hazard]
    if re.match(r"MFZL|FZL",airstype):
      break
    issTime = "%s/%s" % (issueTime[2:8],issueTime[8:12])
    validFrom = snap.find('validFrom').text
    startTime = "%s/%s" % (validFrom[2:8],validFrom[8:12])
    validUntil = snap.find('validUntil').text
    stopTime = "%s/%s" % (validUntil[2:8],validUntil[8:12])
    if startTime == stopTime:
      fltlvl1 = ""
      fltlvl2 = ""
      hhmm = validFrom[8:12]
      hhmindiff = "%04d" % (int(hhmm) % 300)
      hh=hhmindiff[0:2]
      MM=hhmindiff[2:4]
      hhminfract = int(hh)+int(MM)/60.0
      # Obtain a new End Time, by subtracting hhminfract from starttime and
      #  add 3 hours.
      epochtimestart = int(datetime.strptime(validFrom,"%Y%m%d%H%M").strftime('%s'))
      epochtimestop = epochtimestart - (hhminfract*3600) + (3600*3) - 60
      stopTime = time.strftime("%y%m%d/%H%M",time.localtime(epochtimestop))
      
      status = snap.find('Status').text
      if re.match(r"TB|IC",airstype):
        fltlvl1 = snap.find('Base').text
        fltlvl2 = snap.find('Top').text
      # Write the Airmet Text Line for NMAP:
      sys.stdout.write("|%s|%s|%s|%s|%s%02d|%s|%s|%s|%d|%d\n" % (airstype,issTime,
         startTime,stopTime,'KCI',iseqsuffix,updatenum,fltlvl1,fltlvl2,
         correction,cancel))
      latpts = snap.find('latPts').text
      # Convert latpts to array
      lats = [float(x) for x in latpts.split()]
      lonpts = snap.find('lonPts').text
      # Convert lonpts to array
      lons = [float(x) for x in lonpts.split()]
      numpts = int(snap.find('nLatLonPts').text)
      for i in range(numpts):
        sys.stdout.write("    %5.2f  %7.2f\n" % (lats[i],lons[i]))
      closeFlag = snap.find('closeFlg').text
      if closeFlag == "1":
        sys.stdout.write("    %5.2f  %7.2f\n" % (lats[0],lons[0]))
  # endfor 
   
  # To insure that NMAP2 appropriately not displays on a forecast update,
  # print out empty records with Hazards appropriate for the Bulletin Type.
  # These must be for each valid forecast time of the product.
  hazards = []
  if re.match(r"S|SIERRA",bulletin):
    hazards = ['IR','MO']
  if re.match(r"T|TANGO",bulletin):
    hazards = ['TB','SW','WS']
  if re.match(r"Z|ZULU",bulletin):
    hazards = ['IC']
  # Calculate the epochtimeuntil time from until time string...
  epochtimeuntil = int(datetime.strptime(untilTime,"%Y%m%d%H%M").strftime('%s'))
  epochcycletimebegin = epochtimeuntil - 6*3600
  fltlvl1 = fltlvl2 = ""
  issTime = "%s/%s" % (issueTime[2:8],issueTime[8:12])
  for hazard in hazards:
    # Run through forecast hours 0,3,6,9,12
    for fcsthr in range(0,15,3):
      epochvalidbegtime = epochcycletimebegin + fcsthr*3600
      epochvalidendtime = epochvalidbegtime+179*60
      startTime=time.strftime("%y%m%d/%H%M",time.localtime(epochvalidbegtime))
      stopTime =time.strftime("%y%m%d/%H%M",time.localtime(epochvalidendtime))
      sys.stdout.write("|%s|%s|%s|%s|%s%02d|%s|%s|%s|%d|%d\n" % (hazard,issTime,
        startTime,stopTime,'KCI',iseqsuffix,updatenum,fltlvl1,fltlvl2,
        correction,cancel))
  
    #endfor    
  #endfor    
  sys.stdout.close()
  sys.stdout = original_stdout
  exit(0)

if __name__ == '__main__':
  main()
  
  
  
  
    
    
  
  
  
  
  
  
  
  

