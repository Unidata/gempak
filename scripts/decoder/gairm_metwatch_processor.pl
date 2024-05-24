#!/usr/bin/perl -w
use strict;
use POSIX qw(strftime);
use feature qw(switch);
no if $] >= 5.018, warnings => qw( experimental::smartmatch );
use Env;
use File::Copy;
use File::Basename;
###########################################################################
#
# Usage: gairm_metwatch_processor.pl -i <infile> -bulletin <S|T|Z> 
#           [-ofmt <dest dir/output fmt string>] [-bufrpath <path>] 
#           [-bufrexpr <expr>]
#
# Purpose: Support G-Airmet Metwatch function at 3-hour resolution via
#            AIRM dcairm decoded format.  The software is LDM event driven
#            by receipt of G-AIRMET BUFR.  The latest G-AIRMET BUFR
#            messages are then identified from a database of BUFR
#            messages, then converted to XML, then to GEMPAK decoded
#            AIRMET text (YYMMDD_HH.gairm) via program xml2gairm4nmap.pl.
#            The airmet files must be referenced in the datatype.tbl
#            with the appropriate path for NMAP2 to load via MISC/GAIRM.
#            The AIRMET text files are built using one of 3 BUFR messages
#            as input, in conjunction with an existing database of BUFR
#            messages.  The database is referenced to determine the update
#            number counts.
#
# Inputs:  G-AIRMET BUFR and ID of Bulletin Type via -bulletin,
#          BUFR messages database specified in -bufrpath
#          
# Outputs: Intermediary XML documents, AIRMET text directory and format specified by 
#          -ofmt switch.
# Dependency:  Script xml2gairm4nmap.pl
#
# Defaults:  
#   ofmt = $HOME/gairm/latest/%Y%m%d_%H.gairm
#   bufrpath /data/products/bufr/gairm
#   bufrexpr set to '(\d{8})_(\d{4})_(S|T|Z).gairm'
#         
# Author: Larry J. Hinson, AWC  03/2010          
#
##############################################################################
my $usageStatement;
$usageStatement = "Usage: gairm_metwatch_processor.pl -i <infile> -bulletin <S|T|Z> [-o <outfile>] [-bufrpath <path>] [-bufrexpr <expr>]
       Optional Switches:
         -h list usage
         -i <inpfile> Full path of input file. Use - for stdin
         -bulletin <S|T|Z>
         -ofmt <Dest Dir/output format str>          [Default = /\$HOME/gairm/latest/%Y%m%d_%H.gairm]
         -bufrpath <path> Path to bufr files...      [Default = /data/products/bufr/gairm]
         -bufrexpr [regular expression]              [Default = /(\\d{8})_(\\d{4})_(S|T|Z).gairm/]\n";
my ($inputfile, $inputsw, $stdinsw, $bufrpath, $bufrpathsw,
    $bufrexpr, $bufrexprsw, $ofmt, $outfilesw, $bulletin, 
    $bulletinsw, $statusflag);

if (($#ARGV+1) < 2) {
  print $usageStatement;
  exit 1;
}

$stdinsw = $bufrpathsw = $bufrexprsw = 0;
$bufrexpr = '^(\d{8})_(\d{4})_(S|T|Z).gairm$';
$bufrpath = "/data/products/bufr/gairm";
$ofmt = "$ENV{'HOME'}/gairm/latest/%Y%m%d_%H.gairm";
$statusflag = "NRML";

while ($#ARGV+1) {
  given($ARGV[0]) {
    when ("-i") {
      if ($ARGV[1] ne "-") {
        $inputfile = $ARGV[1];
      } else {
        $inputfile = "<&STDIN";
	$stdinsw = 1;
      }
      $inputsw = "-1";
    }
    when ("-bufrpath") {
      $bufrpath = $ARGV[1];
      $bufrpathsw = 1;
    }
    when ("-bufrexpr") {
      $bufrexpr = $ARGV[1];
      $bufrexprsw = 1;
    }
    when ("-ofmt") {
      $ofmt = $ARGV[1];
    }
    when ("-bulletin") {
      $bulletin = $ARGV[1];
      $bulletinsw = 1;
    }
    when ("-h") {
      print $usageStatement;
      exit 1;
    }
  }
  shift(@ARGV);
}
if (! ($inputsw && $bulletinsw)) {
  print $usageStatement;
  exit 1;
}

my $origdir = $ENV{'PWD'};
if ($inputfile !~ /\// && ! $stdinsw) {
  $inputfile = sprintf("%s/%s",$origdir,$inputfile);
}

my $tmpdir = "/var/tmp/gairm_metwatch_processor.pl.$$";
mkdir ($tmpdir, 0777);
chdir ($tmpdir);

my $buffer;
open (INFILE,"$inputfile") || die "Cannot open $inputfile";
open (OUTFILE,">temp.bufr") || die "Cannot open Redirected file temp.bufr";
binmode INFILE;
binmode OUTFILE;
while ( read (INFILE, $buffer, 65536) ) {
  print OUTFILE $buffer;
}
close(INFILE);
close(OUTFILE);
# Now Process buffer message to XML...
print ">>>bulletin=$bulletin\n";
`airmet_bufrvgf -i temp.bufr -opxml "%Y%m%d_%H%M_${bulletin}_<cycle>_pre.xml"`;
# xml created....Derive input bufr YYYYMMDD_HHMM parameter
my $dir = $tmpdir;
opendir(DIR,$dir);
my @xmlfiles = grep(/\d{8}_\d{4}_(S|T|Z)_\d{2}Z_pre.xml/,readdir(DIR));
closedir(DIR);
$_ = $xmlfiles[0];

my($xml_yyyymmdd,$xml_hhmm) = (/(\d{8})_(\d{4})/);

$_=$xml_hhmm;

my ($hour) = /(\d{2})/;

# Now Calculate an Update Number, based off of 0245Z issuance
#  Account for if BUFR has already been filed away...
#  If 0245Z issuance, set update number to 1.

my $updatecount = 0;
if ( $xml_hhmm ne "0245" ) {
  $dir = $bufrpath;
  my @bufrfiles;
  opendir(DIR,$dir);
  @bufrfiles = reverse sort grep(/$bufrexpr/,readdir(DIR));;
  closedir(DIR);
  my $file;
  foreach $file (@bufrfiles) {
    $_=$file;
    my ($yyyymmdd,$hhmm,$btype) = /$bufrexpr/;
    if (($btype =~ /S|SIERRA/ && $bulletin =~ /S|SIERRA/) ||
        ($btype =~ /T|TANGO/ && $bulletin =~ /T|TANGO/)   ||
        ($btype =~ /Z|ZULU/  && $bulletin =~ /Z|ZULU/)) {
        if ("${xml_yyyymmdd}${xml_hhmm}" ge "${yyyymmdd}${hhmm}") {
          $updatecount++;
        }
        if ($hhmm eq "0245") {
          last;
        }
    }
  }
} else {
  $updatecount = 1;
}
# Now update the Airmet Text Database
my $command = "xml2gairm4nmap.py -i $xmlfiles[0] --ofmt \"$ofmt\" --updatenum $updatecount --outhour $hour --append";
print "command = $command\n";
`xml2gairm4nmap.py -i $xmlfiles[0] --ofmt "$ofmt" --updatenum $updatecount --outhour $hour --bulletin $bulletin --append`;

chdir ($origdir);
`rm -rf ${tmpdir}`;

exit 0;

