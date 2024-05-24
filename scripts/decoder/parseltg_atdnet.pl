#!/usr/bin/perl -w
use strict;
use feature qw(switch);
no if $] >= 5.018, warnings => qw( experimental::smartmatch );
use Env;
########################################################################
#
# Usage: parseltg_atdnet.pl -i <file> -o <format specs>
#
# Purpose: Parse the GZipped Compressed ATDNet Lightning data received 
# through LDM under SFUK45 EGRR header.  This means stripping the header,
# unzipping the file, then running the GEMPAK decoder ltgdecode on it.
# ltgdecode needs the -G switch to parse this type of data.
#
# Inputs: Raw LDM SFUK45 EGRR header data that has been closed in LDM
# 
# Outputs: Decoded Lightning data in longword format to specified path
# and output file format in -o switch
#
# Author: Larry J. Hinson, AWC 04/2011
#
########################################################################

my ($inputfile,$stdinsw,$inputsw,$outputformat,$usageStatement,$origdir);
my $rawbin;


if (($#ARGV+1) < 2) {
  print "Usage: parseltg_atdnet.pl -i <file> -o <format specs>\n";
  exit 1;
}

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
    when ("-o") {
      $outputformat = $ARGV[1];
    }
    when ("-h") {
      print $usageStatement;
      exit 1;
    }
  }
  shift(@ARGV);
}
if (! $inputsw) {
  print $usageStatement;
  exit 1;
}

$origdir = $ENV{'PWD'};
if ($inputfile !~ /\// && ! $stdinsw) {
  $inputfile = sprintf("%s/%s",$origdir,$inputfile);
}

my $tmpdir = "/var/tmp/parseltg_atdnet.pl.$$";
mkdir ($tmpdir, 0777);
chdir ($tmpdir);

#Copy file or stdin to file named 'input' in temp directory...
open (INFILE,"$inputfile") || die "Cannot open $inputfile";
open (OUTFILE, ">temp.raw") || die "Cannot open redirected file temp.raw";
binmode INFILE;
binmode OUTFILE;
while ( read (INFILE, $rawbin, 65536) ) {
  print OUTFILE $rawbin;
}
close(INFILE);
close(OUTFILE);
# Now process temp.raw to gz file...
`dd if=temp.raw skip=31 bs=1 > temp.gz`;
`gunzip temp.gz >& /dev/null`;
# Now decode temp.gz store in format specified in outputformat...
`cat temp|ltgdecode -e -p 3600 -G -L -o $outputformat`;

# Process complete
chdir ($origdir);
`rm -rf ${tmpdir}`;
exit 0;

