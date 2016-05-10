#!/usr/bin/perl

@a1 = ();
@pth = ();
$p2="";

#####################<path>#####################

sub addP {
   if (@pth) { push @a1, "        <path>" . join(" ", @pth) . "</path>"; }
   @pth = ();
   $p2="";
}

while (<>) {
   chomp;
   $pri = substr($_,74,2);
   if ($pri =~ m/8[0]/) { addP(); }
   if ($pri =~ m/8./) { push @a1, $_; }
   $p1 = substr($_,61,4) . "." . substr($_,65,2) . "," . substr($_,55,3) . "." . substr($_,58,2);
   $p1 =~ s/\s+//g;
#if ($pri =~ m/8[15]/) { print "p1:",$p1, "  p2:",$p2,"\n"; }
   if ($pri =~ m/8[15]/ && $p2 ne $p1) { 
      push @pth, $p1;
      $p2=$p1;
   }
}
addP();

#####################<landZone>#####################
@a2 = ();
@a3 = ();
@lz = ();
$zz = "";

sub mpush { push @lz, substr($_[0],0,2) . 'Z' . substr($_[0],2,3); }

sub addZ {
   print "        <landZones>" . join(" ", @lz) . "</landZones>\n"; 
   @lz = ();
}

foreach (@a1) {
   $pri = substr($_,74,2);
   if ($pri =~ m/8./ && ! /path/) {
      $z1=substr($_,78,5);
      $z2=substr($_,83,5);
      if ($pri eq '80') {
         if (@lz) { addZ(); }
      }
      if ($z1 ne '') { mpush($z1); }
      if ($z2 ne '') { mpush($z2); }
   }
   print $_,"\n";
}


