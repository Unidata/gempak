#!/usr/bin/perl

@a1 = ();
@pth = ();

#####################<path>#####################

sub addP {
   if (@pth) { push @a1, "            <path>" . join(" ", @pth) . "</path>"; }
   @pth = ();
}

while (<>) {
   chomp;
   $pri = substr($_,74,2);
   if ($pri =~ m/6[09]/) { addP(); }
   if ($pri =~ m/6./) { push @a1, $_; }
   $p1 = substr($_,61,4) . "." . substr($_,65,2) . "," . substr($_,55,3) . "." . substr($_,58,2);
   $p1 =~ s/\s+//g;
   if ($pri =~ m/6[5]/ && !grep { /$p1/ } @pth) { push @pth, $p1; }
}

#####################<landZone>#####################
@a2 = ();
@a3 = ();
@lz = ();
$zz = "";

sub mpush {
   $zz = $_[0];
   if (!grep { /$zz/ } @lz) { push @lz, $zz; }
}

sub addZ {
   @lz2 = @lz;
   foreach (@lz2) {
      $_ = substr($_,0,2) . 'Z' . substr($_,2,3);
   }
   push @a2, "            <landZones>" . join(" ", @lz2) . "</landZones>"; 
   push @a2, @a3;
   @a3 = ();
   @lz = ();
}

foreach (@a1) {
   $pri = substr($_,74,2);
   push @a3, $_;
   if ($pri =~ m/6./) {
      $z1=substr($_,78,5);
      $z2=substr($_,83,5);
      if ($pri eq '60' && substr($_,52,2) eq 'US') {
         if ($z1 ne '') { mpush($z1); } 
         if (@lz) { addZ(); }
         mpush(($z1 eq '') ? $zz : (($z2 eq '') ? $z1 : $z2));
      } else {
         if ($z1 ne '') { mpush($z1); }
         if ($z2 ne '') { mpush($z2); }
      }
   }
}

if (@a3) { push @a2, @a3; }
print join("\n",@a2) . "\n";

