#!/usr/bin/perl

@a2 = ();
@a3 = ();
@lz = ();
$zz = "";

sub mpush {
   $zz = $_[0];
   $zz =~ s/\s+//g;
   if ($zz ne '' && !grep { /$zz/ } @lz) { push @lz, $zz; }
}

sub addZ {
   @lz2 = @lz;
   foreach (@lz2) {
      $_ = substr($_,0,2) . 'Z' . substr($_,2,3);
   }
   if (@lz2) { push @a2, "            <landZones>" . join(" ", @lz2) . "</landZones>"; }
   push @a2, @a3;
   @a3 = ();
   @lz = ();
}

while (<>) {
   chomp;
   $pri = substr($_,74,2);
   $nm = substr($_,16,33);
   $nm =~ s/\s+//g;
   if ($pri =~ m/9./) {
      push @a3, $_;
      $z1=substr($_,78,5);
      $z2=substr($_,83,5);
      if ($pri =~ m/9[0]/ && substr($_,52,2) eq 'US') {
         # check for path exception
         if ($nm eq 'Key_West') { push @a3, "            <path>-81.50,24.65</path>"; }
         mpush($z1);
         addZ();
         mpush(($z1 eq '') ? $zz : (($z2 eq '') ? $z1 : $z2));
      } else {
         if ($z1 ne '') { mpush($z1); }
         if ($z2 ne '') { mpush($z2); }
      }
   }
}

if (@a3) { push @a2, @a3; }
print join("\n",@a2) . "\n";

