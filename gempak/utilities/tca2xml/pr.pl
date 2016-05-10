#!/usr/bin/perl

@a1 = ();
@pth = ();
@lz = ();
$p2="";

sub addP {
   ## Puerto Rico - duplicates landZones
   if (@lz) { 
      $t1 = pop @a1;
      push @a1, "            <landZones>" . join(" ", @lz) . "</landZones>"; 
      push @a1, $t1;
   }
   if (@pth) { push @a1, "            <path>" . join(" ", @pth) . "</path>"; }
   @pth = ();
   if (@lz) { push @a1, "            <landZones>" . join(" ", @lz) . "</landZones>"; }
   @lz = ();
}

while (<>) {
   chomp;
   $pri = substr($_,74,2);
   $z1=substr($_,78,3);
   $z2=substr($_,81,3);
   if ($pri =~ m/7[09]/) { addP(); push @a1, $_; }
   if ($pri =~ m/7./) { 
      $p1 = substr($_,61,4) . "." . substr($_,65,2) . "," . substr($_,55,3) . "." . substr($_,58,2);
      $p1 =~ s/\s+//g;
      if ($pri =~ m/7[25]/ && $p2 ne $p1 && !grep { /$p1/ } @pth ) { push @pth, $p1; }
      if ($pri =~ m/70/) { $p2=$p1; }
      if ($z1 ne '') { push @lz, 'PRZ'.$z1; }
      if ($z2 ne '') { push @lz, 'PRZ'.$z2; }
   }
}
addP();

print join("\n",@a1) . "\n";

