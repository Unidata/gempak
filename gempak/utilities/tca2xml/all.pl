#!/usr/bin/perl

@a1 = ();
@pth = ();
$nm="";
%except=();

#####################<path>#####################
sub addP {
   if (@pth) { push @a1, "            <path>" . join(" ", @pth) . "</path>"; }
   @pth = ();
}

pathX(); 
while (<>) {
   chomp;
   $pri = substr($_,74,2);
   $nm = substr($_,16,33);
   $nm =~ s/\s+//g;
   $p1 = substr($_,62,3) . "." . substr($_,65,2) . "," . substr($_,56,2) . "." . substr($_,58,2);
   $p1 =~ s/\s+//g;
   if ($pri =~ m/1[09]/) { 
      addP(); 
      # breakpoint exception (ie. Caswell_Beach)
      if ($nm eq 'Caswell_Beach') { s/3390  -3390/3390  -7803/; } 
      # check exceptions in paths
      if(except->{$nm}) { push @pth, except->{$nm}; }
   }
   if ($pri =~ m/1./) { push @a1, $_; }
   if ($pri =~ m/1[5]/ && !grep { /$p1/ } @pth) { push @pth, $p1; }
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
   if ($pri =~ m/1./) {
      $z1=substr($_,78,5);
      $z2=substr($_,83,5);
      if ($pri eq '10' && substr($_,52,2) eq 'US') {
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

#####################path exceptions####################
sub pathX {
   $except{'Cameron'}='-92.30,29.53';
   $except{'Intracoastal_City'}='-91.54,29.53';
   $except{'Mouth_Mississippi_River'}='-89.10,29.17 -89.26,29.18';
   $except{'Port_Aransas'}='-96.9,28.1 -97.32,27.77 -96.45,28.46 -96.9,28.1';
   $except{'MS/AL_border'}='-88.33,30.38';
   $except{'AL/FL_border'}='-87.44,30.30';
   $except{'Hallandale_Beach'}='-80.15,26.01';
   $except{'Sandy_Hook'}='-74.18,40.45 -74.25,40.46';
   $except{'Hull'}='-70.97,42.35';
   $except{'Cape_Elizabeth'}='-70.19,43.65';
   $except{'Port_Clyde'}='-69.07,44.10';
   $except{'Key_West'}='-81.50,24.65';
}
