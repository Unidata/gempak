#!/usr/bin/perl

$p1="";
$pri2="";
$pth="";

while (<>) {
   chomp;
   $pri = substr($_,74,2);
   if ($pri =~ m/5[15]/) {
      $p1 = substr($_,61,4) . "." . substr($_,65,2) . "," . substr($_,56,2) . "." . substr($_,58,2);
      $p1 =~ s/\s+//g;
      if ($pri2 eq '') { $pth = "        <path>"; }
      elsif ($pri eq '51' && $pri2 eq '55') { $pth .= "</path>\n        <path>"; }
      $pth .= " " . $p1; 
      $pri2 = $pri;
   }
   if ($pri =~ m/5[0]/) {
      if ($pth ne '') {
         print $pth,"</path>\n";
         $pth = "";
      }
      s/-- (HI|--)/HI US/; 
      print;
      print "\n";
      $pri2="";
      $z1=substr($_,78,3);
      $z2=substr($_,81,3);
      $st=substr($_,49,2);
      if($st eq "--") { $st = substr($_,52,2); }
      $z2 = ($z2 eq '') ? $z1 : $z2;
      if ($z1 ne '') {
         $lz="<landZones>";
         for $i ($z1 .. $z2) {
            $lz .= "$st"."Z"."$i ";
         }
         $lz =~ s/\s+$//g ;
         print "        $lz</landZones>\n";
      }
   }
}
if ($pth ne '') { print $pth,"</path>\n"; }

