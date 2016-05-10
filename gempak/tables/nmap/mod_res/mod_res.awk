BEGIN {
#
# Chapter A, general products
chapter["A1"] = "standard";
chapter["A2"] = "surface";
chapter["A3"] = "precip";
chapter["A4"] = "streamlines";
chapter["A5"] = "basic_wx";
chapter["A6"] = "qpf";
chapter["A7"] = "severe";
chapter["A8"] = "ndfd";
chapter["A9"] = "Q-G_Thory";
#
# Chapter B, targeted products
chapter["B1"] = "aviation";
chapter["B2"] = "tropical";
chapter["B3"] = "marine";
chapter["B4"] = "exp_marine";
chapter["B5"] = "hurricane";
chapter["B6"] = "volcash";
chapter["B7"] = "oceanic";
chapter["B8"] = "stability";
chapter["B9"] = "ensemble";
#
# Chapter C, Special use products
chapter["C1"] = "misc";
chapter["C2"] = "medr";
chapter["C3"] = "comp";
chapter["C4"] = "no_col-fill";
chapter["C5"] = "monochrome";
chapter["C6"] = "intl_sam";
chapter["C7"] = "anl_loops";
chapter["C8"] = "loops";
chapter["C9"] = "radar";
#
# Other products
chapter["D1"] = "OA_SFC";
}


# ! S. Chiswell/Unidata   10/05   replaced nam;nam20;nam40;nam44 with nam;nam12;nam20;nam40;nam44;wseta;wrf
# ! S. Chiswell/Unidata   10/05   replaced dgex;gfs;gfshd with dgex;gfs;gfshd;gfs002;gfs003;gfsthin;gfs212;gfs211
# ! M. James/Unidata      05/16   added gfs215

{ 
	if ( index($0,"\!") == 1 ) { 
	   print $0 ; 
	} 
	else { 
	   ic = split($0,astr); 
           if ( ic != 4 ) { 
		printf "\!\!%s\n", $0; 
	   } 
           else { 
              chapnum = "E";
              for ( var in chapter ) {
                 if ( chapter[var] == astr[3] ) {
                    chapnum = var;
		    break;
		 }
	      }
	      printf "%-32s %-51s %s.%-12s ", astr[1], astr[2], chapnum, astr[3]; 
	      sub("nam;nam20;nam40;nam44","nam;nam12;nam20;nam40;nam44;wseta;wrf",astr[4]);
              sub("dgex;gfs;gfshd","dgex;gfs;gfshd;gfs002;gfs003;gfsthin;gfs212;gfs215;gfs211",astr[4]);
	      im = split(astr[4],mlist,";"); 
              if ( im == 0 ) { 
		printf "%16s\n", astr[4]; 
	      } 
              else { 
	         for ( i=1; i <= im; i++) { 
		   if ( i > 1 ) printf ";" ; 
		   sub("eta", "nam", mlist[i]); 
		   sub("avn", "gfs", mlist[i]); 
		   sub("wsnam", "wseta", mlist[i]); 
		   printf "%s",mlist[i]; 
	         } 
                 printf "\n"; 
	      } 
	   } 
	} 
}

