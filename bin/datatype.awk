{
while (getline != 0)
   {
   if ( index($0,"!") != 1 )
      {
      dstr = $0;
      np = split ($0, darr);
      if ( np >= 9 )
         {
         DONE = 0;
         while ( ( DONE == 0 ) && ( getline <OLD != 0 ) )
            {
            if ( index($0,"!") != 1 )
               {
               nn = split ($0, oarr);
               if ( ( nn >= 9 ) && ( oarr[1] == darr[1] ) )
                  {
                  DONE = 1;
                  #printf "found match %s %s\n",oarr[1],darr[1];
                  printf "%-12s %-25s %-48s %s\n",darr[1],oarr[2],oarr[3],substr(dstr,89,(length(dstr)-88));
                  }
               }
            }
         close(OLD);
         if ( DONE == 0 )
            {
            printf "%s\n",dstr;
            }
         }
      #printf "nf %d ",np;
      #for(i=1;i<=np;i++) printf "%s ",darr[i];
      #printf "\n";
      }
   else
      {
      printf "%s\n",$0;
      }
   }
}
