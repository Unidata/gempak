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
                  }
               }
            }
         close(OLD);
         if ( DONE == 0 )
            {
            printf "%-12s %-25s %-48s ",darr[1],darr[2],darr[3];
            printf "%-8s %-8s %4s %6s %6s %12s",darr[4],darr[5],darr[6],darr[7],darr[8],darr[9];
            if ( np == 9 ) {
               timematch="4";
               printf " %6s\n",timematch; 
               }
            else
               printf "\n";
            }
         }
      }
   }
}
