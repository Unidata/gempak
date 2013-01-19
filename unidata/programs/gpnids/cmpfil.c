#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define cmpfil	cmpfil_
#endif

void cmpfil ( char *fil1, int *len1, char *fil2, int *len2, int *icmp, int *ier )
{

int i;

*ier = 0;

for (i=0; i < *len1; i++)
   {
   if ( i > *len2 - 1 )
      {
      *icmp = -1;
      return;
      }

   if ( fil1[i] < fil2[i] )
      {
      *icmp = -1;
      return;
      }
   else if ( fil1[i] > fil2[i] )
      {
      *icmp = 1;
      return;
      }
      
   }

if( *len1 > *len2 )
   *icmp = 1;
else
   *icmp = 0;
}
