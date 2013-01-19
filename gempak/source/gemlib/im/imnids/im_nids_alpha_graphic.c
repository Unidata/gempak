#include "geminc.h"
#include "gemprm.h"

extern int read_int (unsigned char *);
extern short read_short(unsigned char *);


void im_nids_alpha_graphic ( unsigned char *buf )
{
int i, k, icnt;
int blen, boff, ipag, lpag, npag;
int packet_code=0, cval;

if ( (int)read_short(buf) != -1 )
   {
   printf("block divider not found\n");
   return;
   }

if ( (int)read_short(buf + 2) != 2) 
   {
   printf("alpha block '2' not found %d\n",(int)read_short(buf + 2));
   return;
   }

blen = read_int(buf + 4);
/*printf("alpha block length %d\n", blen);*/
npag = (int)read_short(buf + 8);
ipag = 0;
boff = 10;
while ( ( ipag < npag ) && (boff < blen ) )
   {
   ipag = (int)read_short(buf + boff);
   lpag = (int)read_short(buf + boff + 2);
   /*printf("boff %d page number %d length %d\n",boff,ipag,lpag);*/

   icnt = 0;
   while ( icnt < lpag)
      {
      packet_code = (int)read_short(buf + boff + 4 + icnt);
      if ( ( packet_code == 8 ) || ( packet_code == 1 ) )
         {
         /* write out text table */
         icnt = icnt + 4 + packet_code_8 ( buf + boff + 4 + icnt, -1 ); /* ipag to -1 */
         }
      else if ( packet_code == 10 )
         icnt = icnt + 4 + packet_code_10 ( buf + boff + 4 + icnt, -1 );
      else
         {
         printf("alpha packet code %d not handled [icnt %d]\n",packet_code,icnt);
         icnt = icnt + (int)read_short(buf + boff + 6 + icnt);
	 break;
         }

      }

   boff = boff + lpag + 4;
   }
}


