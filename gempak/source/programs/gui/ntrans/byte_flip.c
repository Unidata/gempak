#include "geminc.h"
#include "interface.h"

extern int doflip;

void byte_flip ( unsigned short *temp )
{
unsigned short temp1,temp2;
/*---------------------------------------------------------------------*/
    if ( doflip == 1 ) {
	temp1 = (*temp >> 8) & 0x00ff;
	temp2 = *temp & 0x00ff;
	*temp = (temp2 << 8) + temp1;
   }
}
