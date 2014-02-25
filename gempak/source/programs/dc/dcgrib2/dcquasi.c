#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>

#define MAXQROW	100

int qnumpts,qnumrows;
int qrowpts[MAXQROW];

void dcquasi ( int nrow, unsigned char *buf, int *isum)
{
int i,indx,ival;

indx = 0; *isum = 0;
for(i=0;i<nrow;i++)
   {
   qrowpts[i] = *isum;
   ival = gb_btoi(buf, indx, 2, FALSE);
   *isum += ival;
   indx += 2;
   /*printf("dcquasi %d/%d %d isum %d\n",i,nrow,ival,*isum);*/
   }

qnumpts = *isum;
qrowpts[nrow]=qnumpts;
qnumrows = nrow;
}
