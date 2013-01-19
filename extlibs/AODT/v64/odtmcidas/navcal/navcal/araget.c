#include <stdio.h>
#include <errno.h>
#include <unistd.h>	/* read, write, lseek, close */
#include "aracom.h"
#include "direct.h"
#include "arasubs.h"

/*  ARAGET: RETRIEVE PARAMETERS FROM AN AREA FILE    (RCD)
C               fd - (I) INPUT  AREA#
C               OFFSET - (I) INPUT  STARTING BYTE # 0-based
C               NBYTE - (I) INPUT  NUMBER OF BYTES TO RETRIEVE
C               ARRAY - (I) OUTPUT  ARRAY */

int  araget (int fd, int offset, int nbyte, int *array)
{
 int i;  /* index */
 int cur_area = -1;
 char cerror[120];

 for (i=0; i<N_AREAS; i++)
  {if(aracom[i].fd == fd) {cur_area=i; break;}}
 if(cur_area == -1)
  {printf ("file %d not opened \n",fd); return -1;}
 directptr = (void *)aracom[cur_area].dir;

 errno = 0;
 i = lseek(fd,offset,0);
 if (i < 0)
  {sprintf(cerror,"araget: lseek error l1 errno= %d fdi: %d %d %d\n",
     errno,fd,offset,i);
   perror(cerror); printf(cerror); return -1;}
 errno = 0;
 i = read(fd, array, nbyte);
 if (i != nbyte && errno != 0)
  {sprintf(cerror,"araget: read error r1 errno= %d fd: %d "
     " %d %d %d\n",errno,fd,nbyte,i,offset);
   printf(cerror); perror(cerror); return -1;}

 return 0;
}
