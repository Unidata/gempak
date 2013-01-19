#include <stdio.h>
#include <errno.h>
#include <unistd.h>	/* read, write, lseek, close */
#include "aracom.h"
#include "direct.h"
#include "arasubs.h"

/* READS DIRECTORY ENTRY FROM AREA */
/* fd  INPUT  AREA NUMBER */
/* direct  OUTPUT  64 WORD DIRECTORY ENTRY  */
extern char cerror[120];
extern char **varg;

int  readd (int fd, int direct[])
{
 int i;  /* index */
 int cur_area = -1;

 for (i=0; i<N_AREAS; i++)
  {if(aracom[i].fd == fd) {cur_area=i; break;}}
 if(cur_area == -1)
  {printf ("file %d not opened \n",fd); return -1;}
 directptr = (void *)aracom[cur_area].dir;

 for(i=0;i<64;i++) {direct[i] = aracom[cur_area].dir[i];}

 return 0;
}
