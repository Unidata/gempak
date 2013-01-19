#include <stdio.h>
#include <errno.h>
#include <unistd.h>     /* read, write, lseek, close */
#include "aracom.h"
#include "arasubs.h"

int clsara (int fd)
{
 int i;
 int cur_area = -1;

 for (i=0; i<N_AREAS; i++)
  {if(aracom[i].fd == fd) {cur_area=i; break;}}
 if(cur_area == -1)
  {printf ("file %d not opened \n",fd); return -1;}

 close (fd);

 if(aracom[cur_area].area_name != NULL)
 {
 	free(aracom[cur_area].area_name);
 }

 aracom[cur_area].area_name = NULL;
 aracom[cur_area].fd = -1;
 aracom[cur_area].dir[32] = -1;
 new_area--;

 return 0;
}
