#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>      /* open */
#include <unistd.h>     /* read, write, lseek, close */

#define PARMS 0666
#define OPEN
#include "direct.h"
#include "aracom.h"
#include "arasubs.h"

int opnara (char *area, char red_wrt)
{
 int i;
 int fd;
 int cur_area;
 static int n_bytes, n_read, lpos, start_byte;
 char cerror[120];
 Direct_ptr dptr;
 static int initial = 1;	/* initialize aracom */

 if (initial)
  {for (i=0; i<N_AREAS; i++) {aracom[i].area_name = NULL;}
   initial = 0;
  }

 /* Has this file already been opened?  Check file name against those already
    opened.  Does not work if the same file is given but one has the full path
    name.  If already opened,  exit after setting fd. */
 for (i=0; i<N_AREAS; i++)
  {if (aracom[i].area_name == NULL) continue;
   if (!strcmp(area,aracom[i].area_name))
     /* {cur_area = i; return 0;} */
     {cur_area = i; return aracom[i].fd;} 
  }

 /* Search for first available empty slot. */
 cur_area = -1;
 for (i=0; i<N_AREAS; i++)
  {if (aracom[i].area_name == NULL) {cur_area = i; break;}
  }
 if (cur_area < 0)
  {printf("no empty slots available\n");
   return -1;
  }
 new_area++;
 aracom[cur_area].area_name = malloc(strlen(area)+1);
 strcpy(aracom[cur_area].area_name, area);
 /*aracom[cur_area].area_name = area;*/

 if (red_wrt == 'R'  ||  red_wrt == 'r')
  {
   fd = open (area, O_RDONLY, 0) ;
   if (fd < 0)
     {sprintf(cerror,"opnara: open error errno= %d file: %s\n",
             errno,area);
      perror(cerror); printf(cerror); return -1;}
    aracom[cur_area].fd = fd;
  }
 else if (red_wrt == 'W'  ||  red_wrt == 'w')
  {
   fd = open (area, O_RDWR, 0);
   if (fd < 0)
    {fd = open (area, O_RDWR | O_CREAT, PARMS);
     if (fd < 0)
       {sprintf(cerror,"opnara create: open error errno= %d file: %s\n",
               errno,area);
        perror(cerror); printf(cerror); return -1;}
     aracom[cur_area].fd = fd;
     return fd;	/* new area, do not read in directory */
    }
   aracom[cur_area].fd = fd;
  }
 else
  {printf("read-only / read-write flag not set %c\n", red_wrt); return -2;}

 /* read in directory */
 start_byte = 0;
 if ((lpos = lseek(fd, start_byte, 0)) < 0)
  {sprintf(cerror,"opnara: lseek error l1 errno= %d fd: %d %d %d\n",
     errno,fd,start_byte,lpos);
   perror(cerror); printf(cerror); return -1;}
 errno = 0;  n_bytes = 64 * 4;
 n_read = read (fd, aracom[cur_area].dir, n_bytes);
 if ( (n_read != n_bytes && errno != 0) || (n_read != n_bytes) )
  {sprintf(cerror,"opnara: read error r1 errno= %d fd: %d "
     " %d %d\n",errno,fd,n_read,n_bytes);
   printf(cerror); perror(cerror); }
 aracom[cur_area].dir[32] = fd;

 dptr = (void *)aracom[cur_area].dir;
 aracom[cur_area].data_loc = dptr->data_loc;
 aracom[cur_area].pref_len = dptr->line_pref;
 aracom[cur_area].line_len = dptr->nele * dptr->data_bytes * dptr->nbands;
 aracom[cur_area].elem_len = dptr->data_bytes * dptr->nbands;

 /* Set options */
/* GAD 7/11/00 flipped dir[51] and [52] */
 aracom[cur_area].options[0] = aracom[cur_area].dir[10];	/* precision */
 aracom[cur_area].options[1] = aracom[cur_area].dir[10];	/* spacing */
 aracom[cur_area].options[2] = aracom[cur_area].dir[52];	/* units */
 aracom[cur_area].options[3] = 1;				/* scaling */
 aracom[cur_area].options[4] = aracom[cur_area].dir[51];	/* calibration */

/* Determine word flipping.  If word 1 (0-based) of directory is 4 then there
   is no flipping.  If it is 0x04000000 then flip.  Other combinations are
   not legal. */
 if (dptr->type == 4) aracom[cur_area].flip = 0;
 else if (dptr->type == 0x04000000)
  {aracom[cur_area].flip = 1;
   printf("cannot do word flipping with this version\n");}
 else
  {printf("invalid area, area type not 4 or 0x04000000, will close file\n");
   aracom[cur_area].flip = -1;
   close (fd);
   return -1;}
 
 return fd;
}
