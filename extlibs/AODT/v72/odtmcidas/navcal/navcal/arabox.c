#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include "context.h"
#include "direct.h"
#include "arasubs.h"
#include "mccal.h"

void ARABOX(int area,int fstln,int lstln,int fstel,int lstel,
            int band,char *pqty,int spac,int elelen,int *scale,int *buf) 
{
/* Obtains a rectangular array of data from a McIDAS area file.
   Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
   Refer to "McIDAS Software Acquisition and Distribution Policies"
   Input:
        area    - file descriptor for area.
        fstln   - First line of data to obtain (0 based).
        lstln   - Last line of data to obtain.
        fstel   - First element of data to obtain (0 based).
        lstel   - Last element of data to obtain.
        band    - Band number (0 for non-banded data).
        pqty    - Physical quantity ('RAW', 'RAD', 'TEMP' ...).
        spac    - Number of bytes used to store the data value
                  (1, 2, or 4).
        elelen  - Element dimension of the array that will contain
                  the data.
  
   Output:
        buf     - Array containing the data.
  
   Remarks:
        This routine does not initialize the output array.
        It does not reduce resolution.
*/

  int  i=1,nele,offset,line;
  int iok;
  int opts[5], vals[5];
  /* new */ 
  int j,cur_area=-1;
  /* long infoc,ibuf[4],type[14]; */
  long ibuf[4],type[14];
  char infoc[4];

  strncpy ((char *)&opts[0], "SPAC", 4);
  vals[0] =  spac;
  strncpy ((char *)&opts[1], "UNIT", 4);
  strncpy ((char *)&vals[1], pqty, 4);
  iok = araopt(area,1,2, opts, vals);  /* set output precision */


  if(fstel<0) 
  {    /* first element is negative (left of the image) */
/* should not allow? Will cause segmentation violation due to negative
   subscript being passed to redara */
    nele=lstel+1;
    offset=-fstel;
    fstel=0;
  }
  else 
  {
    nele=lstel-fstel;
    offset=0;
  }
  for(line=fstln;line<=lstln;line++) 
  {
    /* note : buf in redara is only 2000 long... not 300000 as passed in */
    iok = redara(area,line,fstel,nele,band,&buf[i+offset-1]);
    i=i+elelen;
  }

  /* new stuff to obtain scaling factor */
  for (j=0; j<N_AREAS; j++)
   {if(aracom[j].fd == area) {cur_area=j; break;}}
  ibuf[0]=band;
  ibuf[1]=aracom[cur_area].dir[2];
  ibuf[2]=aracom[cur_area].dir[51];
  ibuf[3]=band;
  strncpy ((char *)infoc, "INFO", 4);
  iok=kb1opt(infoc,ibuf,type);
  *scale=type[11];
  /* end new */
}
