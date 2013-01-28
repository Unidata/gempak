#include <stdio.h>
#include <errno.h>
#include <unistd.h>	/* read, write, lseek, close */
#include "direct.h"
#include "mcidas.h"
#include "arasubs.h"
#include "mccal.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


int stripbnd (int *buf, int *lbuf, int band, int cur_area, int line, int nele);
static int pbuf[1000];

extern int arfd;    /* defined in metdaa */

/*  STATUS = REDARA (area,line,elem,nele,band,iarray)
    REDARA - Read line from area returning specified subset of line
    Input:
      AREA = (I) area number
      LINE = (I) line to read
      ELEM = (I) starting element desired. 0 returns first byte of data
      NELE = (I) number of elements. Will not return any past line end
      BAND = Spectral band of desired data or 'ALL ' for all bands
    OUTPUT:
      IARRAY = (I) array to hold returned data */

int  redara (int fd, int line, int elem, int nele, int band, int *buf)
{
#define LBUF 300000
 int lbuf[LBUF];
 int all = 0x414c4c20;	/* for test of band = 'ALL ' */
 int i;  /* index */
 int loc;	/* location of first byte */
 int len;	/* length of prefix */
 int cur_area = -1;
 int status;
 int slot;
 char cerror[120];

 arfd=fd;
 for (i=0; i<N_AREAS; i++)
  {if(aracom[i].fd == fd) {cur_area=i; break;}}
 if(cur_area == -1)
  {printf ("file %d not opened \n",fd); return -1;}
 directptr = (void *)aracom[cur_area].dir;
 slot = aracom[cur_area].slot;

 /* Check nele, cannot go off end of line */
 if ((elem+nele) > directptr->nele)
  {printf("elem+nele off edge of line %d %d %ld\n", elem,nele,directptr->nele);}
 loc = (aracom[cur_area].line_len + aracom[cur_area].pref_len) * line +
    aracom[cur_area].pref_len +
    aracom[cur_area].elem_len * elem +
    directptr->data_loc;
 /*len = aracom[cur_area].line_len;*/
 len = aracom[cur_area].elem_len * nele;
 if (len/4 > LBUF)
  {printf("redara: len (%d) > LBUF (%d)\n", len/4, LBUF); return -2;}

 /* check for invalid line number */
  if( line>=aracom[cur_area].dir[8] || line<0 ) {
    printf("REDARA ERROR: invalid line number = %d\n", line);
    (void)memset(buf, 0, sizeof(buf)*nele);
    return 0;
  }

 errno = 0;
 i = lseek(fd,loc,0);
 if (i < 0)
  {sprintf(cerror,"redara: lseek error l1 errno= %d fdi: %d %d %d\n",
     errno,fd,loc,i);
   perror(cerror); printf(cerror); return -1;}
 errno = 0;
 i = read (fd, lbuf, len);
 if (i != len && errno != 0)
  {sprintf(cerror,"redara: read error r1 errno= %d fd: %d "
     " %d %d %d\n",errno,fd,len,i,loc);
   printf(cerror); perror(cerror); return -1;}

 if (band == all)
     {for (i=0; i<len/4; i++) buf[i] = lbuf[i];}
 else   {
   status = stripbnd (buf, lbuf, band, cur_area, line, nele);
   if (status < 0) return -4;
   if (aracom[cur_area].dir[52] != aracom[cur_area].options[2]) {
     if (slot == 1) {
       status = kb1cal (pbuf, aracom[cur_area].dir, nele, band, buf);
      }
      else if (slot == 2) {
       status = kb2cal (pbuf, aracom[cur_area].dir, nele, band, buf);
      }
      else if (slot == 3) {
       status = kb3cal (pbuf, aracom[cur_area].dir, nele, band, buf);
      }
    }
    else if (aracom[cur_area].options[0] != aracom[cur_area].options[1]) {
        mpixel(nele, aracom[cur_area].options[0], aracom[cur_area].options[1], buf);
    }
  }

 return 0;
}

int stripbnd (int *buf, int *lbuf, int band, int cur_area, int line, int nele)
{
 int i,k,ix;
 int loc;	/* location of first byte */
 int len;	/* length of prefix */

 /* read in prefix */
 directptr = (void *)aracom[cur_area].dir;
 loc = directptr->data_loc +
   (aracom[cur_area].line_len + aracom[cur_area].pref_len) * line;
 len = aracom[cur_area].pref_len;
 i = lseek(aracom[cur_area].fd,loc,0);
 if (i < 0)
  {perror("redpfx: lseek ");printf("errno= %d\n",errno);return -1;}
 i = read (aracom[cur_area].fd, pbuf, len);
 if (i != len)
  {perror("redpfx: read ");printf("errno= %d\n bytes read %d",errno,i);return -1;}

 /* CHECK TO SEE IF band (the number) IS IN THIS AREA. */
 /* A single band area may not have a level section. */
 if (directptr->nbands == 1)
  {ix = 0;}	/* only one band, starts at location zero */
  else
  {
   if (directptr->validity_code != 0) len = 4; else len = 0;
   len = len + directptr->len_doc + directptr->len_cal;
   for (ix=0; ix<directptr->len_lev; ix++)
    {if ( *(((char *)pbuf)+(len+ix)) == band) goto found_band;}
   return 1;
  }

 found_band:
 if (directptr->data_bytes == 1)
  {
   for (i=ix,k=0; k<nele; i=i+directptr->nbands,k++)
    { *(((char *)buf)+k) = *(((char *)lbuf)+i); }
  }
 else
 if (directptr->data_bytes == 2)
  {
   for (i=ix,k=0; k<nele; i=i+directptr->nbands,k++)
    { *(((short *)buf)+k) = *(((short *)lbuf)+i); }
  }
 else
 if (directptr->data_bytes == 4)
  {;}
 else
  {return -3;}

 return 0;
}
