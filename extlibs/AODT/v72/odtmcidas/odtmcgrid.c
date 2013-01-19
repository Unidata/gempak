
/* Fills the data structure in GridHdr.h with unpacked grib data */
   
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <strings.h>
#include <string.h>
#include "grid.h"
#include "f2c.h"

#define PARMS 0666
#define HEADER_SIZE 64		/* length of header part of output file */
#define A_POW(x, y)  (float)( pow( (double)(x),(double)(y) ) )

int mcidas_writegrid(char *,int,float ***,int,int,int,int);
int mcidas_igopen (char *,char,int *);
int mcidas_igmake(int,char *,long);
int mcidas_igput(int,int,int *,int,int,int *,int *);
int mcidas_igclos (int );
int mcidas_lwrd(int,int,int,long *);
int mcidas_lwwr(int,int,int,long *);

static char cerror[120];

int mcidas_writegrid(char *gridpath,int gridnum,float ***array,int nx,int ny,int date,int time) 
{
  int *GridHeader = NULL;		/* grid header			*/
  int i,j,ii,jj,b,bptr,iyy;	                /* index */
  int iret,gridf;
  int nrows,ncols;
  int icreat=0;
  int gridwritten;
  int gstat;
  int exist_stat;
  int **data;
  int power10,type,ivalue;
  char *cgridnum;
  char *GRIDNAME;
  char *ident;
  float value,maxlat,minlat,maxlon,minlon,intlat,intlon,factor;
  unsigned int MISS;
  
  MISS = (unsigned int) 0x80808080;

  power10=100;

  /* allocate memory */
  ident=(char *)calloc((size_t)33,sizeof(char));
  cgridnum=(char *)calloc((size_t)9,sizeof(char));
  GRIDNAME=(char *)calloc((size_t)200,sizeof(char));

  b=sizeof(int);
  bptr=sizeof(int*);
  data=(int **)calloc((size_t)nx,bptr);
  for(iyy=0;iyy<nx;iyy++) {
    data[iyy]=(int *)calloc((size_t)ny,b);
  }
  sprintf(cgridnum,"GRID%4.4d",gridnum);
  (void)strcpy((char *)&GRIDNAME[0],gridpath);
  strncat(GRIDNAME,cgridnum,strlen(cgridnum));
  /*(void)strcpy((char *)&GRIDNAME[0],cgridnum);*/
  exist_stat = (int)mcidas_igopen(GRIDNAME,'W',&icreat);
  if (exist_stat == -1) return (-102);
  gridf = exist_stat;

  gstat=0;
  if (icreat == 1) {
    /* memset(ident,' ',32);    fix from STW */
    /* ident[32]=NULL;          fix from STW */
    gstat = mcidas_igmake(gridf,ident,2500); }
  if (gstat == -1) return (-103);

  GridHeader = (int *)malloc(sizeof(int)*HEADER_SIZE);
  if (GridHeader == NULL) {
    /* printf("Could not malloc space for grid header "); */
    return(-101);
  }

  maxlat=array[0][0][0];
  maxlon=array[0][0][1];
  minlat=array[nx-1][ny-1][0];
  minlon=array[nx-1][ny-1][1];
  intlat=(maxlat-minlat)/ny;
  intlon=(maxlon-minlon)/nx;

/*
  printf("Output GRID file = %8s\n",cgridnum);
  printf("number of rows=%d\n",ny);
  printf("number of cols=%d\n",nx);
  printf("date=%d\n",date);
  printf("time=%d\n",time);
  printf("maxlat=%f\n",maxlat);
  printf("minlat=%f\n",minlat);
  printf("maxlon=%f\n",maxlon);
  printf("minlon=%f\n",minlon);
  printf("intlat=%f\n",intlat);
  printf("intlon=%f\n",intlon);
*/

  memset((void *)GridHeader,(int)0,(size_t)256);	/* fill it with zeros */
  GridHeader[0]  = nx*ny;
  GridHeader[1]  = ny; 
  GridHeader[2]  = nx; 
  GridHeader[3]  = date;
  GridHeader[4]  = time;
  GridHeader[5]  = 0;
  (void)strcpy((char *)&GridHeader[6],"WIND");
  GridHeader[7]  = 1;
  GridHeader[9]  = 1001;       /* surface */
  (void)strcpy((char *)&GridHeader[32],"ADT");
  GridHeader[33] = 4;
  GridHeader[34] = (int)(maxlat*10000);
  GridHeader[35] = (int)(maxlon*10000);
  GridHeader[36] = (int)(minlat*10000);
  GridHeader[37] = (int)(minlon*10000);
  GridHeader[38] = (int)(intlat*10000);
  GridHeader[39] = (int)(intlon*10000);
  /*(void)strcpy((char *)&GridHeader[52],"Parameter");*/

	/* A McIDAS grid is stored left most column first, then progressing
	**   eastward  **/

  nrows = GridHeader[1];
  ncols = GridHeader[2];
  factor=(float)A_POW(10.0,power10);
  printf("nrows=%d ncols=%d\n",nrows,ncols);
  for (i = 0 ; i < ncols ; i++) {
    for (j = 0 ; j < nrows ; j++) {
      value=array[i][j][2];
      if(value>0.0) {
        data[j][i] = (int)value*10;
/*        printf("i=%d j=%d  value=%f %d\n",i,j,value,data[i][j]);*/
      } else {
        if(value<=-999.0) {
          ivalue = MISS;               /* outer wind radii */
        } else {
          ivalue=(int)value*10;
        }
        data[j][i] = ivalue;
      }
    }
  }

  iret = mcidas_igput(gridf, 0, data, nrows, ncols, GridHeader,&gridwritten);

  for(iyy=0;iyy<nx;iyy++) {
    free(data[iyy]);
  }
  free(data);
  free(GridHeader);
  free(ident);
  free(cgridnum);
  free(GRIDNAME);

  if (iret < 0) {
    /* printf("Failed to write grid\n"); */
    return(-104);
  }

  iret=mcidas_igclos(gridf);
  if (iret < 0) {
    /* printf("Failed to write grid\n"); */
    return(-105);
  }

  return (106);
}

/* igopen only opens the file, no initialization is done.  igmakx does that.
C IGOPEN  OPEN GRID FILE, RETURN FILE-REF-NO
C $ FUNCTION IGOPEN(GFNO, FILNAM)  (RJL 5/85)
C $ OPEN GRID FILE, RETURN FILE-REF-NUMBER.  FN VAL IS 0 (OK), -1
C $   CAN'T OPEN, E.G., NO SUCH FILE OR,
C $    -2 INVALID GRIDFILE NUMBER.
C $ GFNO = (I) INPUT  GRID FILE NO.
C $ FILNAM = (I) OUTPUT  FILE NAME OF THE SPECIFIED GRID FILE (8-CHARS)
*/


int mcidas_igopen (char *gridfile, char red_wrt, int *icreat)
{
 int i;
 int fd;
 int cur_gridfile;
 static int n_bytes, n_read, lpos, start_byte;
 static int initial = 1;	/* initialize aracom */

 if (initial)
  {for (i=0; i<N_GRIDS; i++) {gridcom[i].gridfile_name = NULL;}
   initial = 0;
  }

 /* Has this file already been opened?  Check file name against those already
    opened.  Does not work if the same file is given but one has the full path
    name.  If already opened,  exit after setting fd. */
 for (i=0; i<N_GRIDS; i++)
  {if (gridcom[i].gridfile_name == NULL) continue;
   if (!strcmp(gridfile,gridcom[i].gridfile_name))
    {cur_gridfile = i; return gridcom[i].fd;}
  }

 /* Search for first available empty slot. */
 cur_gridfile = -1;
 for (i=0; i<N_GRIDS; i++)
  {if (gridcom[i].gridfile_name == NULL) {cur_gridfile = i; break;}
  }
 if (cur_gridfile < 0)
  {/* printf("no empty slots available\n"); */
   return -1;
  }
 gridcom[cur_gridfile].gridfile_name = gridfile;
 gridcom[cur_gridfile].maxgrd = -1;	/* signifies not initialized */
 gridcom[cur_gridfile].dir_loc = -1;	/* signifies not initialized */

 *icreat = 0;
 if (red_wrt == 'R')
  {
   fd = open (gridfile, O_RDONLY, 0) ;
   if (fd < 0)
     {sprintf(cerror,"igopen: open error errno= %d file: %s\n",
             errno,gridfile);
      perror(cerror); /*printf(cerror);*/ return -1;}
    gridcom[cur_gridfile].fd = fd;
    /* printf("igopen read only fd %2d\n", fd);	/ DEBUG */
  }
 else if (red_wrt == 'W')
  {
   fd = open (gridfile, O_RDWR, 0);
   if (fd < 0)
    {fd = open (gridfile, O_RDWR | O_CREAT, PARMS);
     *icreat = 1;
     if (fd < 0)
       {sprintf(cerror,"igopen create: open error errno= %d file: %s\n",
               errno,gridfile);
        perror(cerror); /*printf(cerror);*/ return -1;}
     gridcom[cur_gridfile].fd = fd;
     return fd;	/* new gridfile, do not read in directory */
    }
   gridcom[cur_gridfile].fd = fd;
  }
  else
  {/*printf("incorrect read_write flag %c\n", red_wrt);*/ return -1;}

 start_byte = 0;
 if ((lpos = lseek(fd, start_byte, 0)) < 0)
  {sprintf(cerror,"igopen: lseek error l1 errno= %d fd: %d %d %d\n",
     errno,fd,start_byte,lpos);
   perror(cerror); /*printf(cerror);*/ return -102;}
 errno = 0;  n_bytes = GDIR * 4;
 n_read = read (fd, gridcom[cur_gridfile].dir, n_bytes);
 if ( (n_read != n_bytes && errno != 0) || (n_read != n_bytes) )
  {sprintf(cerror,"igopen: read error r1 errno= %d fd: %d "
     " %d %d\n",errno,fd,n_read,n_bytes);
   /*printf(cerror);*/ perror(cerror); }

 gridcom[cur_gridfile].dir_loc = 0;	/* dir has start of directory */
 gridcom[cur_gridfile].maxgrd = -gridcom[cur_gridfile].dir[10];

 return fd;
}


/*
 igmake.c Version 2 
 This version uses memory from the heap (malloc) to create the header part of a
 gridfile.  Since it is both allocated and freed in this call to igmake.c
 it is assumed that the released memory is usable (no leaks).
*/

int mcidas_igmake(int fd, char *ident, long maxgrd)
{
 int i,in;
 int cur_gridfile = -1;
 int lpos, n_read, n_bytes, start_byte;
 long *ident_ptr;
 char cerror[130];

 /*printf("igmake: maxgrd %4ld\n", maxgrd); */	/* DEBUG DEBUG */
 for (i=0; i<=N_GRIDS; i++)
  {if(gridcom[i].fd == fd) {cur_gridfile=i; break;}}
 if(cur_gridfile == -1)
  {/*printf ("file %d not opened \n",fd);*/ return -1;}

 /* Allocate enough memory for buffer.  The length is (maxgrd+12) 1-based */
 ident_ptr = malloc((maxgrd+12)*4);
 if (ident_ptr == NULL)
  {/*printf("malloc failed in igmake\n");*/ return -1;}

 /* Write out identification */
 for (i=0; i<8; i++) {ident_ptr[i] = 0x20202020;}	/* blank fill */
/* tlo removed for LINUX
 in = strlen(ident);
 strncpy((char *)ident_ptr, ident, in);
*/
 /* The maximum number of grids, as a negative number signifying version */
 ident_ptr[10] = -maxgrd;
 gridcom[cur_gridfile].maxgrd = maxgrd;
 /* Set these to a value >= 0 and <= 19365.  This is a flag for byte flipping. */
 /* word 9 is the one tested */
 ident_ptr[8] = ident_ptr[9] = 1;

 /* set grid pointers to -1 (empty) */
 for (i=11; i<(maxgrd+11); i++) {ident_ptr[i] = -1;}

 /* Put in location of next available grid location (a word).
   For new grid this is max_grids+13.  If (maxgrd+11 => IB) then
   write out later. */
 ident_ptr[maxgrd+11] = maxgrd+12;

 /* write out the buffer */
 errno = 0;
 start_byte = 0;
 n_bytes = (maxgrd+12) * 4;	/* (maxgrd+12) is a length (1-based)*/
 lpos = lseek (fd, start_byte, 0);
 if (lpos < 0)
  {sprintf(cerror,"igmake: lseek error l1 errno= %d fd: %d %d %d\n",
     errno,fd,start_byte,lpos);
   perror(cerror); /*printf(cerror);*/ return -1;}
 n_read = write (fd, ident_ptr, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"igmake: write error w1 errno= %d fd: %d %d %d\n",
    errno,fd,n_read,n_bytes);
   /*printf(cerror);*/ perror(cerror); }

 free(ident_ptr);
 return 0;
}

int mcidas_igput(int fd, int gno, int *grid, int nr, int nc, int *header, int *agno)
{
 int i,j,k,li,lj;
 long locg;	/* location of next available slot for grid */
 long locf;	/* location of next free space */
 long locnf;	/* location of new free space */
 int grid_size;	/* the size in words of this grid */
 int cur_gridfile = -1;

 for (i=0; i<=N_GRIDS; i++)
  {if(gridcom[i].fd == fd) {cur_gridfile=i; break;}}
 if(cur_gridfile == -1)
  {/*printf ("file %d not opened \n",fd);*/ return -2;}
 if (gridcom[cur_gridfile].maxgrd < 0)
  {/*printf("gridfile maxgrids < 0\n");*/ return -3;}

 grid_size = nr * nc + 64;

 if (nr < 0  ||  nc < 0)
  {			/* delete a grid from the gridfile */
   k = mcidas_lwrd(fd, gno+10, 1, &locg);
   if (k < 0) {return -10;}
   locg = -1;
   k = mcidas_lwwr(fd, gno+10, 1, &locg);
   if (k < 0) {return -10;}
   return 0;
  }
  else
  {			/* add a grid to the gridfile */
   if (gno >= 0)
    {
     locg = -1;	/* initialize to -1 for no available slot */
     for (j=11; j<gridcom[cur_gridfile].maxgrd+10; j=j+GDIR)
      { 
       k = mcidas_lwrd(fd, j, GDIR, gridcom[cur_gridfile].dir);
       if (k < 0) {return -9;}
       gridcom[cur_gridfile].dir_loc = j;
       for (i=0; i<GDIR; i++)
	{
         if ((j+i) > gridcom[cur_gridfile].maxgrd+10) break;
	 if (gridcom[cur_gridfile].dir[i] == -1)
          {locg = j+i; li = i; lj = j; goto out_of_j_loop;}
	}
      }			/* for loop on j */ 
     out_of_j_loop:
     if (locg == -1) return -1;		/* no room */
     *agno = j + i - 10;
     /* have a slot for the grid, need location of free space */
     k = mcidas_lwrd(fd, gridcom[cur_gridfile].maxgrd+11, 1, &locf);
     if (k < 0) {return -4;}
     gridcom[cur_gridfile].dir[li] = locf;
     /*k = mcidas_lwwr(fd, lj, GDIR, &gridcom[cur_gridfile].dir);*/
     k = mcidas_lwwr(fd, locg, 1, &gridcom[cur_gridfile].dir[li]);
     if (k < 0) {return -5;}
     locnf = locf + grid_size;
     k = mcidas_lwwr(fd, gridcom[cur_gridfile].maxgrd+11, 1, &locnf);
     if (k < 0) {return -6;}
     header[0] = nr * nc;
     header[1] = nr;
     header[2] = nc;
     k = mcidas_lwwr(fd, locf, 64, (long *)header);	/* write the grid header */
     if (k < 0) {return -7;}
     locf = locf + 64;
     k = mcidas_lwwr(fd, locf, nr*nc, (long *)grid);	/* write the grid */
     if (k < 0) {return -8;}
    }			/*  (gno => 0) */
   return 0;
  }

 return 0;
}


int mcidas_lwrd(int fd, int loc, int len, long *buf)
{
 long lpos, n_bytes, n_read;

 errno = 0;
 n_bytes = len * 4;
 lpos = lseek (fd, loc*4, 0);
 if (lpos < 0)
  {sprintf(cerror,"lwrd: lseek error l1 errno= %d fd: %d %d %ld\n",
     errno,fd,loc,lpos);
   perror(cerror); /*printf(cerror);*/ return -1;}
 n_read = read (fd, buf, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"lwrd: read error r1 errno= %d fd: %d %ld len %ld loc %d lpos %ld\n",
    errno,fd,n_read,n_bytes,loc*4,lpos);
   /*printf(cerror);*/ perror(cerror); return -2;}
 return 0;
}

int mcidas_lwwr(int fd, int loc, int len, long *buf)
{
 long lpos, n_bytes, n_write;

 errno = 0;
 n_bytes = len * 4;
 lpos = lseek (fd, loc*4, 0);
 if (lpos < 0)
  {sprintf(cerror,"lwwr: lseek error l1 errno= %d fd: %d %d %ld\n",
     errno,fd,loc,lpos);
   perror(cerror); /*printf(cerror);*/ return -1;}
 n_write = write (fd, buf, n_bytes);
 if (n_write != n_bytes && errno != 0)
  {sprintf(cerror,"lwwr: write error w1 errno= %d fd: %d %ld %ld\n",
    errno,fd,n_write,n_bytes);
   /*printf(cerror);*/ perror(cerror); return -2;}
 return 0;
}

int mcidas_igclos (int fd)
{
 int i;
 int cur_gridfile = -1;
                                                                                                                                                                                        
 for (i=0; i<=N_GRIDS; i++)
  {if(gridcom[i].fd == fd) {cur_gridfile=i; break;}}
 if(cur_gridfile == -1)
  {/* printf ("file %d not opened \n",fd); */  return -1;}
                                                                                                                                                                                        
 close (fd);
 gridcom[cur_gridfile].gridfile_name = NULL;
 gridcom[cur_gridfile].fd = -1;
                                                                                                                                                                                        
 return 0;
}

