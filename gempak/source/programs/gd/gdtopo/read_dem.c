#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define read_dem read_dem_
#define cgdtwdt cgdtwdt_
#define im_gm2gi im_gm2gi_
#endif

int   gsize=0;
float *grid=NULL;


int
row_num (float *lat, float maxlat, float dlat)
{
  int row = (int) rint ((maxlat - *lat) * dlat);

  *lat = maxlat - (float) row / dlat;
  return (row);
}

int
col_num (float *lon, float minlon, float dlon)
{
  int col = (int) rint ((*lon - minlon) * dlon);

  *lon = minlon + (float) col / dlon;
  return (col);
}

float
read_short (unsigned char *x)
{
/* DEM is little endian 16 bit */
  short ival = (x[1] << 8) | x[0];
  return ((float) ival);
}

#define HDRSIZ 80

void
read_dem (char *filnam, char *ijskip, char *garea, int *ikx, int *iky,
	  float *gltln, float *rmin, float *rmax, int *iret)
{

  char cdproj[16];
  float centrd[2];

/* raster data base boundaries */
  float MAXLAT, MINLON, dlat, dlon;
  float nrec_lat, nrec_lon;

  int nrecline;

  int X[2], Y[2];

  int i, k, ii, ier, irec, iscnt, kscnt, ix, iy;
  int ifd, istep;
  char ival;
  int imin, jmin, imax, jmax;
  int ix1, ix2, nsx, iy1, iy2, nsy, autos;
  float ravg, rdev;

  unsigned char *buf, bhead[HDRSIZ];

  ssize_t bsz;
  size_t ihdrsz, rbsz;

/*-------------------------------------------------------------------*/

  *iret = -1;

  in_gskp ( ijskip, &ix1, &ix2, &nsx, &iy1, &iy2, &nsy, &autos, &ier);

  cfl_dopn (filnam, &ifd, &ier);
  if (ier != 0)
    {
      printf ("could not open topography file %s\n", filnam);
      return;
    }

  /*
   * Read HDRSIZ bytes of header to identify product.
   */
  bsz = read (ifd, bhead, HDRSIZ);
  if (bsz != HDRSIZ)
    {
      printf ("could not read raster header from file %s\n", filnam);
      close (ifd);
      return;
    }


  if ( bhead[9] != ' ' ) {
      /* this is just a raster, with no header */
      MAXLAT = 90.0; MINLON = -180.0;
      nrec_lat = 120.0;
      nrec_lon = 120.0;
      dlat = 180.0;
      dlon = 360.0;
      rbsz = sizeof(char);
      ihdrsz = 0;
  }
  else {
      i = sscanf ((const char *)bhead, "%f %f %f %f %f %f",
	  &MAXLAT, &MINLON, &nrec_lat, &nrec_lon, &dlat, &dlon);
      rbsz = sizeof(short);
      ihdrsz = HDRSIZ;
  }

  lc_gare (garea, gltln, cdproj, centrd, &ier, 48, sizeof (cdproj));
  if (ier != 0) {
      printf ("could not process GAREA\n");
      close (ifd);
      return;
  }

#ifdef DEBUG
  for (i = 0; i < 4; i++)
    printf ("gltln %f\n", gltln[i]);
#endif

  Y[0] = row_num (&gltln[0], MAXLAT, nrec_lat);
  Y[1] = row_num (&gltln[2], MAXLAT, nrec_lat);
  X[0] = col_num (&gltln[1], MINLON, nrec_lon);
  X[1] = col_num (&gltln[3], MINLON, nrec_lon);

  *ikx = abs (X[0] - X[1]);
  *iky = abs (Y[1] - Y[0]);

#ifdef DEBUG
  printf ("look X %d %d Y %d %d [%d %d]\n", X[0], X[1], Y[0], Y[1], *ikx,
	  *iky);
  for (i = 0; i < 4; i++)
    printf ("gltln %f\n", gltln[i]);
#endif


  if (*ikx < 1 || *iky < 1)
    {
      printf ("Requested area too small %d %d\n", *ikx, *iky);
      close (ifd);
      return;
    }

  if (*ikx * *iky > gsize)
    {
      if ( gsize > 0 )
	 grid = (float *)realloc( grid, ( (*ikx) * (*iky) )*sizeof(float) );
      else
	 grid = (float *)malloc( ( (*ikx) * (*iky) )*sizeof(float) );
      if ( grid == NULL )
         {
         gsize = 0;
         printf ("Error allocating requested grid points ( %d ).\n",
	      *ikx * *iky);
         close ( ifd );
         return;
         }
      else
	 gsize = (*ikx) * (*iky);
    }

  if (Y[0] > Y[1])
    istep = -1;
  else
    istep = 1;

  nrecline = (int) (nrec_lon * dlon);
  buf = (unsigned char *) malloc (nrecline * rbsz);

  ii = 0;
  iscnt = 0;
  kscnt = 0;
  iy = 0;

  for (i = 0; i < *iky; i++)
    {
      if ( iscnt == 0 ) {
          ix = 0;
          irec = (Y[0] + (i * istep)) * nrecline + X[0];
          irec = irec * rbsz + ihdrsz;
          lseek (ifd, irec, SEEK_SET);
          bsz = read (ifd, buf, rbsz * (*ikx));
          for (k = 0; k < bsz / rbsz; k++) {
              if ( kscnt == 0 ) {
                  if ( rbsz == 2 )
	              grid[ii] = read_short (buf + (k * 2));
                  else {
                      ival = buf[k];
                      grid[ii] = (float)ival;
                  }
	          ii++;
                  ix++;
              }
              kscnt++;
              if ( kscnt > nsx ) kscnt = 0;
	  }
          iy++;
      }
      iscnt++;
      if ( iscnt > nsy ) iscnt = 0;
    }
  free (buf);
  close (ifd);

  *ikx = ix;
  *iky = iy;

  imin = 1;
  jmin = 1;
  imax = *ikx;
  jmax = *iky;
  gr_stat ( grid, ikx, iky, &imin, &jmin, &imax, &jmax, rmin, rmax, &ravg, &rdev,
	iret);

}



void cgdtwdt ( const int *iacss, const int *ikx, const int *iky, 
	const char *parm, const char *time1, const char *time2, 
	const int *ivcord, const int *nbits, float *rnvblk, int *iret)
{
int i;

int ighdr[LLGDHD];
const int ipktyp=MDGGRB;
const int level1=0;
const int level2=-1;
const int rewrit=G_TRUE;

for ( i=0; i < LLGDHD; i++ ) ighdr[i] = 0;

cgd_wpgd ( iacss, (const float *) grid, ikx, iky, (const int *)ighdr, time1,
                time2, &level1, &level2, ivcord, parm, &rewrit,
                &ipktyp, nbits, iret );
}
