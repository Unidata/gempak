#include "geminc.h"
#include "gemprm.h"

#define HEADER_LINE    1
#define SUBHEADER_LINE 2
#define SEGBGN_LINE    3
#define DATA_LINE      4

static int whatline ( unsigned char *line );
static void maxmin ( float *plat, float *plon, int npts, 
		     float *maxlat, float *minlat, 
		     float *maxlon, float *minlon );
static void outseg ( float *plat, float *plon, int npr );
/************************************************************************
 * reducebnd.c                                                          *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 * whatline                                                             *
 * maxmin                                                               *
 * outseg                                                               *
 ***********************************************************************/

int main ( int argc, char *argv[] )
/************************************************************************
 * reducebnd                                                            *
 *                                                                      *
 * This function reduces the points of a line segement in bound file.   *
 * It is sure that the number of points of a segement is less than	*
 * LLMXPT after reduce.							*
 *                                                                      *
 * reducebnd ( argc, argv )                                             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          7/03   Created                                 *
 ***********************************************************************/
{
  char fname[FILE_FULLSZ];
  float filter, tmpfil;
  int npts, npr, nout, maxpoints;
  int mode, istat, iunit, itype;
  float xsize, ysize;
  char device[8], filnam[20], proj[8];
  float angle1, angle2, angle3;
  float lllat, lllon, urlat, urlon;
  float maxlat, minlat, maxlon, minlon;
  float *plat, *plon, *px, *py, *xout, *yout;
  float lat[4], lon[4];
  unsigned char line[LLMXLN];
  int ii, nn, ier;
  FILE *fp;
/*---------------------------------------------------------------------*/

  if ( argc != 3 ) {
    fprintf ( stderr, "Usage: %s bound-file reduce-factor\n", argv[0] );
    exit ( 1 );
  }

  strcpy ( fname, argv[1] );
  cst_crnm ( argv[2], &filter, &ier );
  if ( filter < 0.1 ) {
    filter = 0.1;
  }
  maxpoints = LLMXPT;

  /*
   * init GEMPLT and set DEVICE.
   */
  mode = 1;
  ginitp ( &mode, &istat, &ier );
  iunit = itype = 1;
  xsize = ysize = 1.0F;
  strcpy ( device, "GN" );
  strcpy ( filnam, "CGR_INSERT" );
  gsdeva ( device, &iunit, filnam, &itype, &xsize, &ysize, &ier,
	   strlen(device), strlen(filnam) );

  plat = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  plon = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  px = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  py = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  xout = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  yout = (float *)malloc ( maxpoints / 2 * sizeof ( float ) );
  if ( plat == NULL || plon == NULL || 
       px == NULL || py == NULL ||
       xout == NULL || yout == NULL ) {
    fprintf ( stderr, "Failed to allocate memory.\n" );
    exit ( 1 );
  }

  fp = cfl_ropn ( fname, NULL, &ier );
  if ( ier != 0 ) {
    fprintf ( stderr, "Failed to open the file: %s\n", fname );
    exit ( 1 );
  }

  while ( 1 ) {
    cfl_rdln ( fp, LLMXLN, (char *)line, &ier );
    if ( ier != 0 ) {
      break;
    }

    switch ( whatline ( line ) ) {
    case HEADER_LINE:
      printf ( "%s\n", line );
      break;
    case SUBHEADER_LINE:
      printf ( "%s\n", line );
      break;
    case SEGBGN_LINE:
      sscanf ( (char *)line, "%d %f %f %f %f %f %f", 
	      &npts, &maxlat, &minlat, &maxlon, &minlon, &plat[0], &plon[0] );
      npr = 1;

      /*
       * If the number of points for a segement exceeds maxpoints,
       * increase the size.
       */
      if ( npts > maxpoints ) {
	maxpoints = npts;
	plat = (float *)realloc ( plat, maxpoints / 2 * sizeof ( float ) );
	plon = (float *)realloc ( plon, maxpoints / 2 * sizeof ( float ) );
	px = (float *)realloc ( px, maxpoints / 2 * sizeof ( float ) );
	py = (float *)realloc ( py, maxpoints / 2 * sizeof ( float ) );
	xout = (float *)realloc ( xout, maxpoints / 2 * sizeof ( float ) );
	yout = (float *)realloc ( yout, maxpoints / 2 * sizeof ( float ) );
	if ( plat == NULL || plon == NULL || 
	     px == NULL || py == NULL ||
	     xout == NULL || yout == NULL ) {
	  fprintf ( stderr, "Failed to allocate memory.\n" );
	  exit ( 1 );
	}
      }

      break;
    case DATA_LINE:
      nn = sscanf ( (char *)line, "%f %f %f %f %f %f %f %f",
		   &lat[0], &lon[0], &lat[1], &lon[1], 
		   &lat[2], &lon[2], &lat[3], &lon[3] );
      for ( ii = 0; ii < nn/2; ii++, npr++) {
	plat[npr] = lat[ii];
	plon[npr] = lon[ii];
      }

      /* 
       * Got the whole segement, reduce its points.
       */
      if ( npts == 2 * npr ) {
	if ( minlat >= 0.0F ) {
	  /*
	   * Use North STR projection.
	   */
	  strcpy ( proj, "STR" );
	  angle1 = 90.0F;
	  angle2 = -90.0F;
	  angle3 = 0.0F;
	  lllat = -15.0F;
	  lllon = -135.0F;
	  urlat = -15.0F;
	  urlon = -135.0F;
	}
	else if (maxlat < 0.0F ) {
	  /*
	   * Use South STR projection.
	   */
	  strcpy ( proj, "STR" );
	  angle1 = -90.0F;
	  angle2 = -90.0F;
	  angle3 = 0.0F;
	  lllat = 15.0F;
	  lllon = -135.0F;
	  urlat = 15.0F;
	  urlon = -135.0F;
	}
	gsmprj ( proj, &angle1, &angle2, &angle3,
		 &lllat, &lllon, &urlat, &urlon, &ier, strlen(proj) );
	gtrans ( sys_M, sys_D, &npr, plat, plon, px, py, &ier,
		 strlen(sys_M), strlen(sys_D) );
	cv_rduc ( &npr, px, py, &filter, &nout, xout, yout, &ier );
	
	/*
	 * If the number of points of a segement after reduce is still
	 * more than LLMXPT, increase the filter and reduce again until 
	 * it is less than LLMXPT.
	 */
	tmpfil = filter;
	while ( nout > LLMXPT / 2 ) {
	  tmpfil = 2.0 * tmpfil;
	  npr = nout;
	  for ( ii = 0; ii < nout; ii++ ) {
	    px[ii] = xout[ii];
	    py[ii] = yout[ii];
	  }
	  cv_rduc ( &npr, px, py, &tmpfil, &nout, xout, yout, &ier );
	}

	gtrans ( sys_D, sys_M, &nout, xout, yout, plat, plon, &ier,
		 strlen(sys_D), strlen(sys_M) );
	npr = nout;

	/*
	 * output the reduced segement.
	 */
	outseg ( plat, plon, npr );
      }

      break;
    }
  }

  if ( ier != 4 ) {
    fprintf ( stderr, "Failed to read the file: %s\n", fname );
    exit ( 1 );
  }

  cfl_clos ( fp, &ier );
  free ( plat );
  free ( plon );
  free ( px );
  free ( py );
  free ( xout );
  free ( yout );

  return 0;
}


static int whatline ( unsigned char *line )
/************************************************************************
 * whatline                                                             *
 *                                                                      *
 * This function tells what kind of line the input is.                  *
 *                                                                      *
 * static int whatline ( line )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *line           unsigned char   Input line                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      whatline        static int      Line catagory                   *
 * Log:                                                                 *
 * R. Tian/SAIC          7/03                                           *
 ***********************************************************************/
{
  unsigned char ch;
  int blk_count, loop_count;
/*---------------------------------------------------------------------*/
  
  blk_count = 0;
  loop_count = 0;
  
  do {
    ch = *line++;
    
    if ( ch == 'B' ) {
      /*
       * Header line.
       */
      return HEADER_LINE;
    } else if ( ch == '<' ) {
      /*
       * Sub-header line.
       */
      return SUBHEADER_LINE;
    } else if ( isspace ( ch ) ) {
      /*
       * Data line.
       */
      blk_count++;
    } else {
      /*
       * Data line.
       */
      blk_count = 0;
    }
    
    if ( blk_count > 10 ) {
      return SEGBGN_LINE;
    }
  } while ( ++loop_count < 20 );
  
  return DATA_LINE;
}

static void maxmin ( float *plat, float *plon, int npts, 
		     float *maxlat, float *minlat, 
		     float *maxlon, float *minlon )
/************************************************************************
 * maxmin                                                               *
 *                                                                      *
 * This function calculates max/min of lat/lon.                         *
 *                                                                      *
 * maxmin ( plat, plon, npts, maxlat, minlat, maxlon, minlon )          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *plat           float         Input latitude                    *
 *      *plon           float         Input longitude                   *
 *      npts            int           Number of points                  *
 * Output parameters:                                                   *
 *      *maxlat         float         Max of latitude                   *
 *      *minlat         float         Min of latitude                   *
 *      *maxlon         float         Max of longitude                  *
 *      *minlon         float         Min of longitude                  *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          7/03   Created                                 *
 ***********************************************************************/
{
  int ii;
/*---------------------------------------------------------------------*/

  *maxlat = -9999.0F;
  *minlat = 9999.0F;
  *maxlon = -9999.0F;
  *minlon = 9999.0F;

  for ( ii = 0; ii < npts; ii++ ) {
    *minlat = G_MIN ( *minlat, plat[ii] );
    *maxlat = G_MAX ( *maxlat, plat[ii] );
    *minlon = G_MIN ( *minlon, plon[ii] );
    *maxlon = G_MAX ( *maxlon, plon[ii] );
  }
}

static void outseg ( float *plat, float *plon, int npr )
/************************************************************************
 * outseg                                                               *
 *                                                                      *
 * This function outputs the segement.                                  *
 *                                                                      *
 * outseg( plat, plon, npr )                                            *
 *                                                                      *
 * Input parameters:                                                    *
 *	*plat		float		Input latitude                  *
 *	*plon		float		Input longitude                 *
 *	npr		int		Number of points                *
 * Output parameters:                                                   *
 * 	none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          7/03   Created                                 *
 ***********************************************************************/
{
  int npts, nwl, nrl, np, ii, jj;
  float maxlat, minlat, maxlon, minlon;
/*---------------------------------------------------------------------*/

  npts = 2 * npr;
  nrl = ( npr - 1 ) % 4;
  nwl = ( npr - 1 - nrl ) / 4;
  maxmin ( plat, plon, npr, &maxlat, &minlat, &maxlon, &minlon );
  
  /*
   * Beginning line.
   */
  printf ( "%4d              %9.3f%9.3f%9.3f%9.3f%9.3f%9.3f\n",
	   npts, maxlat, minlat, maxlon, minlon, plat[0], plon[0] );

  /*
   * Whole line.
   */
  for ( ii = 0; ii < nwl; ii++ ) {
    for ( jj = 0; jj < 4; jj++) {
      np = ii * 4 + jj + 1;
      printf ( "%9.3f%9.3f",
	       plat[np], plon[np] );
    }
    printf ( "\n" );
  }
  
  /*
   * Remaining line.
   */
  if ( nrl > 0 ) {
    for ( ii = 0; ii < nrl; ii++ ) {
      np = nwl * 4 + 1 + ii;
      printf ( "%9.3f%9.3f", plat[np], plon[np] );
    }
    printf ( "\n" );
  }
}
