#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_binpoly ( char *name, int np, float *x, float *y, int *iret )
/************************************************************************
 * clo_binpoly								*
 *									*
 * This function determines which bounds are contained within (either	*
 * partly or wholly) the polygon defined by the set of points (x,y).	*
 * The input polygon points are assumed to be (lat,lon).		*
 * Those bounds are indicated via the CLO hotlist.			*
 *									*
 * clo_binpoly ( name, np, x, y, iret )					*
 *									*
 * Input parameters:							*
 *	*name		char		Bound name			*
 *	np		int		Number of points in polygon	*
 *	*x		float		x-coordinate poly points	*
 *	*y		float		y-coordinate poly points	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
int	ii, which, ier;
long	bnd_strec, strec;

int	intrsct, nposs, npart, nparts, found, npts;
int	*inout, *pbnd;
float	*xp, *yp;
float	lat1, lon1, lat2, lon2;
float	minlat, minlon, maxlat, maxlon;
char	buff[128], *ptr;

char	fnm[132];
FILE	*fp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( name );

    nhot = 0;

    minlat =  90.0F;
    maxlat = -90.0F;
    minlon =  360.0F;
    maxlon = -360.0F;
    for ( ii = 0; ii < np; ii++ )  {
	minlat = G_MIN ( minlat, x[ii] );
	maxlat = G_MAX ( maxlat, x[ii] );
	minlon = G_MIN ( minlon, y[ii] );
	maxlon = G_MAX ( maxlon, y[ii] );
    }
  
    nposs = 0;

    /*
     *  Sort bounds by their maximum longitude from east-to-west.
     */
    clo_sortbnd ( name, BND_MXLON, &ier );

    /*
     *  Allocate some space based on max possible number of bounds
     */
    pbnd = (int *)malloc((size_t)clo.loc[which].bnd.nbnd * sizeof(int) );
    xp = (float *)malloc((size_t)clo.loc[which].bnd.nbnd * sizeof(float) );
    yp = (float *)malloc((size_t)clo.loc[which].bnd.nbnd * sizeof(float) );

    ii = 0;
    while ( ii < clo.loc[which].bnd.nbnd  &&
            clo.loc[which].bnd.bound[ii].maxlon >= minlon )  {

      if ( clo.loc[which].bnd.bound[ii].minlon <= maxlon  &&
           clo.loc[which].bnd.bound[ii].minlat <= maxlat  &&
           clo.loc[which].bnd.bound[ii].maxlat >= minlat )  {

        pbnd[nposs] = ii;
	xp[nposs] = clo.loc[which].bnd.bound[ii].cenlat;
	yp[nposs] = clo.loc[which].bnd.bound[ii].cenlon;
        nposs++;

      }

      ii++;

    }


    if ( nposs == 0 )  {
	free ( yp );
	free ( xp );
	free ( pbnd );
	return;
    }

    /*
     *  Send centroid locs to cgr_inpoly for definite inclusion check.
     */
    inout = (int *) malloc ( (size_t)nposs * sizeof(int) );
    cgr_inpoly ( sys_M, &nposs, xp, yp, sys_M, &np, x, y, inout, &ier );

    /*
     *  For each bound whose centroid is not definitely inside, read in
     *  bounds points and check via cgr_polyint.
     *
     *
     *  Open boundaries file.
     */
    strcpy(fnm, clo.loc[which].bnd.filename);
    fp = (FILE *)cfl_tbop(fnm, "bounds", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
	free ( yp );
	free ( xp );
	free ( pbnd );
        return;
    }

    for ( ii = 0; ii < nposs; ii++ )  {

      if ( inout[ii] == 0 )  {

        /*
         *  This bound's centroid is not inside polygon.
         *  For each part of each bound, check if inside poly (cgr_polyint).
         *  Once any part is found to be inside, save off to hotlist.
         */
        nparts = clo.loc[which].bnd.bound[pbnd[ii]].nparts;
        bnd_strec = clo.loc[which].bnd.bound[pbnd[ii]].strec;

	npart = 0;
	found = 0;
	while ( npart < nparts && found != 1 )  {

	  /*
	   *  Gather points for each part and send to cgr_polyint.
	   */
          strec = clo.loc[which].bnd.bound[pbnd[ii]].bndspt[npart].strec;

          cfl_seek ( fp, strec, 0, &ier );
          cfl_trln( fp, sizeof(buff), buff, &ier );

          npBnd = 0;
          sscanf ( buff, "%d %f %f %f %f %f %f", &npts,
                   &lat1, &lat2, &lon1, &lon2,
                   &(xpBnd[npBnd]), &(ypBnd[npBnd]) );
          npts /= 2;
          npBnd++;

          while ( npBnd < npts )  {

            cfl_trln ( fp, sizeof(buff), buff, &ier );
            /*
             *  Bounds points come in pairs of (lat,lon),
             *  i.e., they are never split between lines.
             */
            ptr = strtok ( buff, " " );
            while ( ptr != '\0' )  {
                sscanf ( ptr, "%f", &(xpBnd[npBnd]) );
                ptr = strtok ( NULL, " " );
                sscanf ( ptr, "%f", &(ypBnd[npBnd]) );
                npBnd++;
                ptr = strtok ( NULL, " " );
            }

          }

	  whichBnd = which;

	  cgr_polyint ( sys_M, &npBnd, xpBnd, ypBnd, 
			sys_M, &np, x, y, &intrsct, &ier );

	  /*
	   *  If they intersect, save off to hotlist; skip remaining parts.
	   */
	  if ( intrsct == 1 )  {
	    hotlist[nhot] = pbnd[ii];
	    hotstrec[nhot] = bnd_strec;
	    nhot++;
	    found = 1;
	  }

	  npart++;

        }

      }
      else  {
        /*
         *  This boundary centroid is inside polygon.
         *  Save off inside centroids strec into hotlist.
         */
	hotlist[nhot] = pbnd[ii];
	hotstrec[nhot] = clo.loc[which].bnd.bound[ii].strec;
	nhot++;
      }

    }

    free ( inout );

    /*
     *  Close boundaries file and return.
     */
    cfl_clos ( fp, &ier );

    free ( yp );
    free ( xp );
    free ( pbnd );

}
