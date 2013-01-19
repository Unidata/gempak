#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"

/*
 *  Set default half-width (statute miles)
 */
#define	HAFWID	60.0F

/*
 *  Local global variables.
 */
static float	_halfWid;
static float	_latSv[] = {0.F,0.F,0.F,0.F,0.F,0.F,0.F,0.F};
static float	_lonSv[] = {0.F,0.F,0.F,0.F,0.F,0.F,0.F,0.F};
static Boolean	_qSnap = FALSE;


/* 
 *  private functions 
 */
void pgwpts_comp ( int point, float lat1, float lon1, float lat2,
			float lon2, float width, float *dir, 
			float *lat, float *lon, int *iret );
void pgwpts_snap ( int which, float lat1, float lon1, int np,
			float *olat, float *olon, char *out_ancpts,
			float *lat2, float *lon2, float *anclat, 
			float *anclon, char *ancid, int *iret );


/************************************************************************
 * nmap_pgwpts.c							*
 *									*
 * This module contains fuctions to track the watchbox points.		*
 *									*
 * CONTENTS:                                                            *
 * pgwpts_init()	initialized the watchbox points			*
 * pgwpts_save()	saves the watchbox points locally		*
 * pgwpts_get()		retrieves the watchbox points			*
 * pgwpts_setSnap()	set the local snap variable			*
 * pgwpts_expand()	expands the watch by a distance			*
 *									*
 * pgwpts_snap()	compute new watch pts using a "snap" algorithm	*
 * pgwpts_comp()	compute watch pts given axis and half-width	*
 ***********************************************************************/

/*=====================================================================*/

void pgwpts_init ( float *ilat, float *ilon, int shape, 
			float *olat, float *olon, int *iret )
/************************************************************************
 * pgwpts_init                                                    	*
 *                                                                      *
 * This function takes two input points (lat, lon) and the watch	*
 * shape (EW, NS or ESOL) and returns 8 pairs of (lat, lon) points as 	*
 * an initial starting point for future editing.			*
 *                                                                      *
 * void pgwpts_init( ilat, ilon, shape, olat, olon, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *ilat	float		2 latitudes of axis endpoint locations	*
 *  *ilon	float		2 longitudes of axis endpoint locations	*
 *  shape	int		Watch shape (EW, NS, ESOL)		*
 *                                                                      *
 * Output parameters:                                             	*
 *  *olat	float		8 latitudes of watch			*
 *  *olon	float		8 longitudes of watch			*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 6/98						*
 * D.W.Plummer/NCEP	11/98	Changes for editing sides of pgram	*
 * S. Law/GSC		11/98	Changed parameters from x/y to lat/lon	*
 *				and renamed from pgwbxw_init		*
 ***********************************************************************/
{
    int		ier;
    float	tlat[8], tlon[8];
/*---------------------------------------------------------------------*/

    *iret = 0;

    tlat[0] = ilat[0];
    tlon[0] = ilon[0];
    tlat[4] = ilat[1];
    tlon[4] = ilon[1];

    pgwpts_get( -1, shape, tlat, tlon, olat, olon, &ier );

}

/*=====================================================================*/

void pgwpts_save ( float *lat, float *lon )
/************************************************************************
 * pgwpts_save                                                    	*
 *                                                                      *
 * This function saves information about a selected watch into local	*
 * global.								*
 *									*
 * void pgwpts_save( lat, lon )						*
 *                                                                      *
 * Input parameters:                                                    *
 *  *lat	float		lat array	              		*
 *  *lon	float		lon array	              		*
 *                                                                      *
 * Output parameters:                                             	*
 *  none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 9/98						*
 * D.W.Plummer/NCEP	11/98	Changes for editing sides of pgram	*
 * S. Law/GSC		11/98	Changed parameters from x/y to lat/lon	*
 *				and renamed from pgwbxw_spts		*
 * M. Li/GSC		10/99	Modified clo_dist code			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 ***********************************************************************/
{
int	n, ier, npx;
/*---------------------------------------------------------------------*/

	for ( n = 0 ; n < 8 ; n++ )  {
	    _latSv[n] = lat[n];
	    _lonSv[n] = lon[n];
	}
	npx = 1;
	clo_dist( &lat[1], &lon[1], &npx, &_latSv[0], &_lonSv[0], &_halfWid, &ier );

    	return;
}

/*=====================================================================*/

void pgwpts_get ( int point, int shape, float *ilat, 
			float *ilon, float *olat, float *olon, int *iret )
/************************************************************************
 * pgwpts_get                                                    	*
 *                                                                      *
 * A note about watch boxes and their points:				*
 * A parallelogram watch box consists of eight points; points 0 and 4	*
 * define the watch axis endpoints while the remaining 6 points (1,2,3,	*
 * 5,6,7) along with the watch half-width define the parallelogram	*
 * watch corner points.  Of the remaining six points, (1,2,3) are to	*
 * one side of the axis; (5,6,7) are located on the opposite side.	*
 * When drawn consecutively as a (closed) line, they depict the watch	*
 * box.									*
 *                                                                      *
 * This function re-computes edited watch box points. "point" literally	*
 * is the watch box point what was edited (moved), and determines	*
 * the type of computation to be performed.  "point" = (0,4) indicates	*
 * compute the remaining corner and edge points (1,2,3,5,6,7) based on	*
 * the given axis endpoints (0,4); otherwise, compute a new half-width	*
 * and a new axis endpoint based on edited number "point", then		*
 * recompute all six remaining points.					*
 *									*
 * If "point" is -1, then it is assumed that the watch box is to be	*
 * initialized, i.e., points (0,4) are given and the remaining six	*
 * point:s are to be calculated using a default half-width.		*
 *                                                                      *
 * void pgwpts_get( point, shape, ilat, ilon, olat, olon, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *  point	int		Edited corner point			*
 *  shape	int		Watch shape (EW, NS, ESOL)		*
 *  *ilat	float		lat array (map coords)			*
 *  *ilon	float		lon array (map coords)			*
 *                                                                      *
 * Output parameters:                                             	*
 *  *olat	float		New lat array (map coords)		*
 *  *olon	float		New lon array (map coords)		*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 6/98						*
 * D.W.Plummer/NCEP	11/98	Changes for editing sides of pgram	*
 * S. Law/GSC		11/98	Changed parameters from x/y to lat/lon	*
 *				and renamed from pgwbxw_gpts		*
 * D.W.Plummer/NCEP	 6/99	Change to get ANCHOR pts inside watch;	*
 * 				snap half-width to nearest 5 statute	*
 *				miles; fix bug in side point editing	*
 * M. Li/GSC		10/99	Modified clo_dist and clo_dltln codes	*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * T. Piper/SAIC	 4/02	Fixed UMR for point = -1 case		*
 * D.W.Plummer/NCEP	 6/05	Apply LLSC rounding prior to cgr_inpoly	*
 ***********************************************************************/
{

int	i, ii, npx, inside, eight=8, two=2, inout[2], ier;
float	dir[2], dist, lat1, lon1, lat2, lon2;
float	latx, lonx;
float	anclat[2], anclon[2];
double	dlat, dlon, ang;
char	out_ancpts[32], anc0id[8], anc1id[8];
float	tolat[9], tolon[9];
#define LLSC(xxxx)      ( (float)G_NINT( (xxxx) * 100.0F ) / 100.0F )
/*---------------------------------------------------------------------*/

    *iret = 0;		/* never changed */
    npx = 1;

    /* 
     * Copy in coordinate array to out coordinate array 
     */
    if ( point != -1 ) {
        for (i = 0; i < 8; i++) {
	    olat[i] = ilat[i];
	    olon[i] = ilon[i];
        }
    }

    /*
     *  Save endpoints into (lat1,lon1) and (lat2,lon2).
     */
    lat1 = ilat[0];
    lon1 = ilon[0];
    lat2 = ilat[4];
    lon2 = ilon[4];

    if ( point == -1 )  {

        /*
         *  Initialize parallelogram watch box;
         *  set half-width (convert default from statute miles to meters),
         *  save endpoints, set point to 0 which will force 
	 *  computation of corner points.
         */
        _halfWid = HAFWID * SM2M;
	_latSv[0] = lat1;
	_lonSv[0] = lon1;
	_latSv[4] = lat2;
	_lonSv[4] = lon2;
	point = 0;

    }

    /*
     *  Compute new corner points based on box type (shape).
     */
    switch ( shape )  {

      case EW:		/* EAST-WEST Parallelogram	*/

	/*
	 *  Compute new distance and endpoint, if necessary.
	 */
	if ( point == 1  ||  point == 7 )  {

	    if ( !G_DIFF(_latSv[0], _latSv[4]) )
		lon1 = _lonSv[4] + ( olat[point] - _latSv[4] ) *
		    ( _lonSv[0] - _lonSv[4] ) / ( _latSv[0] - _latSv[4] );
	    else
		lon1 = olon[point];
	    lat1 = olat[point];

	    clo_dist( &olat[point], &olon[point], &npx, &lat1, &lon1, &_halfWid, &ier );

	}	
	else if ( point == 2 )  {

	    if ( !G_DIFF(_latSv[5], _latSv[7]) )
		lonx = _lonSv[5] + ( olat[point] - _latSv[5] ) *
		    ( _lonSv[7] - _lonSv[5] ) / ( _latSv[7] - _latSv[5] );
	    else
		lonx = olon[point];
	    latx = olat[point];

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}
	else if ( point == 3  ||  point == 5 )  {

	    if ( !G_DIFF(_latSv[4], _latSv[0]) )
		lon2 = _lonSv[0] + ( olat[point] - _latSv[0] ) *
		    ( _lonSv[4] - _lonSv[0] ) / ( _latSv[4] - _latSv[0] );
	    else
		lon2 = olon[point];
	    lat2 = olat[point];

	    clo_dist(&olat[point], &olon[point], &npx, &lat2, &lon2, &_halfWid, &ier );

	}
	else if ( point == 6 )  {

	    if ( !G_DIFF(_latSv[1], _latSv[3]) )
		lonx = _lonSv[1] + ( olat[point] - _latSv[1] ) *
		    ( _lonSv[3] - _lonSv[1] ) / ( _latSv[3] - _latSv[1] );
	    else
		lonx = olon[point];
	    latx = olat[point];

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}

	/*
	 *  Fix relative angles for the four corner points.
	 */
	if ( lat1 > lat2 )  {
	    dir[0] = 90.F;
	    dir[1] = 270.F;
	}
	else  {
	    dir[0] = 270.F;
	    dir[1] = 90.F;
	}
	break;

      case NS:		/* NORTH-SOUTH Parallelogram	*/ 

	/*
	 *  Compute new distance and endpoint, if necessary.
	 */
	if ( point == 1  ||  point == 7 )  {

	    if ( !G_DIFF(_lonSv[0], _lonSv[4]) )
		lat1 = _latSv[4] + ( olon[point] - _lonSv[4] ) *
		    ( _latSv[0] - _latSv[4] ) / ( _lonSv[0] - _lonSv[4] );
	    else
		lat1 = olat[point];
	    lon1 = olon[point];

	    clo_dist( &olat[point], &olon[point], &npx, &lat1, &lon1, 
		     &_halfWid, &ier );

	}	
	else if ( point == 2 )  {

	    if ( !G_DIFF(_lonSv[5], _lonSv[7]) )
		latx = _latSv[5] + ( olon[point] - _lonSv[5] ) *
		    ( _latSv[7] - _latSv[5] ) / ( _lonSv[7] - _lonSv[5] );
	    else
		latx = olat[point];
	    lonx = olon[point];

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}
	else if ( point == 3  ||  point == 5 )  {

	    if ( !G_DIFF(_lonSv[4], _lonSv[0]) )
		lat2 = _latSv[0] + ( olon[point] - _lonSv[0] ) *
		    ( _latSv[4] - _latSv[0] ) / ( _lonSv[4] - _lonSv[0] );
	    else
		lat2 = olat[point];
	    lon2 = olon[point];

	    clo_dist( &olat[point], &olon[point], &npx, &lat2, &lon2, 
		     &_halfWid, &ier );

	}
	else if ( point == 6 )  {

	    if ( !G_DIFF(_lonSv[1], _lonSv[3]) )
		latx = _latSv[1] + ( olon[point] - _lonSv[1] ) *
		    ( _latSv[3] - _latSv[1] ) / ( _lonSv[3] - _lonSv[1] );
	    else
		latx = olat[point];
	    lonx = olon[point];

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}

	/*
	 *  Fix relative angles for the four corner points.
	 */
	if ( lon1 < lon2 )  {
	    dir[0] = 0.F;
	    dir[1] = 180.F;
	}
	else  {
	    dir[0] = 180.F;
	    dir[1] = 0.F;
	}
	break;

      case ESOL:		/* EITHER-SIDE-OF-LINE Pgram	*/

	/*
	 *  Compute new distance and endpoint, if necessary.
	 */
	if ( point == 1  ||  point == 7 )  {

	    cgr_lindist( _lonSv[0], _latSv[0], _lonSv[4], _latSv[4], 
			olon[point], olat[point], &lon1, &lat1, &dist, &ier);

	    clo_dist( &olat[point], &olon[point], &npx, &lat1, &lon1, &_halfWid, &ier );

	}	
	else if ( point == 2 )  {

	    cgr_lindist( _lonSv[5], _latSv[5], _lonSv[7], _latSv[7], 
			olon[point], olat[point], &lonx, &latx, &dist, &ier);

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}
	else if ( point == 3  ||  point == 5 )  {

	    cgr_lindist( _lonSv[0], _latSv[0], _lonSv[4], _latSv[4], 
			olon[point], olat[point], &lon2, &lat2, &dist, &ier);

	    clo_dist( &olat[point], &olon[point], &npx, &lat2, &lon2, &_halfWid, &ier );

	}
	else if ( point == 6 )  {

	    cgr_lindist( _lonSv[1], _latSv[1], _lonSv[3], _latSv[3], 
			olon[point], olat[point], &lonx, &latx, &dist, &ier);

	    clo_dist( &olat[point], &olon[point], &npx, &latx, &lonx, &_halfWid, &ier );

	    _halfWid = _halfWid / 2.0F;

	}

	/*
	 *  Fix/compute relative angles for the four corner 
	 *  and two edge points.
	 */
	if ( G_DIFF(lon1, lon2) )  {

	    if ( lat1 > lat2 )  {
		dir[0] = 90.F;
		dir[1] = 270.F;
	    }
	    else  {
		dir[0] = 270.F;
		dir[1] = 90.F;
	    }

	}
	else {

	    dlat = (double)(_latSv[4] - _latSv[0]);
	    dlon = (double)(_lonSv[4] - _lonSv[0]);

	    ang = RTD * atan2( dlat, dlon );

	    dir[0] = (float)-ang;
	    dir[1] = dir[0] + 180.0F;

	}
	break;

    }

    _halfWid = SM2M * (float)(G_NINT ( _halfWid * M2SM / 5.0F ) * 5);

    if ( point == 2 )  {
        /*
         *  If point (2) was edited, compute new axis points (0,4)
         */
        clo_dltln ( &olat[7], &olon[7], &_halfWid, &dir[0], &lat1, &lon1, &ier );
        clo_dltln ( &olat[5], &olon[5], &_halfWid, &dir[0], &lat2, &lon2, &ier );
    }
    else if ( point == 6 )  {
        /*
         *  If point (6) was edited, compute new axis points (0,4)
         */
        clo_dltln ( &olat[1], &olon[1], &_halfWid, &dir[1], &lat1, &lon1, &ier );
        clo_dltln ( &olat[3], &olon[3], &_halfWid, &dir[1], &lat2, &lon2, &ier );
    }

    pgwpts_comp( point, lat1, lon1, lat2, lon2, _halfWid, 
		dir, olat, olon, &ier );

    if (_qSnap)  {

      inside = G_FALSE;

      out_ancpts[0] = '\0';
      for ( ii = 0; ii < 8; ii++ )  {
	tolat[ii] = olat[ii];
	tolon[ii] = olon[ii];
      }

      while ( ! inside )  {

        /*
         *  "Snap" endpoints to 16-pt compass direction and 5 statute miles
         *  relative to closest ANCHOR point; re-do previous computations.
         * 
         *  Keep track of anchor points that, upon snapping of the watch box,
         *  end up outside the newly snapped box.  If they do, re-do the snap
	 *  (with the original set of points) without considering those that 
	 *  are outside.  Continue until both anchor points are inside the box.
         */
        pgwpts_snap( 0, lat1, lon1, 8, olat, olon, out_ancpts,
			&latx, &lonx, &anclat[0], &anclon[0], anc0id, &ier );
        lat1 = latx;
        lon1 = lonx;
        pgwpts_snap( 1, lat2, lon2, 8, olat, olon, out_ancpts,
			&latx, &lonx, &anclat[1], &anclon[1], anc1id, &ier );
        lat2 = latx;
        lon2 = lonx;

        pgwpts_comp( point, lat1, lon1, lat2, lon2, _halfWid, dir, 
		    tolat, tolon, &ier );

	/*
	 *  For points 2 and 6,  calculate opposite side points;
	 *  use previous directions for EW and NS; recalculate for ESOL
	 */

	if ( point == 2 || point == 6 )  {

          if ( shape == ESOL )  {

	    if ( G_DIFF(lon1, lon2) )  {

	        if ( lat1 > lat2 )  {
		    dir[0] = 90.F;
		    dir[1] = 270.F;
	        }
	        else  {
		    dir[0] = 270.F;
		    dir[1] = 90.F;
	        }

	    }
	    else {

	        dlat = (double)(tolat[4] - tolat[0]);
	        dlon = (double)(tolon[4] - tolon[0]);

	        ang = RTD * atan2( dlat, dlon );

	        dir[0] = (float)-ang;
	        dir[1] = dir[0] + 180.0F;

	    }

	  }

	  /*
	   *  Pretend we've edited point 0 to re-do all the points
	   */
          pgwpts_comp( 0, lat1, lon1, lat2, lon2, _halfWid, dir, 
		       tolat, tolon, &ier );

        }

	if ( !ERMISS(anclat[0]) || !ERMISS(anclat[1]) )  {

    	  tolat[0] = lat1;
    	  tolon[0] = lon1;
    	  tolat[4] = lat2;
    	  tolon[4] = lon2;
	  for ( ii = 0; ii < eight; ii++ )  {
	      tolat[ii] = LLSC ( tolat[ii] );
	      tolon[ii] = LLSC ( tolon[ii] );
	  }
	  cgr_inpoly ( sys_M, &two, anclat, anclon, sys_M, &eight, 
			tolat, tolon, inout, &ier );

	  inside = ( inout[0] == G_TRUE && inout[1] == G_TRUE );

	  if ( inout[0] == G_FALSE )  {
		strcat ( out_ancpts, anc0id );
		strcat ( out_ancpts, " " );
	  }
	  if ( inout[1] == G_FALSE )  {
		strcat ( out_ancpts, anc1id );
		strcat ( out_ancpts, " " );
	  }

	}
	else  {
	  inside = G_TRUE;
	}

      }

      if ( inside )  {
	 for ( ii = 0; ii < 8; ii++ )  {
	    olat[ii] = tolat[ii];
	    olon[ii] = tolon[ii];
	 }
      }

    }

    /*
     *  Set axis endpoints.
     */
    olat[0] = lat1;
    olon[0] = lon1;
    olat[4] = lat2;
    olon[4] = lon2;

    /*
     *  Save entire box.
     */
    if ( point == 0 || point == 2 || point == 4 || point == 6 )  {
	for ( i = 0 ; i < 8 ; i++ )  {
	    _latSv[i] = olat[i];
	    _lonSv[i] = olon[i];
	}
    }

    return;

}

/*=====================================================================*/

void pgwpts_expand ( float dist, int shape, float *lat, float *lon, 
                     int *iret )
/************************************************************************
 * pgwpts_expand                                                   	*
 *                                                                      *
 * This function expands the original watch to form a new watch. The    *
 * corresponding sides of the original watch and the expanded watch     *
 * have a distance specified by variable "dist" in the unit of statute  *
 * miles. The 8 latlon points of the original watch are passed in and   *
 * the 8 latlon points of the expanded watch are calculated.            *
 *                                                                      *
 * void pgwpts_expand( dist, shape, lat, lon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *  dist	float		dist. between two watches(statute miles)*
 *  shape	int		Watch shape (EW, NS, ESOL)		*
 *									*
 * Input/Output parameters:						*
 *  *lat	float		lat array (map coords) 			*
 *				input and output			*
 *  *lon	float		lon array (map coords)			*
 *				input and output			*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *				 0 - Normal				*
 *				-1 - negative distance			* 
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		11/05	initial coding				*
 ***********************************************************************/
{
int	ier;
float	dir1, dir2, new_lat, new_lon, angle1, ext_dist;
Boolean snap_save;
/*---------------------------------------------------------------------*/

    *iret = 0; 
 
    /*
     * Check the value of dist.
     */
    if ( dist < 0 ) {

      *iret = -1;
      return;
    }
    else if ( fabs(dist) < 1E-5F ) {  /* dist is too close to 0 */

      return;
    }

    /*
     * Save the current _qSnap value and set _qSnap to FALSE.
     */
    snap_save = _qSnap;
    _qSnap    = FALSE;

    /*
     * Save the latlon pt info into a local global.
     */
    pgwpts_save ( lat, lon );

    /*
     * First extend latlon point 0.
     */
    /*
     * Calculate the direction of point 0 relative to point 4.
     * and the direction of point 7 relative to point 1.
     */
    clo_direct ( &(lat[0]), &(lon[0]), &(lat[4]), &(lon[4]), 
		 &dir1, &ier );
    clo_direct ( &(lat[7]), &(lon[7]), &(lat[1]), &(lon[1]), 
		 &dir2, &ier );

    angle1 = dir1 - dir2;
    ext_dist = dist / sin (angle1 / 360 * 2 * PI); 
    if (ext_dist < 0)  ext_dist *= -1.0;

    /*
     * Extend point 0 by a distance specified by "ext_dist" and in
     * a direction specified by "dir1", Get a new pair of latlon.
     */
    ext_dist = ext_dist * SM2M;
    clo_dltln ( &(lat[0]), &(lon[0]), &ext_dist, &dir1, 
		&new_lat, &new_lon, &ier );

    /*
     * Extend the watch (without snapping) so point 0 goes to the 
     * new latlon position.
     */
    lat[0] = new_lat;
    lon[0] = new_lon;
    pgwpts_get ( 0, shape, lat, lon, lat, lon, &ier );

    /*
     * Secondly extend latlon point 2.
     */
    /*
     * Calculate the direction of point 2 relative to point 6.
     * and the direction of point 3 relative to point 1.
     */
    clo_direct ( &(lat[2]), &(lon[2]), &(lat[6]), &(lon[6]), 
		 &dir1, &ier );
    clo_direct ( &(lat[3]), &(lon[3]), &(lat[1]), &(lon[1]), 
		 &dir2, &ier );

    angle1 = dir1 - dir2;
    ext_dist = dist / sin (angle1 / 360 * 2 * PI); 
    if (ext_dist < 0)  ext_dist *= -1.0;

    /*
     * Extend point 2 by a distance specified by "ext_dist" and in
     * a direction specified by "dir1", Get a new pair of latlon.
     */
    ext_dist = ext_dist * SM2M;
    clo_dltln ( &(lat[2]), &(lon[2]), &ext_dist, &dir1, 
		&new_lat, &new_lon, &ier );

    /*
     * Extend the watch (without snapping) so point 2 goes to the 
     * new latlon position.
     */
    lat[2] = new_lat;
    lon[2] = new_lon;
    pgwpts_get ( 2, shape, lat, lon, lat, lon, &ier );

    /*
     * Thirdly extend latlon point 4.
     */
    /*
     * Calculate the direction of point 4 relative to point 0.
     * and the direction of point 5 relative to point 3.
     */
    clo_direct ( &(lat[4]), &(lon[4]), &(lat[0]), &(lon[0]), 
		 &dir1, &ier );
    clo_direct ( &(lat[5]), &(lon[5]), &(lat[3]), &(lon[3]), 
		 &dir2, &ier );

    angle1 = dir1 - dir2;
    ext_dist = dist / sin (angle1 / 360 * 2 * PI); 
    if (ext_dist < 0)  ext_dist *= -1.0;

    /*
     * Extend point 4 by a distance specified by "ext_dist" and in
     * a direction specified by "dir1", Get a new pair of latlon.
     */
    ext_dist = ext_dist * SM2M;
    clo_dltln ( &(lat[4]), &(lon[4]), &ext_dist, &dir1, 
		&new_lat, &new_lon, &ier );

    /*
     * Extend the watch (without snapping) so point 4 goes to the 
     * new latlon position.
     */
    lat[4] = new_lat;
    lon[4] = new_lon;
    pgwpts_get ( 4, shape, lat, lon, lat, lon, &ier );

    /*
     * Fourthly extend latlon point 6.
     */
    /*
     * Calculate the direction of point 6 relative to point 2.
     * and the direction of point 7 relative to point 5.
     */
    clo_direct ( &(lat[6]), &(lon[6]), &(lat[2]), &(lon[2]), 
		 &dir1, &ier );
    clo_direct ( &(lat[7]), &(lon[7]), &(lat[5]), &(lon[5]), 
		 &dir2, &ier );

    angle1 = dir1 - dir2;
    ext_dist = dist / sin (angle1 / 360 * 2 * PI); 
    if (ext_dist < 0)  ext_dist *= -1.0;

    /*
     * Extend point 6 by a distance specified by "ext_dist" and in
     * a direction specified by "dir1", Get a new pair of latlon.
     */
    ext_dist = ext_dist * SM2M;
    clo_dltln ( &(lat[6]), &(lon[6]), &ext_dist, &dir1, 
		&new_lat, &new_lon, &ier );

    /*
     * Extend the watch (without snapping) so point 6 goes to the 
     * new latlon position.
     */
    lat[6] = new_lat;
    lon[6] = new_lon;
    pgwpts_get ( 6, shape, lat, lon, lat, lon, &ier );

    /*
     * Restore the value of the _qSnap.
     */
    _qSnap    = snap_save;

}

/*=====================================================================*/

void pgwpts_setSnap ( Boolean snap )
/************************************************************************
 * pgwpts_setSnap							*
 *									*
 * This function sets the local snap variable.				*
 *									*
 * void pgwpts_setSnap (snap)						*
 *									*
 * Input parameters:							*
 *	snap		Boolean	to snap or not to snap			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/98						*
 * S. Law/GSC		11/98	Renamed from pgwbxw_setsnap		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    _qSnap = snap;

    return;
}

/*=====================================================================*/

void pgwpts_snap ( int which, float lat1, float lon1, int np, 
			float *olat, float *olon, char *out_ancpts,
			float *lat2, float *lon2, float *anclat, 
			float *anclon, char *ancid, int *iret )
/************************************************************************
 * pgwpts_snap                                                          *
 *                                                                      *
 * This function takes an input (lat,lon) and converts it to a		*
 * (lat,lon) along a 16-pt compass direction with a distance rounded to	*
 * the nearest 5 statute miles.						*
 * 									*
 * void pgwpts_snap ( which, lat1, lon1, np, olat, olon, out_ancpts, 	*
 *			lat2, lon2, anclat, anclon, ancid, iret )	*
 *                                                                      *
 * Input parameters:                                         		*
 *  which	int		?????					*
 *  lat1        float           latitute				*
 *  lon1        float           longitude				*
 *  np		int		Number of points in polygon area	*
 *  *olat	float		Array of polygon area latitudes		*
 *  *olon	float		Array of polygon area longitudes	*
 *  *out_ancpts	char		List of invalid anchor points		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *lat2        float          adjusted latitute			*
 *  *lon2        float          adjusted longitude			*
 *  *anclat      float          ANCHOR latitute				*
 *  *anclon      float          ANCHOR longitude			*
 *  *ancid       char           ANCHOR id				*
 *  *iret        int            Return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/98                                           *
 * S. Law/GSC		11/98	Renamed from pgwbxw_snap		*
 * D.W.Plummer/NCEP     12/98   Changed calling sequence of clo_closest *
 * D.W.Plummer/NCEP     12/98   Removed call to clo_tgid		*
 * D.W.Plummer/NCEP      1/99   Changed clo_ancinpoly to clo_tinpoly	*
 * D.W.Plummer/NCEP      6/99   Change to get ANCHOR pts inside watch	*
 * M. Li/GSC		10/99	Modified clo_direct, clo_dist and 	*
 *				clo_dltln codes				*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * A. Hardy/GSC         01/00	Changed calls for clo_tinpoly, _tgltln  *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          06/01   modified for "Toggle Anchor Pts" button *
 * D.W.Plummer/NCEP	 7/01	added return of ANCHOR lat and lon	*
 * D.W.Plummer/NCEP	 6/05	Apply LLSC rounding prior to clo_tinpoly*
 ***********************************************************************/
{
int	ii, maxll, npt, index, idir, nclose, ier, npx, icmp, nid;
int	dist5, order[50], ier2;
float   dist, ang16, llat2, llon2;
float   alat[50], alon[50], lat[5], lon[5], flat, flon;
float	*tolat, *tolon;
float	dirs[]=
	{   0.0F,  22.5F,  45.0F,  67.5F,  90.0F, 112.5F, 135.0F, 157.5F, 
	  180.0F, 202.5F, 225.0F, 247.5F, 270.0F, 292.5F, 315.0F, 337.5F, 0.0F};
float	dir;
char	cdir[4], id[9], anc_id[256], ina_ancpts[50], *ptr;
#define LLSC(xxxx)      ( (float)G_NINT( (xxxx) * 100.0F ) / 100.0F )
/*---------------------------------------------------------------------*/

    *iret = 0;
    npx = 1;

    G_MALLOC ( tolat, float, np, "Error allocating tolat" );
    G_MALLOC ( tolon, float, np, "Error allocating tolon" );
    for ( ii = 0; ii < np; ii++ )  {
	tolat[ii] = LLSC ( olat[ii] );
	tolon[ii] = LLSC ( olon[ii] );
    }
    clo_tinpoly( "ANCHOR", sys_M, np, tolat, tolon, &ier );
    G_FREE ( tolon, float );
    G_FREE ( tolat, float );

    maxll = sizeof(alat)/sizeof(float);
    clo_tgltln( "ANCHOR", maxll, &npt, alat, alon, &ier );

    clo_tgid ( "ANCHOR", 50, sizeof(anc_id), &nid, anc_id, &ier );

    /*
     * Check each anchor point against inactive anchor pts array.
     */
    pgwlst_getInaAncPts (ina_ancpts);
    strcat ( ina_ancpts, " " );
    strcat ( ina_ancpts, out_ancpts );

    ptr = strtok (anc_id, ";" );
    while (ptr != NULL) {
       if ( strstr(ina_ancpts, ptr) != (char*)NULL ) {
            nid--;
       }
       ptr = strtok(NULL, ";");
    }
    
    /* 
     * Deal with the case when there is no valid anchor pt in watch area.
     */
    if ( nid == 0 )  {
        lat[0] = LLSC ( lat1 + 5.0F ); 	lon[0] = LLSC ( lon1 - 5.0F );
        lat[1] = LLSC ( lat1 + 5.0F ); 	lon[1] = LLSC ( lon1 + 5.0F );
        lat[2] = LLSC ( lat1 - 5.0F ); 	lon[2] = LLSC ( lon1 + 5.0F );
        lat[3] = LLSC ( lat1 - 5.0F ); 	lon[3] = LLSC ( lon1 - 5.0F );
        lat[4] = lat[0];	lon[4] = lon[0];
        clo_tinpoly( "ANCHOR", sys_M, sizeof(lat)/sizeof(float), 
                     lat, lon, &ier );
        clo_tgltln( "ANCHOR", maxll, &npt, alat, alon, &ier );
	ier = -1;
    }

    if ( ier == 0 )  {

        /*
         * Sort all anchor pts on the hotlist.
         */
	nclose = npt;
	clo_closest( alat, alon, npt, lat1, lon1, nclose, order, &ier2 );

        /*
         * Dedermine the closest valid anchor point.
         */
        index = -1;
        do {
          index++;
          clo_tclosest ( "ANCHOR", alat[order[index]], alon[order[index]], 
                         1, &ier2 );
	  clo_tgid ( "ANCHOR", 1, sizeof(id), &nid, id, &ier2 );
	  id[3] = '\0';
        }
        while ( strstr(ina_ancpts, id) != (char*)NULL );

	/*
	 *  This is the location of the closest ANCHOR point.
	 */
	*anclat = alat[order[index]];
	*anclon = alon[order[index]];

	clo_dist( &lat1, &lon1, &npx, anclat, anclon, &dist, &ier );

	/*
	 *  Convert to statute miles (nearest 5).
	 */
	dist = (float)(G_NINT ( ( dist * M2SM ) / 5.0F )) * 5.0F;
	dist5 = (int)dist;

	if ( G_DIFF(dist, 0.0F) )  {

	    *lat2 = *anclat;
	    *lon2 = *anclon;

	    clo_tclosest ( "ANCHOR", *anclat, *anclon, nclose, &ier );
	    clo_tgid ( "ANCHOR", 1, sizeof(id), &nid, id, &ier );
	    id[3] = '\0';
	    strcpy ( cdir, "-" );

	    pgwbxw_setAnchor ( which, id, *anclat, *anclon, dist5, cdir, &ier );

	}
	else  {

	    clo_direct ( &lat1, &lon1, anclat, anclon, &ang16, &ier );

            idir = G_NINT( ang16 / 22.5F );

	    dist *= SM2M;
            clo_dltln ( anclat, anclon, &dist, &dirs[idir], 
		&llat2, &llon2, &ier );
            *lat2 = llat2;
            *lon2 = llon2; 

	    dir = dirs[idir];
	    clo_compass ( &dir, cdir, &icmp, &ier );
	    clo_tclosest ( "ANCHOR", *anclat, *anclon, nclose, &ier );
	    clo_tgid ( "ANCHOR", 1, sizeof(id), &nid, id, &ier );
	    id[3] = '\0';
	    clo_tgltln ( "ANCHOR", 1, &npt, &flat, &flon, &ier );
	    pgwbxw_setAnchor ( which, id, *anclat, *anclon, dist5, cdir, &ier );

	}

	strcpy ( ancid, id );

	return;

    }
    else  {

	*lat2 = lat1;
	*lon2 = lon1;

	*anclat = RMISSD;
	*anclon = RMISSD;
	strcpy ( ancid, "NONE" );

    }

    pgwbxw_setAnchor ( which, "---", RMISSD, RMISSD, IMISSD, "---", &ier );

    return;

}

/*=====================================================================*/

void pgwpts_comp ( int point, float lat1, float lon1, float lat2, 
			float lon2, float width, float *dir, 
			float *lat, float *lon, int *iret )
/************************************************************************
 * pgwpts_comp                                                          *
 *                                                                      *
 * This function takes a parallelogram watch endpoints along with which	*
 * point was edited and the orientation (direction) of the watch, and	*
 * computes the remaining points (corner and edge middle).		*
 * 									*
 * void pgwpts_comp( point, lat1, lon1, lat2, lon2, width, dir, 	*
 *		     lat, lon, iret )					*
 *                                                                      *
 * Input parameters:                                         		*
 *  point	int		Index of edited point (0 thru 8)	*
 *  lat1	float		Latitude of endpoint #1			*
 *  lon1	float		Longitude of endpoint #1		*
 *  lat2	float		Latitude of endpoint #2			*
 *  lon2	float		Longitude of endpoint #2		*
 *  width	float		Half-width of watch			*
 *  *dir	float		Directional array			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *lat	float		Latitude array containing watch points	*
 *  *lon	float		Longitude array containing watch points	*
 *  *iret       int		Return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/98                                           *
 * S. Law/GSC		11/98	Renamed from pgwbxw_comp		*
 * M. Li/GSC		10/99	Modified clo_dltln code			*
 ***********************************************************************/
{
int	ier;
float	latc, lonc;
/*---------------------------------------------------------------------*/
    *iret = 0;

    latc = ( lat1 + lat2 ) / 2.0F;
    lonc = ( lon1 + lon2 ) / 2.0F;

    if ( point != 2 && point != 6 )  {

        /*
         *  If point (0,1,3,4,5,7) was edited, compute all non-axis points.
         *  (note dir array contains north-relative angles).
         */
        clo_dltln ( &lat1, &lon1, &width, &dir[0], &(lat[1]), &(lon[1]), &ier );
        clo_dltln ( &latc, &lonc, &width, &dir[0], &(lat[2]), &(lon[2]), &ier );
        clo_dltln ( &lat2, &lon2, &width, &dir[0], &(lat[3]), &(lon[3]), &ier );
        clo_dltln ( &lat2, &lon2, &width, &dir[1], &(lat[5]), &(lon[5]), &ier );
        clo_dltln ( &latc, &lonc, &width, &dir[1], &(lat[6]), &(lon[6]), &ier );
        clo_dltln ( &lat1, &lon1, &width, &dir[1], &(lat[7]), &(lon[7]), &ier );

    }

    else if ( point == 2 )  {

        /*
         *  If point (2) was edited, compute new points (1,2,3).  
	 *  (note dir array contains north-relative angles).
         */
        clo_dltln ( &lat1, &lon1, &width, &dir[0], &(lat[1]), &(lon[1]), &ier );
        clo_dltln ( &latc, &lonc, &width, &dir[0], &(lat[2]), &(lon[2]), &ier );
        clo_dltln ( &lat2, &lon2, &width, &dir[0], &(lat[3]), &(lon[3]), &ier );
    }

    else if ( point == 6 )  {

        /*
         *  If point (6) was edited, compute new points (5,6,7).
	 *  (note dir array contains north-relative angles).
         */
        clo_dltln ( &lat2, &lon2, &width, &dir[1], &(lat[5]), &(lon[5]), &ier );
        clo_dltln ( &latc, &lonc, &width, &dir[1], &(lat[6]), &(lon[6]), &ier );
        clo_dltln ( &lat1, &lon1, &width, &dir[1], &(lat[7]), &(lon[7]), &ier );
    }

    return;

}

/*=====================================================================*/
