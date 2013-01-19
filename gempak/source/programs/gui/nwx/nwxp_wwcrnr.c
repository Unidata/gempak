#include "nwx_cmn.h"

/************************************************************************
 * nwxp_wwcrnr.c							*
 *									*
 * This module contains watch box utility functions.			*
 *									*
 * CONENTS:								*
 *	wwcrnr_calc()	calculate watch box points			*
 ***********************************************************************/
void wwcrnr_calc ( float *side, int *iflag, char locid1[], 
		char locid2[], float dist[], float bear[], 
		float rlat[], float rlon[], int *npt, int *iret )
/************************************************************************
 * wwcrnr_calc                                                      	*
 *                                                                      *
 * This function calculates the corner points of a watch box, given two *
 * anchor points and a distance and bearing from each anchor point, and *
 * a distance from an axis endpoint to a corner.  The anchor points,    *
 * bearings and distances are used to determine the axis endpoints of   *
 * the watch box.                                                       *
 *                                                                      *
 * wwcrnr_calc ( side, iflag, locid1, locid2, dist, bear, rlat, rlon,   *
 *           npt, iret )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*side		float		Distance from endpt to corner(m)*
 *	*iflag		int		Orientation of corner wrt endpt *
 *                                        = 1 - east/west               *
 *					  = 2 - north/south             *
 *					  = 3 - either side             *
 *	locid1 [ ]	char		First anchor point              *
 *	locid2 [ ]	char		Second anchor point             *
 *	dist [2]	float		Distances from anchor points(m) *
 *	bear [2]	float  		Bearings from anchor point(deg) *
 *									*
 * Output parameters:                                                   *
 *	rlat [ ]	float		Corner latitudes		*
 *	rlon [ ]	float		Corner longitudes		*
 *	*npt		int		Number of corner points         *
 *	*iret		int		Return value			*
 *					  0 = normal return             *
 *					 -1 = station not found in table*
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP	 7/99						*
 * M. Li/GSC		10/99		Modified clo_dltln code		*
 * A. Hardy/GSC		01/99		Changed type form int to char   *
 * D. Kidwell/NCEP	 3/01		Corrected subscript error	*
 * T. Piper/GSC		 6/01		Freed aryptr			*
 * R. Tian/SAIC		 7/03		Changed to call cst_gtag	*
 * E. Safford/SAIC	12/07	rename to wwcrnr_calc()			*
 ***********************************************************************/
{
	int	ier, nret, maxlen;
	int 	iaxis;
	char	info[128], qstate[1], type[7], tmpstr[10];
	float	xlat, xlon, xlata[2], xlona[2], b1, b2;
	double	deltax, deltay, alpha;
/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 *  Initialize the clo library values.
 */
	clo_init ( &ier );

	strcpy(type, "ANCHOR");
	qstate[0] = '\0';
	maxlen = sizeof(info);

/*
 *  Find the locations of the anchor points.
 */
	for ( iaxis = 0; iaxis < 2; iaxis++ ) {
 
	    if ( iaxis == 0 ) {
		locid1[3] = '\0';
		clo_findstn ( type, locid1, qstate, 1, maxlen, &nret,
			      info, &ier );	
	    }
	    else {
	        locid2[3] = '\0';
	        clo_findstn ( type, locid2, qstate, 1, maxlen, &nret, 
		info, &ier );	
	    }

	    if ( ier == -2 ) {
		*iret = -1;
		return;
	    }

            cst_gtag ( "LAT", info, "99999", tmpstr, &ier );
            cst_crnm ( tmpstr, &xlat, &ier );
            cst_gtag ( "LON", info, "99999", tmpstr, &ier );
            cst_crnm ( tmpstr, &xlon, &ier );

/*
 *  Get the axis endpoint from the anchor point, distance and
 *  bearing.
 */
	    clo_dltln ( &xlat, &xlon, &dist[iaxis], &bear[iaxis],
                        &(xlata[iaxis]), &(xlona[iaxis]), &ier );
	}

	if ( *iflag == 1 || G_DIFF(xlona[0], xlona[1]) ) {

/*
 *  Corner points are east and west of axis.
 */
	    b1 = 90.0F;
	    b2 = 270.0F;
	}
	else if ( *iflag == 2 || G_DIFF(xlata[0], xlata[1]) ) {

/*
 *  Corner points are north and south of axis.
 */
	    b1 = 0.0F;
	    b2 = 180.0F;
	}
	else {

/*
 *  Corner points are either side of axis.
 */
	    deltax = (double)( xlona[1] - xlona [0] );
	    deltay = (double)( xlata[1] - xlata [0] );
	    alpha  = RTD *  atan( (double)(deltay/deltax) );
	    b1     = (float)(alpha + 90.);
	    b2 	   = (float)(alpha - 90.);
	    if ( b1 < 0.0F ) b1 = b1 + 360.0F;
	    if ( b2 < 0.0F ) b2 = b2 + 360.0F;

/*
 *  Convert from math degrees to degrees from north.
 */
	    if ( b1 <= 90.0F ) {
		b1 = 90.0F - b1;
	    }
	    else {
		b1 = 450.0F - b1;
	    }
	    if ( b2 <= 90.0F ) {
		b2 = 90.0F - b2;
	    }
	    else {
		b2 = 450.0F - b2;
	    }
	}
	
/*
 *  Get the corner points.
 */
	for ( iaxis = 0; iaxis < 2; iaxis++ ) {
	    clo_dltln ( &xlata[iaxis], &xlona[iaxis], side, &b1,
			&xlat, &xlon, &ier );
	    if ( iaxis == 0 ) {
		rlat[0] = xlat;
		rlon[0] = xlon;
	    }
	    else {
		rlat[3] = xlat;
		rlon[3] = xlon;
	    }

	    clo_dltln ( &xlata[iaxis], &xlona[iaxis], side, &b2,
			&xlat, &xlon, &ier );
	    if ( iaxis == 0 ) {
		rlat[1] = xlat;
		rlon[1] = xlon;
	    }
	    else {
		rlat[2] = xlat;
		rlon[2] = xlon;
	    }
	}
	*npt = 4;
}
