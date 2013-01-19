#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  ILINE		0	/* need height and wind info */

void uka_jtin ( int njp, float jlat[], float jlon[], int nwp,
                float wlat[], float wlon[], float wspd [],
                float wlvl[], float wlvla[], float wlvlb[], int wtyp[], 
		int *nop, float olat[], float olon[], float ospd[], 
		float olvl[], float olvla[], float olvlb[], int *iret )
/************************************************************************
 * uka_jtin								*
 *                                                                      *
 * This function puts the core points, the wind barb points and the 	*
 * hash points in the correct order, and gets the wind speed values 	*
 * for the hash marks.							* 
 *                                                                      *
 * uka_jtin ( njp, jlat, jlon, nwp, wlat, wlon, wspd, wlvl, wlvla, 	*
 *            wlvlb, wtyp, nop,	olat, olon, ospd, olvl, olvla, olvlb, 	*
 *            iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *      njp             int             Number of jet core points	*
 *      jlat[]          float           Latitudes of core points	*
 *      jlon[]          float           Longitudes of core points	*
 *      nwp             int             No. of wind (barb & hash) pts	*
 *      wlat[]          float           Latitudes of wind points        *
 *      wlon[]          float           Longitudes of wind points  	*
 *      wspd[]          float           Speed of wind points (m/sec)	*
 *      wlvl[]          float           Flight level of wind pts (m)	*
 *	wlvla[]		float		Flight level above jet (m)	*
 *	wlvlb[]		float		Flight level below jet (m)	*
 *      wtyp[]          int             Wind point types		*
 *                                         1 = wind barb point		*
 *                                         2 = hash point		*
 *									*
 * Output parameters:							*
 *      *nop            int             Total number of points		*
 *      olat[]          float           Latitudes of points		*
 *      olon[]          float           Longitudes of points		*
 *      ospd[]          float           Speed of points (m/sec)		*
 *      olvl[]          float           Flight levels of points (m)	*
 *	olvla[]		float		Flight level above jet (m)	*
 *	olvlb[]		float		Flight level below jet (m)	*
 *      *iret           int             Return code			*
 *                                         0 = normal return		*
 *                                        15 = curve fit problem	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/04	Extracted from sig_jets			*
 * M. Li/SAIC		04/04	Added flight level above/below jet	*
 * M. Li/SAIC		05/04	Copied from sig_jtin			*
 * M. Li/SAIC		07/04	Added olvl to UKA_JTSP			*
 * M. Li/SAIC		01/06	Added CED projection  			*
 ***********************************************************************/
{
    int 	ii, ier;
    char	proj[8];
    float 	minlat, maxlat, lllat, lllon, urlat, urlon;
    float 	angle1, angle2, angle3;
    float	dens, crvscl;
    float 	px[MAXPTS], py[MAXPTS], qx[MAXPTS], qy[MAXPTS],
          	tx[MAXPTS], ty[MAXPTS];
    int    	otyp[MAXPTS];
    int		widx[MAXPTS];
/*---------------------------------------------------------------------*/
    *iret   = 0;

    dens = 5.0F;
    crvscl = 30.0F;
    *nop = MAXPTS;
    minlat = 9999.0F;
    maxlat = -9999.0F;
    for ( ii = 0; ii < njp; ii++ ) {
    	minlat = G_MIN ( minlat, jlat[ii] );
    	maxlat = G_MAX ( maxlat, jlat[ii] );

    }

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
    else {
	/*
         * Use CED projection.
         */
	strcpy ( proj, "CED" );
        angle1 = 0.0F;
        angle2 = 0.0F;
        angle3 = 0.0F;
        lllat = -90.0F;
        lllon = -180.0F;
        urlat = 90.0F;
        urlon = 180.0F;
    }
    gsmprj ( proj, &angle1, &angle2, &angle3,
     	     &lllat, &lllon, &urlat, &urlon, &ier, strlen(proj) );
    gtrans ( sys_M, sys_D, &njp, jlat, jlon, px, py, &ier,
     	     strlen(sys_M), strlen(sys_D) );
    gtrans ( sys_M, sys_D, &nwp, wlat, wlon, qx, qy, &ier,
     	     strlen(sys_M), strlen(sys_D) );
    cgr_insert ( px, py, njp, qx, qy, nwp, dens, crvscl,
                 tx, ty, nop, widx, &ier );

    if ( ier != 0 ) {
        *iret = 15;
    }
    else {
    	gtrans ( sys_D, sys_M, nop, tx, ty, olat, olon, &ier,
     		 strlen(sys_D), strlen(sys_M) );

    	for ( ii = 0; ii < *nop; ii++) {
	    olvl[ii]  = SIGRLMS;
	    olvla[ii] = SIGRLMS;
	    olvlb[ii] = SIGRLMS;
	    ospd[ii]  = SIGRLMS;
	    otyp[ii]  = ILINE;
      	}
    	for ( ii = 0; ii < nwp; ii++ ) {
	    ospd[widx[ii]] = wspd[ii];
	    olvl[widx[ii]] = wlvl[ii];
	    olvla[widx[ii]] = wlvla[ii];
	    olvlb[widx[ii]] = wlvlb[ii];
	    otyp[widx[ii]] = wtyp[ii]; 
    	}

    	/*
     	 * Calculate wind speed for hash marks.
     	 */

    	uka_jtsp ( *nop, otyp, ospd, olvl, &ier );
    }

}
