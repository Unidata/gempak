#include "geminc.h"
#include "gemprm.h"

#define NUM_VOR		( 1 )  /* number of closest VORs */

/************************************************************************
 * closnaponept.c                                                       *
 *                                                                      *
 * This module contains the subroutines to find the single nearest snap *
 * point for a given point.                                             *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *   public functions:                                                  *
 *	clo_snapOnePt		-	find nearest snap point		*
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	02/06	initial coding                          *
 ***********************************************************************/



void clo_snapOnePt ( float inLat, float inLon, float *snapLat, 
		     float *snapLon, int *iret ) 
/************************************************************************
 * clo_snapOnePt	                                                *
 *                                                                      *
 * This function finds the nearest snap point to the input point.       *
 *                                                                      *
 * void clo_snapOnePt ( inLat, inLon, snapLat, snapLon, iret )          *
 *                                                                      *
 * Input parameters:                                                    *
 *      inLat           float           lat of the input point          *
 *      inLon           float           lon of the input point          *
 *                                                                      *
 * Output parameters:                                                   *
 *      *snapLat        float           lat of the snapped point        *
 *      *snapLon        float           lon of the snapped point        *
 *      *iret           int             return code                     *
 *                                      =  0    normal return           *
 *                                      =  1    no change               *
 *                                      = -1    no snapped point found  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      02/06   initial coding                          *
 ***********************************************************************/
{
    int         nclose, ier, nStn;
    float       newLat[ NUM_VOR ], newLon[ NUM_VOR ];
/*---------------------------------------------------------------------*/
                                                                                       
    /*
     *  Default case, no change
     */
    *iret = 1;
    *snapLat = inLat;
    *snapLon = inLon;
                                                                                       
    /*
     *  Find nearest snap point
     */
    nclose = NUM_VOR;
    clo_tclosest ( "SNAP", inLat, inLon, nclose, &ier );

    if( ier < 0 ) {
	*iret = -1;
    }
    else {

        clo_tgltln ( "SNAP", NUM_VOR, &nStn, newLat, newLon, &ier );

        if( ier < 0 ) {
	    *iret = -1;
        }
        else {
	    *snapLat = newLat[ 0 ];
	    *snapLon = newLon[ 0 ];
	    *iret = 0;
        }
    }

}
                                                                                
