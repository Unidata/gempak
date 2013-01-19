#include "geminc.h"
#include "gemprm.h"

#define NUM_SNAP	( 50 )  	/* number of closest SNAPs */
#define CLUSTER_DIST	( 30.0F )	/* clustering distance */

static	Boolean	clustering_load = False;	
static	float	_scdist;		/* Distance to define two 
					     consecutive as clustering */

/************************************************************************
 * closnap.c                                                            *
 *                                                                      *
 * This module contains the subroutines to find the nearest SNAP for a  *
 * single GFA point or a GFA polygon.  	                  		*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *   public functions:                                                  *
 *	clo_snapPt 		-	find nearest SNAP (snap point)	*
 *	clo_snapPtGFA 		-	find non-clustering snap point	*
 *	clo_snapPoly 		-	snap entire polygon		*
 *	clo_isCluster 		-	check clustering of two points	*
 *                                                                      *
 *   private functions:                                                 *
 *	clo_snapOneRound 	-	snap all point in a polygon	*
 *	verifySnapPt 		-	check if a point is at the other*
 *					side of the GFA			*
 *                                                                      *
 * Log:                                                                 *
 * B. Yin/SAIC		12/05	Moved and modified from pgsmear module  *
 * D.W.Plummer/NCEP	02/06	Redo algorithm; rm verifySnapPtOutside	*
 * J. Wu/SAIC		03/07	Add de-clustering processing	  	*
 ***********************************************************************/
                                                                              
/*
 *  Private functions
 */

static Boolean verifySnapPt(	int             npts,
                               float           smearLat[],
                               float           smearLon[],
                               float           origLat,
                               float           origLon,
                               float           snapLat,
                               float           snapLon );

static void clo_snapOneRound(	Boolean		expandOnly, 
	 			float 		tolerance, 
				Boolean 	reorder, 
				Boolean 	closed, 
				int 		nin, 
				float 		*inLat, 
				float 		*inLon, 
				int 		*nout,  
				float 		*outLat, 
				float 		*outLon, 
				int 		*iret );

/*
 *  Public functions
 */
void clo_snapPt ( int indx, float lat, float lon, int indx2, int npts,
                  float smearLat[], float smearLon[], Boolean expandOnly, 
		  float tolerance, float *snapLat, float *snapLon, int *iret )
/************************************************************************
 * clo_snapPt		                                                *
 *                                                                      *
 * This function finds the nearest SNAP to the input GFA point.         *
 *                                                                      *
 * THIS FUNCTION ASSUMES THE ORIGINAL SEQUENCE OF POINTS IS ORIENTED	*
 * IN A CLOCKWISE DIRECTION (i.e., USE CGR_REORDER).			*
 *                                                                      *
 * void clo_snapPt ( indx, lat, lon, npts, smearLat, smearLon,         	*
 *                   expandOnly, snapLat, snapLon, iret )    		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	indx		int		index of the input point	*
 *      lat             float           lat of the GFA point            *
 *      lon             float           lon of the GFA point            *
 * 	indx2		int		index of the 2nd input point	*
 *      npts            int             the number of GFA points        *
 *      smearLat        float[]         lats of all GFA points          *
 *      smearLon        float[]         lons of all GFA points          *
 *      expandOnly      Boolean         flag to expand GFA only         *
 *      tolerance       float           closeness tolerance		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *snapLat        float           lat of the snapped point        *
 *      *snapLon        float           lon of the snapped point        *
 *      *iret           int             return code                     *
 *                                      =  0    normal return           *
 *                                      =  1    no change               *
 *                                      =  2    point insertion		*
 *                                      = -1    no snapped point found  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          1/05    Created                                 *
 * E. Safford/SAIC      02/05   check if lat/lon is a snap point        *
 * E. Safford/SAIC      06/05   use  10nm distance check on VORs only   *
 * J. Wu/SAIC           10/05   compare "dist" with float number        *
 * J. Wu/SAIC           10/05   remove the priority check               *
 * E. Safford/SAIC      11/05   break after checking all possible pts   *
 * B. Yin/SAIC		12/05	moved from pgsmear to closnap module	*
 *				added a parameter (indx)		*
 * D.W.Plummer/NCEP	04/06	Redo algorithm to use cgr_qrol; updt doc*
 * D.W.Plummer/NCEP	04/06	Account for sparse snap points (iret=2)	*
 * D.W.Plummer/NCEP	07/06	Introduce ASSUMED_SNAP_PT distance	*
 * D.W.Plummer/NCEP     09/06   Replace ASSUMED_SNAP_PT w/ tolerance    *
 * D.W.Plummer/NCEP	11/06	Add cgr_qrol tolerance for 'on the line'*
 ***********************************************************************/
{
    int         nclose, ier, ii, nStn, nPoints, firstSnapIndex;
    float       dist, snpLat[ NUM_SNAP ], snpLon[ NUM_SNAP ];
    float	*smLat, *smLon, tlat, tlon, tol;
    int		snap_indx1, snap_indx2, *inout;
    Boolean     done, firstSnap;
    int		closed = G_FALSE, rol, nSegPoints = 2;
    float	xL1[2], yL1[2], xL2[2], yL2[2];
    char	qrol_tol[128];
/*---------------------------------------------------------------------*/

    /*
     *  Default case, no change
     */
    *iret = 1;
    *snapLat = lat;
    *snapLon = lon;

    /*
     * Transfer smear points and index to local variables
     */
    G_MALLOC ( smLat, float, npts+1, "smLat" );
    G_MALLOC ( smLon, float, npts+1, "smLon" );
    for ( ii = 0; ii < npts; ii++ )  {
	smLat[ii] = smearLat[ii];
	smLon[ii] = smearLon[ii];
    }
    snap_indx1 = indx;
    snap_indx2 = indx2;
    tlat = smLat[snap_indx1];
    tlon = smLon[snap_indx1];

    /*
     *  Find NUM_SNAP nearest snap points
     */
    nclose = NUM_SNAP;
    clo_tclosest ( "SNAP", tlat, tlon, nclose, &ier );
    clo_tgltln ( "SNAP", NUM_SNAP, &nStn, snpLat, snpLon, &ier );

    /*
     * Eliminate those points INSIDE the smear polygon
     */
    G_MALLOC ( inout, int, nStn, "inout" );

    xL1[0] = smLon[(snap_indx1+npts-1)%npts]; 
    yL1[0] = smLat[(snap_indx1+npts-1)%npts];
    xL1[1] = smLon[(snap_indx1+npts  )%npts]; 
    yL1[1] = smLat[(snap_indx1+npts  )%npts];
    xL2[0] = smLon[(snap_indx2+npts+1)%npts]; 
    yL2[0] = smLat[(snap_indx2+npts+1)%npts];
    xL2[1] = smLon[(snap_indx2+npts  )%npts];
    yL2[1] = smLat[(snap_indx2+npts  )%npts];

    /*
     *  Get the snap point
     */
    done = False;
    firstSnap = False;
    ctb_pfstr( "SNAP_QROL_TOL", qrol_tol, &ier );
    if ( ier == 0 )  {
	sscanf ( qrol_tol, "%f", &tol );
    }
    else  {
	tol = 0.001F;
    }

    while ( !done ) {
                                                                                       
        for ( ii = 0; ii < nclose; ii++ ) {
                                                                                    
            nPoints = 1; 

            clo_dist ( &tlat, &tlon, &nPoints, &(snpLat[ii]), &(snpLon[ii]), 
	      	       &dist, &ier );

            if ( ier < 0  ) {
                *iret = -1;
                return;
            }
          
            /*
             *  If the dist < tolerance, then the starting point is assumed
             *  to be a SNAP point.  No change needs to be made.
             */
	    if ( (dist * M2NM) <= tolerance )  {
                 *snapLat = snpLat[ ii ];
                 *snapLon = snpLon[ ii ];
		 *iret = 0;
                 done = True;
                 break;
            }
            
            /*
             *  If the snap is outward (expandOnly) then we need to
             *  consider whether the candidate point is inside the starting
             *  figure.  If it is we can't use it, regardless of distance.
             */
            if ( expandOnly ) {

	    	cgr_qrol ( &nSegPoints, xL1, yL1, &closed, 
			    &(snpLon[ii]), &(snpLat[ii]), &tol, &rol, &ier );
	    	inout[ii] = ( rol <= 0 ) ? 0 : 1;

                if ( inout[ii] == 0 )  {
	            cgr_qrol ( &nSegPoints, xL2, yL2, &closed, 
			    &(snpLon[ii]), &(snpLat[ii]), &tol, &rol, &ier );
	    	    inout[ii] = ( rol >= 0 ) ? 0 : 1;
		}

                if ( inout[ii] == 0 )  {

		    if ( !firstSnap ) {
                        firstSnap = True;
		        firstSnapIndex = ii;                                         		     }

                    *snapLat = snpLat[ ii ];
                    *snapLon = snpLon[ ii ];
		    *iret = 0;
                    done = True;
                    break;

                }

		if ( !done && ( ii == nclose - 1 ) && firstSnap ) {

		   /*
		    *  use the first acceptable snap point if no others are found
		    */
                   *snapLat = snpLat[ firstSnapIndex ];
                   *snapLon = snpLon[ firstSnapIndex ];
		   *iret = -2;
                   done = True;
                   break;

	        }
                                                                      
            }
                                                                                   
            else if ( verifySnapPt( npts, smLat, smLon,
                      tlat, tlon, snpLat[ ii ], snpLon[ ii ] ) ) {
                *snapLat = snpLat[ ii ];
                *snapLon = snpLon[ ii ];
		*iret = 0;
                done = True;
                break;
                                                                            
            }
        }
                                                                                    
        if ( ii >= nclose ) {
	    break;
        }
    }

    /*
     * Insert a point for sparse snap point cases.
     */

    if ( *iret != 0 )  {
        cgr_inpoly ( sys_M, &nStn, snpLat, snpLon,
                     sys_M, &npts, smLat, smLon, inout, &ier );
	for ( ii = 0; ii < nStn; ii++ )  {
	    if ( inout[ii] == 0 )  {
	    	cgr_qrol ( &nSegPoints, xL2, yL2, &closed, 
			&(snpLon[ii]), &(snpLat[ii]), &tol, &rol, &ier );
	    	inout[ii] = ( rol <= 0 ) ? 0 : 1;
	    }
	    if ( inout[ii] == 0 )  {
		*snapLat = snpLat[ ii ];
		*snapLon = snpLon[ ii ];
		*iret = 2;
		break;
	    }
	}
    }
                                                                           
    G_FREE ( inout, int );
    G_FREE ( smLat, float );
    G_FREE ( smLon, float );

}
                                                                                
/*=====================================================================*/

void clo_snapPtGFA ( int indx, float lat, float lon, int indx2, 
		  int ncheck, float *checkLat, float *checkLon, int npts,
                  float smearLat[], float smearLon[], Boolean checkDist, 
		  Boolean expandOnly, float tolerance, float *snapLat, 
		  float *snapLon, int *iret )
/************************************************************************
 * clo_snapPtGFA		                                        *
 *                                                                      *
 * This function finds the nearest SNAP to the input GFA point. 	*
 * 									*
 * The new SNAP point Ps should meet the following criteria:		*
 * 									*
 * (1) away from points P(indx-1) & P(indx2+1) with a table-driven	*
 *     clustering distance "GFA_SNAP_CLUSTER_DIST", if checkDist is True*
 * (2) all points from P(indx) to P(indx2) should be at the right of 	*
 *     segment P(indx-1)->Ps and at the left of P(indx2+1)->Ps, if 	*
 *     "expandOnly" is True (points are ordered clockwise).		*
 * (3) away from points specified in (checkLat, checkLon) with the	*
 *     clustering distance "GFA_SNAP_CLUSTER_DIST". 			*
 *                                                                      *
 * THIS FUNCTION ASSUMES THE ORIGINAL SEQUENCE OF POINTS IS ORIENTED	*
 * IN A CLOCKWISE DIRECTION (i.e., USE CGR_REORDER).			*
 *                                                                      *
 * The points should be ordered in clockwise direction if "expandOnly"	*
 * is TRUE.								*
 *                                                                      *
 * void clo_snapPtGFA ( indx, lat, lon, indx2, ncheck, checkLat, 	*
 *			checkLon, npts, smearLat, smearLon, checkDist,	*
 *			expandOnly, snapLat, snapLon, iret )    	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	indx		int		index of the input point	*
 *      lat             float           lat of the GFA point            *
 *      lon             float           lon of the GFA point            *
 * 	indx2		int		index of the 2nd input point	*
 *      ncheck		int             # of extra points to check   	*
 *      *checkLat       float         	lats of extra check points	*
 *      *checkLon       float         	lons of extra check points	*
 *      npts            int             the number of GFA points        *
 *      smearLat        float[]         lats of all GFA points          *
 *      smearLon        float[]         lons of all GFA points          *
 *      checkDist       Boolean         flag for addition dist check 	*
 *      expandOnly      Boolean         flag to expand GFA only         *
 *      tolerance       float           closeness tolerance		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *snapLat        float           lat of the snapped point        *
 *      *snapLon        float           lon of the snapped point        *
 *      *iret           int             return code                     *
 *                                      =  0    normal return           *
 *                                      =  1    no change               *
 *                                      =  2    point insertion		*
 *                                      = -1    no snapped point found  *
 *                                                                      *
 * Return parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           03/07   cloned from clo_snapPt and added	*
 *                              de-clustering capablitity            	*
 ***********************************************************************/
{
    int         nclose, ier, ii, nStn, nPoints, firstSnapIndex, jj;
    float       dist, snpLat[ NUM_SNAP ], snpLon[ NUM_SNAP ];
    float	*smLat, *smLon, tlat, tlon, tol;
    int		snap_indx1, snap_indx2, *inout;
    Boolean     done, firstSnap, qualify;
    int		closed = G_FALSE, rol, nSegPoints = 2;
    float	xL2[2], yL2[2], xb[2], yb[2], xa[2], ya[2], xt, yt;
    char	qrol_tol[128], cscdist[16];
/*---------------------------------------------------------------------*/

    /*
     *  Default case, no change
     */    
    *iret = 1;
    *snapLat = lat;
    *snapLon = lon;

    /*
     * Transfer smear points and index to local variables
     */
    G_MALLOC ( smLat, float, npts+1, "smLat" );
    G_MALLOC ( smLon, float, npts+1, "smLon" );
    for ( ii = 0; ii < npts; ii++ )  {
	smLat[ii] = smearLat[ii];
	smLon[ii] = smearLon[ii];
    }
    snap_indx1 = indx;
    snap_indx2 = indx2;
    tlat = smLat[snap_indx1];
    tlon = smLon[snap_indx1];

    /*
     *  Find NUM_SNAP nearest snap points
     */
    nclose = NUM_SNAP;
    clo_tclosest ( "SNAP", tlat, tlon, nclose, &ier );
    clo_tgltln ( "SNAP", NUM_SNAP, &nStn, snpLat, snpLon, &ier );

    
    /*
     *  Load the preference settings.
     */    
    ctb_pfstr( "SNAP_QROL_TOL", qrol_tol, &ier );
    if ( ier == 0 )  {
	sscanf ( qrol_tol, "%f", &tol );
    }
    else  {
	tol = 0.001F;
    }
    
    
    if ( !clustering_load ) {
        _scdist = CLUSTER_DIST;        
	
	ctb_pfstr ( "GFA_SNAP_CLUSTER_DIST", cscdist, &ier );
        if ( ier >= 0 ) {
            _scdist = (float) atof ( cscdist );
        }
	
	clustering_load = True;	
    }

    
    /*
     *  Get the snap point
     * 
     * The point Ps should meet the following criteria:
     * 									
     * (1) away from points P(indx-1) & P(indx2+1) with distance "_scdist"
     *     when checkDist is True.
     * (2) all points from P(indx) to P(indx2) should be at the right of segment
     *     P(indx-1)->Ps and at the left of P(indx2+1)->Ps, if "expandOnly" is
     *     True.
     * (3) away from the additional check points specified.
     */    
    G_MALLOC ( inout, int, nStn, "inout" );
   
    xL2[0] = smLon[(snap_indx2+npts+1)%npts]; 
    yL2[0] = smLat[(snap_indx2+npts+1)%npts];
    xL2[1] = smLon[(snap_indx2+npts  )%npts];
    yL2[1] = smLat[(snap_indx2+npts  )%npts];

    xb[0] = smLon[(snap_indx1+npts-1)%npts]; 
    yb[0] = smLat[(snap_indx1+npts-1)%npts];
    
    xa[0] = xL2[0];
    ya[0] = yL2[0];    
   
    done = False;
    firstSnap = False;
    
    while ( !done ) {
                                                                                       
        for ( ii = 0; ii < nclose; ii++ ) {
                                                                                    
	    /*
              *  The point should not be within the clustering distance of
	      *  point indx-1 and point indx2+1. 
              */
	    if ( checkDist &&
	         ( clo_isCluster( &smLat[(snap_indx1+npts-1)%npts], 
		                &smLon[(snap_indx1+npts-1)%npts], 
		                &(snpLat[ii]), &(snpLon[ii]), _scdist ) ||
		   clo_isCluster( &yL2[0], &xL2[0], &(snpLat[ii]),
		                &(snpLon[ii]), _scdist ) ) ) {			       
	         continue;
	    }
	    
	    
	    /*
              *  The point should not be within the clustering distance of
	      *  "ncheck" points specified in (checkLat, checkLon). 
              */
	    qualify = True;
	    for ( jj = 0; jj < ncheck; jj++ ) {
	        if ( clo_isCluster( &(checkLat[jj]), &(checkLon[jj]), 
		                &(snpLat[ii]), &(snpLon[ii]), _scdist ) ) {
	            qualify = False;	            
		    break;
		}
	    }
	    
	    if ( !qualify )  continue;
	               
	    
	    /*
              *  If the dist <= tolerance and we are not in the "expansion"
	      *  mode, then the starting point is assumed to be a SNAP point
	      *  and we are done.  Otherwise, we need more checks.  
              */
	    nPoints = 1; 

            clo_dist ( &tlat, &tlon, &nPoints, &(snpLat[ii]), &(snpLon[ii]), 
	      	       &dist, &ier );

            if ( ier < 0  ) {
                *iret = -1;
                return;
            }
            
	    if ( (dist * M2NM) <= tolerance && !expandOnly ) {
                 *snapLat = snpLat[ ii ];
                 *snapLon = snpLon[ ii ];
		 *iret = 0;
                 done = True;
                 break;
            }
            
            
	    /*
              *  If the snap is outward (expandOnly) then we need to
              *  consider whether the candidate point is inside the starting
              *  figure.  If it is, we can't use it, regardless of distance.
              *
              *  Note: currently, expandOnly is True only in "SMEAR" function
	      *        to make the smear larger than the snapshots.  When
	      *        drawing/editing GFAs,  the flag is False and thus will
	      *        not require the checks within the "expandOnly" bracket.
              */
            if ( expandOnly ) {

		xb[1] = snpLon[ii];
		yb[1] = snpLat[ii];
		    
		xa[1] = snpLon[ii];
		ya[1] = snpLat[ii];		    
		    
		for ( jj = snap_indx1;  jj <= snap_indx2; jj++ ) {
		    
		    xt = smLon[(jj+npts)%npts];		
		    yt = smLat[(jj+npts)%npts];		
                   
		    cgr_qrol ( &nSegPoints, xb, yb, &closed, 
			       &xt, &yt, &tol, &rol, &ier );

	    	    inout[ii] = ( rol >= 0 ) ? 0 : 1;
		        		    
		    if ( inout[ii] == 1 )  break;
		        
	            cgr_qrol ( &nSegPoints, xa, ya, &closed, 
			       &xt, &yt, &tol, &rol, &ier );

	    	    inout[ii] = ( rol <= 0 ) ? 0 : 1;
			 
		    if ( inout[ii] == 1 )  break;
		}
		
		if ( inout[ii] == 0 )  {

		    if ( !firstSnap ) {
                        firstSnap = True;
		        firstSnapIndex = ii;
		    }

                    *snapLat = snpLat[ ii ];
                    *snapLon = snpLon[ ii ];
		    *iret = 0;
                    done = True;
                    break;

                }

		if ( !done && ( ii == nclose - 1 ) && firstSnap ) {

		   /*
		    *  use the first acceptable snap point if no others are found
		    */
                   *snapLat = snpLat[ firstSnapIndex ];
                   *snapLon = snpLon[ firstSnapIndex ];
		   *iret = -2;
                   done = True;
                   break;

	        }
                                                                      
            }

            else if ( verifySnapPt( npts, smLat, smLon,
                      tlat, tlon, snpLat[ ii ], snpLon[ ii ] ) ) {
                *snapLat = snpLat[ ii ];
                *snapLon = snpLon[ ii ];
		*iret = 0;
                done = True;
                break;
                                                                            
            }
        }

        if ( ii >= nclose ) {
	    break;
        }
    }

    /*
     * Insert a point for sparse snap point cases.
     */

    if ( *iret != 0 )  {
        cgr_inpoly ( sys_M, &nStn, snpLat, snpLon,
                     sys_M, &npts, smLat, smLon, inout, &ier );
        for ( ii = 0; ii < nStn; ii++ )  {
	    if ( inout[ii] == 0 )  {
	    	cgr_qrol ( &nSegPoints, xL2, yL2, &closed, 
			&(snpLon[ii]), &(snpLat[ii]), &tol, &rol, &ier );
	    	inout[ii] = ( rol <= 0 ) ? 0 : 1;
	    }
	    if ( inout[ii] == 0 )  {
		*snapLat = snpLat[ ii ];
		*snapLon = snpLon[ ii ];
		*iret = 2;
		break;
	    }
	}
    }
                                                                           
    G_FREE ( inout, int );
    G_FREE ( smLat, float );
    G_FREE ( smLon, float );

}
                                                                                
/*=====================================================================*/
                                                                         
static Boolean verifySnapPt ( int npts, float smearLat[], float smearLon[],
                              float origLat, float origLon, float snapLat,
                              float snapLon )
/************************************************************************
 * verifySnapPt 	                                                *
 *                                                                      *
 * This function verifies if the line between the snap point and the    *
 * original point crosses the smear area.                               *
 *                                                                      *
 * static Boolean verifySnapPt ( npts, smearLat, smearLon, origLat,	*
 *                        origLon, snapLat, snapLon )              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      npts            int             number of GFA points            *
 *      smearLat        float[]         lats of all smear points        *
 *      smearLon        float[]         lons of all smear points        *
 *      origLat         float           lat of the original point       *
 *      origLon         float           lon of the original point       *
 *      snapLat         float           lat of the snap point           *
 *      snapLon         float           lon of the snap point           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      Boolean         True if no intersection         *
 *                                      False if intersection           *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          1/05    Created                                 *
 * B. Yin/SAIC         12/05    Moved from pgsmear to closnap module    *
 ***********************************************************************/
{
    int         ii, npts2, inPts, outPts, numSmear, ier;
    int         *bpnt1, *apnt1, *bpnt2, *apnt2;
                                                                                
    float       lineLat[ 2 ], lineLon[ 2 ];
    float       *outLat, *outLon, *dist;
                                                                              
    Boolean     result;
/*---------------------------------------------------------------------*/
                                                                             
    /*
     *  Keep only two digits after the float point, otherwise the
     *  cgr_intersect will treat them as different points from those
     *  in smearLat[], smearLon[].
     */
    lineLat[ 0 ] = rint ( (double)origLat * 100 ) / 100;
    lineLat[ 1 ] = rint ( (double)snapLat * 100 ) / 100;
    lineLon[ 0 ] = rint ( (double)origLon * 100 ) / 100;
    lineLon[ 1 ] = rint ( (double)snapLon * 100 ) / 100;
                                                                               
    numSmear = npts + 1;
                                                                  
    G_MALLOC ( outLat, float, numSmear, "clo_snap_verifySnapPt outLat" );
    G_MALLOC ( outLon, float, numSmear, "clo_snap_verifySnapPt outLon" );
    G_MALLOC ( bpnt1, int, numSmear, "clo_snap_verifySnapPt bnpt1" );
    G_MALLOC ( apnt1, int, numSmear, "clo_snap_verifySnapPt anpt1" );
    G_MALLOC ( bpnt2, int, numSmear, "clo_snap_verifySnapPt bnpt2" );
    G_MALLOC ( apnt2, int, numSmear, "clo_snap_verifySnapPt anpt2" );

    npts2 = 2;
                                                                    
    /*
     *  make a closed polygon, which cgr_intersect needs.
     */
    smearLat[ npts ] = smearLat[ 0 ];
    smearLon[ npts ] = smearLon[ 0 ];
                                                                        
    inPts = numSmear;
                                                                                
    cgr_intersect ( sys_M, &numSmear, smearLat, smearLon, 
	    	    sys_M, &npts2, lineLat, lineLon, &inPts, 
		    sys_M, &outPts, outLat, outLon,
                    bpnt1, apnt1, bpnt2, apnt2, &ier );
                                                                    
    result = True;
                                                                        
    if ( outPts > 1 ) {
                                                                           
       G_MALLOC ( dist, float, outPts, "clo_snap_verifySnapPt dist" );
                                                                           
       clo_dist ( &origLat, &origLon, &outPts, outLat, outLon, dist, &ier );
                                                                       
       for ( ii = 0; ii < outPts; ii++ ) {
                                                                     
           /*
            *  Two points with distance less than 2 nm are considered as one point
            */
           if ( dist[ ii ] > 2. * NM2M )  {
                                                         
               result = False;
               break;

           }

       }

       G_FREE ( dist, float );

    }

    G_FREE ( bpnt1, int );
    G_FREE ( apnt1, int );
    G_FREE ( bpnt2, int );
    G_FREE ( apnt2, int );
    G_FREE ( outLat, float );
    G_FREE ( outLon, float );

    return result;
                     
}

/*=====================================================================*/

static void clo_snapOneRound ( Boolean expandOnly, float tolerance, Boolean reorder,
		    Boolean closed, int nin, float *inLat, float *inLon,
		    int *nout,  float *outLat, float *outLon, int *iret )
/************************************************************************
 * clo_snapOneRound	                                          	*
 *                                                                      *
 * This function snaps all points of a input polygon to the nearest VOR	*
 * points.  The resulting polygon may still have clusteirng points.	*		
 *                                                                      *
 * static void clo_snapOneRound ( expandOnly, tolerance, reorder,closed,*
 * 		       nin,inlat, inLon, nout, outLat, outLon, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      expandOnly      Boolean         flag to expand GFA only         *
 *      tolerance       Boolean         closeness tolerance		*
 *      reorder         Boolean         flag to re-order the polygon	*
 *      closed          Boolean         if the polygon is closed	*
 * 	nin		int		number of input points		*
 *      inLat		float[]		lats of the input points	*
 *      inLon		float[]		lons of the input points	*
 *									*
 * Output parameters:                                             	*
 * 	*nout		int		number of output point		*
 *      outLat		float[]         lats of snapped points		*
 *      outLon		float[]         lons of snapped points		*
 *	*iret		int		return code    		 	*
 *					=  0	normal return		*
 *					= -1	not a GFA		*
 *					= -2	failed to snap		*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/07	Extracted from pgsmear_sanpEl & enhanced*
 * J. Wu/SAIC		03/07	Replace clo_snapPt with clo_snapPtGFA	*
 * J. Wu/SAIC		03/07	add check points into clo_snapPtGFA	*
 * J. Wu/SAIC		04/07	remove insertion of snapped points	*
 ***********************************************************************/
{
    int		ier, ii, jj, ii2, done, nshift, np;
    int		ier_prev, npts, *roindex;
    float	*lat, *lon, *snapLat, *snapLon, tlat, tlon;
    char 	cscdist[16];
/*---------------------------------------------------------------------*/

    *iret = 0;
	 
    /*
     *  Check if the polygon points are less than 3
     */
    if ( nin < 3 ) {
       *iret = -1;
       return;
    }

    /*
     *  Allocate memory for smeared points, one additional space is allocated
     *  for the routine checking intersection, which needs a closed polygon
     */  
    np = 2 * ( nin + 1); 
    G_MALLOC ( lat, float, np, "clo_snapPoly lat " );
    G_MALLOC ( lon, float, np, "clo_snapPoly lon " );
    G_MALLOC ( snapLat, float, np, "clo_snapPoly snapLat " );
    G_MALLOC ( snapLon, float, np, "clo_snapPoly snapLon " );
    
    for ( ii = 0; ii < nin; ii++ ) {
	snapLat[ ii ] = rint ( (double)inLat[ ii ] * 100 ) / 100;
	snapLon[ ii ] = rint ( (double)inLon[ ii ] * 100 ) / 100;
    }
    
    
    /*
     *  Reorder the point in clockwise - clo_snapPt() requires this.
     */
    if ( reorder ) {
        G_MALLOC ( roindex, int, np, "clo_snapPoly snap roindex " );
              
        cgr_reorder ( &nin, snapLat, snapLon, roindex, &ier );
        for ( ii = 0; ii < nin; ii++ ) {
            lat[ ii ] = snapLat[ roindex[ii] ];
	    lon[ ii ] = snapLon[ roindex[ii] ];
        }
              
        G_FREE ( roindex, int );
    }
    else {
        for ( ii = 0; ii < nin; ii++ ) {
            lat[ ii ] = snapLat[ ii ];
	    lon[ ii ] = snapLon[ ii ];
        }    
    }
    

    /*
     * Retrieve the desired clustering threshold set in prefs.tbl.
     */
    if ( !clustering_load ) {
        _scdist = CLUSTER_DIST;        
	
	ctb_pfstr ( "GFA_SNAP_CLUSTER_DIST", cscdist, &ier );
        if ( ier >= 0 ) {
            _scdist = (float) atof ( cscdist );
        }
	
	clustering_load = True;	
    }


    /*
     *  Shift (rotate) the array if necessary to ensure that the first point
     *  is not part of a cluster - if it is, it must be the first point of one.         
     * 
     *  Note: only do this shift if the input is a CLOSED polygon.
     */    
    if ( closed ) {
	ii = 0;
        jj = 0;
        
	while ( clo_isCluster( &(lat[ii]), &(lon[ii]), 
                          &(lat[nin-1]), &(lon[nin-1]), _scdist ) &&
	        jj < nin ) {
            
	    tlat = lat[ ii ];
	    tlon = lon[ ii ];
	    memmove ( &(lat[ii]), &(lat[ii+1]), (nin-1)*sizeof(float) );
	    memmove ( &(lon[ii]), &(lon[ii+1]), (nin-1)*sizeof(float) );
	    lat[nin-1] = tlat;
	    lon[nin-1] = tlon;        

	    jj += 1;
        }	
    }
    
    
    /*
     *  Find the nearest VOR for each point
     */    
    ii = 0;
    done = G_FALSE;
    ier_prev = 0;
    npts = nin;
    while ( done == G_FALSE )  {

	ii2 = ii;
	while ( (ii2+1) < npts &&
	        clo_isCluster( &(lat[ii]), &(lon[ii]), 
		               &(lat[ii2+1]), &(lon[ii2+1]), _scdist ) ) {
	    ii2 += 1;
	}
	nshift = ii2 - ii;

	clo_snapPtGFA ( ii, lat[ ii ], lon[ ii ], ii2, 0, NULL, NULL, 
			npts, lat, lon, False, expandOnly, tolerance, 
			&snapLat[ii], &snapLon[ii], &ier );
        	
	if ( ier == 1 ) {  		/* keep the orginal points */
	   
	   snapLat[ ii ] = lat[ ii ];
	   snapLon[ ii ] = lon[ ii ];

	}

	else if ( ier == 2 )  {	
			
	    /*
	      * special processing for the case of sparse snap point availability.
	      * in this case, attempt to insert the returned snap point and
	      * send the old one in again.
	      */
	    if ( ier_prev == 2 )  {

		/*
		 * previous point also could not be placed... this is an
		 * unsolveable problem! remove the previously inserted point
		 * and keep the original (unsnapped) point.
		 * Note: if the snapshots are drawn correctly (within 
		 * international boundaries), this section of code should 
		 * never be executed.
		 */
		memmove ( &(lat[ii-1]), &(lat[ii]), (npts-ii)*sizeof(float) );
		memmove ( &(lon[ii-1]), &(lon[ii]), (npts-ii)*sizeof(float) );
		npts -= 1;
		ii -= 1;
		snapLat[ ii ] = lat[ ii ];
		snapLon[ ii ] = lon[ ii ];
	    }
	    else  {

		/*
		 * insert the returned point BEFORE the point being snapped.
		 */
		memmove ( &(lat[ii+1]), &(lat[ii]), (npts-ii)*sizeof(float) );
		memmove ( &(lon[ii+1]), &(lon[ii]), (npts-ii)*sizeof(float) );
		lat[ii] = snapLat[ ii ];
		lon[ii] = snapLon[ ii ];
		npts += 1;
	    }
	}

        else if ( ier < 0 ) {

	   *iret = -2;
	   done = G_TRUE;

	}
	else if ( nshift != 0 )  {   /*
					 * cluster processing - collapse
					 * the entire array back 'nshift'
					 * points.
					 */
	    memmove ( &(lat[ii]), &(lat[ii+nshift]),
		    (npts-ii-nshift)*sizeof(float) );
	    memmove ( &(lon[ii]), &(lon[ii+nshift]),
		    (npts-ii-nshift)*sizeof(float) );
	    lat[ii] = snapLat[ ii ];
	    lon[ii] = snapLon[ ii ];
	    npts -= nshift;
	}

	ii += 1;
	if ( ii >= npts )  done = G_TRUE;
	ier_prev = ier;
	
    }

    
    /*
     *  Copy the snapped points into the GFA
     */
    if ( *iret == 0 )  {
	*nout = npts;
        for ( ii = 0; ii < *nout; ii++ ) {
	    outLat[ ii ] = snapLat[ ii ];
	    outLon[ ii ] = snapLon[ ii ];
        }
    }

    G_FREE ( lat, float );
    G_FREE ( lon, float );
    G_FREE ( snapLat, float );
    G_FREE ( snapLon, float );

}

/*=====================================================================*/

Boolean clo_isCluster ( float *lat1, float *lon1, float *lat2, 
			float *lon2, float cdist )
/************************************************************************
 * clo_isCluster	                                          	*
 *                                                                      *
 * This function checks if two points within a given clustering distance*
 *									*
 * Boolean clo_isCluster ( lat1, lon1, lat2, lon2, cdist )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *lat1		float		lat of the first point		*
 *      *lon1		float		lon of the first point		*
 *      *lat2		float		lat of the second point		*
 *      *lon2		float		lon of the second point		*
 *      cdist		float		the clustering distance		*
 *									*
 * Output parameters:                                             	*
 *	clo_isCluster()	Boolean		True/False - clustering or not	*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/07	Initial coding				*
 ***********************************************************************/
{
    int		ier, one = 1;
    float	qdist;
/*---------------------------------------------------------------------*/
    
    clo_dist ( lat1, lon1, &one, lat2, lon2, &qdist, &ier );
    
    if ( ier == 0 && qdist*M2NM <= cdist )  {
        return	True;
    }
    else {
        return	False;    
    }
	 
}

/*=====================================================================*/
 
void clo_snapPoly ( Boolean expandOnly, float tolerance, Boolean reorder,
		    Boolean closed, int nin, float *inLat, float *inLon,
		    int *nout,  float *outLat, float *outLon, int *iret )
/************************************************************************
 * clo_snapPoly	                                          		*
 *                                                                      *
 * This function snaps points of a input polygon to the	nearest VOR	*
 * points without clustering.  The resulting polygon will feature: (1)	*
 * any two consecutive points are at least GFA_SNAP_CLUSTER_DIST apart	*
 * ( a table-driven clustering distance in prefs.tbl ) and (2) always 	*
 * contains (larger than) the input polygon, if "expandOnly" is TRUE.	*		
 *                                                                      *
 * void clo_snapPoly ( expandOnly, tolerance, reorder, closed, nin,	*
 * 		       inlat, inLon, nout, outLat, outLon, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      expandOnly      Boolean         flag to expand GFA only         *
 *      tolerance       Boolean         closeness tolerance		*
 *      reorder         Boolean         flag to re-order the polygon	*
 *      closed          Boolean         if the polygon is closed	*
 * 	nin		int		number of input points		*
 *      inLat		float[]		lats of the input points	*
 *      inLon		float[]		lons of the input points	*
 *									*
 * Output parameters:                                             	*
 * 	*nout		int		number of output point		*
 *      outLat		float[]         lats of snapped points		*
 *      outLon		float[]         lons of snapped points		*
 *	*iret		int		return code    		 	*
 *					=  0	normal return		*
 *					= -1	not a GFA		*
 *					= -2	failed to snap		*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/07	initial coding				*
 ***********************************************************************/
{
    int		ier, ii, jj, done, npts, nround;
    Boolean	clusterFound;
/*---------------------------------------------------------------------*/

    *iret = 0;
	 
    /*
     *  Check if the polygon points are less than 3
     */
    if ( nin < 3 ) {
       *iret = -1;
       return;
    }

    
    /*
     *  Snap the polygon - may still end up with clusteirng points.
     */          
    clo_snapOneRound ( expandOnly, tolerance, reorder, closed,
		       nin, inLat, inLon, nout, outLat, outLon, &ier );
     
    
    /*
     *  Continue for additional clustering processing until
     *  no more clustering left.
     */        
    done = False;
    nround = 0;
    while ( !done ) {
	
        npts = *nout;       
        clusterFound = False;
	
	for ( ii = 0; ii < npts; ii++ ) {	
	    for ( jj = ii+1; jj < npts; jj++ ) {
	        if ( clo_isCluster( &(outLat[ ii ]), &(outLon[ ii]), 
		         &(outLat[ jj ]), &(outLon[ jj ]), _scdist ) ) {

		    clusterFound = True;
		    break;
		}
	    }
	    
	    if ( clusterFound ) break;
	}
	
        if ( clusterFound ) {
           clo_snapOneRound ( expandOnly, tolerance, False, closed,
			    *nout, outLat, outLon, 
			    nout, outLat, outLon, &ier );
    
	    if ( ier < 0 ) done = True;
	}
	else {
	    done = True;
	}
        
	/*
	  *  Need something to break the deadlock if it happens.
	  */
	nround++;
	if ( nround > 5 ) done = True;
    }
}

/*=====================================================================*/

