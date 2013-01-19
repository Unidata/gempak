#include "afcmn.h"

/************************************************************************
 * affzlvl.c                                             		*
 *                                                                      *
 * This module contains the freezing level processing routines.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *      af_fmtOpenFzlvl		- format open FZLVL into fmt structure	*
 *      af_fmtClosedFzlvl	- format closed FZLVL into fmt structure*
 *									*
 *   private functions:                                                 *
 *      af_verifyFzlvlEndPts	- extends FZLVL line if necessary	*
 *      af_verifyFzlInOutSeqLen	- verify length of in/out fzlvl segments*
 *      af_reorderFzlvl		- reorder a newly opened fzlvl's pts    *
 *      af_addFzlvl		- add parts of FZLVL into fmt struct	*
 *      af_needExtend		- test if FZLVL needs to be extended 	*
 *      af_extendFzlvl		- extend FZLVL line by certain distance	*
 *	af_getFzlvlLen		- read FZLVL max/min length from pref.	*
 *      af_fzlvl2fmt		- construct fmt struct from FZLVL	*
 *      af_getFzlvlStartPt	- determine the proper starting pt in FA* 
 *      af_ptReduceOpenFzlvl	- point reduce open FZLVL contours	*
 * 	af_getPointOrder	- determine if closed line is CW or CCW *
 *	af_inArea		- determine if any pts are in an FA area*
 *	af_allInArea		- determine if all pts are in an FA area*
 *      af_loadAreaPts          - load the points from an FA area       *
 *	af_revPointOrdr		- reverse the point order		*
 *      af_removeGaps           - remove short gaps from open contours	*
 *      af_ptReduceClosedFzlvl  - reduce points for closed fzlvl	*
 ***********************************************************************/


static int af_verifyFzlvlEndPts	( int nPoly, float *latPoly, float *lonPoly, 
				  int npts, const float *latPts, 
				  const float *lonPts, int *noutPts, 
				  float **latOut, float **lonOut, int *iret );

static void af_verifyFzlInOutSegLen ( int inPts, const float inLat[], 
				const float inLon[], Boolean inInOut[], 
				Boolean closed,
				int *outPts, float outLat[], float outLon[],
				Boolean outInOut[], int *iret );

static void af_reorderFzlvl( int npts, const float *xIn, const float *yIn,
			     const Boolean *inInOut, 
			     int *nReorder, float *xReorder, 
			     float *yReorder, Boolean *outInOut, int *iret );

static void af_addFzlvl 	( const VG_DBStruct *elIn, const char *FAarea, 
			  	  int nLine, const float *latLine, 
			  	  const float *lonLine, const Boolean *inout, 
			  	  int *numFmt, GFA_Elem_Format **fmt );

static Boolean af_needExtend 	( float latPt, float lonPt, int nPoly, 
			       	  float *latPoly, float *lonPoly );

static void af_extendFzlvl 	( float latPt1, float lonPt1, float latPt2, 
			     	  float lonPt2, float *latExt, float *lonExt );

static void af_getFzlvlLen 	( void );

static void af_fzlvl2fmt	( const VG_DBStruct *elIn, const char *FAarea, 
			  	  Boolean openLine, int npts, 
				  float *latPts, float *lonPts, 
				  int *numFmt, GFA_Elem_Format *fmt, int *iret );

static void af_getFzlvlStartPt  ( float *latPts, float *lonPts,
				  int npts, Boolean *inout, 
				  int *startPt, int *iret );

static void af_ptReduceOpenFzlvl( int inPts, float inLat[], float inLon[], 
				  int *outPts, float outLat[], float outLon[], 
				  int *iret );

static int af_getPointOrder( 	int inPts, float inLat[], float inLon[], 
				int *iret );

static Boolean af_inArea( int npts, Boolean inoutFlg[], int *iret );

static Boolean af_allInArea( int npts, Boolean inoutFlg[], int *iret );

static void af_loadAreaPts( const gpc_polygon *bnds, int area, int contour, 
    		      float **lat, float **lon, int *npts, int *iret );

static void af_revPointOrdr( 	int npts, float *xPts, float *yPts, int *iret );

static void af_removeGaps( 	int npts, int startPt, float inLat[], 
				float inLon[], Boolean inputFlgs[], 
				int *ptsInArea, float outLat[], float outLon[], 
				Boolean outputFlgs[], int *iret );

static void af_ptReduceClosedFzlvl( int inPts, float inLat[], float inLon[],
					int *outPts, float outLat[], float outLon[],
					int *iret );

/*
 *  Global variables
 */
static  float   _minFzlvlLen    = -1.;  /* minimum Fzlvl length */
static  float   _maxFzlvlGap    = -1.;  /* maximum Fzlvl gap length */




#define		NEAR_CLIP_PT	( 5.0F )
#define		CW_LINE 	( 1 )
#define		CCW_LINE  	( -1 )

/*=====================================================================*/

void af_fmtOpenFzlvl ( const gpc_polygon *bnds, const VG_DBStruct *elIn, 
			      int *numFmt, GFA_Elem_Format **fmt, int *iret)
/************************************************************************
 * af_fmtOpenFzlvl	                                                *
 *                                                                      *
 * This routine formats the input FZLVL element and put it in the gfa	*
 * format structure.							*
 *                                                                      *
 * void af_fmtOpenFzlvl ( bnds, elIn, numFmt, fmt, iret)         	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *bnds		gpc_polygon     Array of area bounds polygons	*
 *	*elIn		VG_DBStruct	the gfa element			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numFmt 	int		# of GFAs in the format struct	*
 *	**fmt		GFA_Elem_Format	gfa format structure		*
 *	*iret		int 	 	0: normal                       *
 *	 				-1: not a open line FZLVL       *
 *                                                                      *
 * Return parameters:                                                   *
 *     			None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 1/06	Created					*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	09/06	use sys_M and rounding to ensure same	*
 *				 results on all projections		*
 * E. Safford/SAIC	10/06	initialize some variables             	*
 * E. Safford/SAIC	02/07	make FZLVL extension controled by prefs *
 * B. Yin/SAIC          05/07   remove the call af_rmPtsNearClip.       *
 ***********************************************************************/
{
    int         ier, ii, jj, kk, nclip = 0, npoly = 0;
    int         newPts = 0, ctr = 0;
    float       *xPoly, *yPoly, *xClip = NULL, *yClip = NULL;
    float       *latPoly = NULL, *lonPoly = NULL;
    float       *latNewLine = NULL, *lonNewLine = NULL;
    char        tmpStr[ 32 ], cval[ 32 ];
                                                                                               
    Boolean     *clipInOut = NULL, extend = FALSE;
/*--------------------------------------------------------------------*/

    /*
     *  If the GFA is not a open line FZLVL, return -1.
     */
    cvg_getFld ( elIn, TAG_GFA_AREATYPE, tmpStr, &ier );

    if ( ! ( ( strcasecmp ( tmpStr, "FZLVL" ) == 0 ) &&
	     ( elIn->hdr.closed == 0 ) ) ) {

	*iret = -1;
	return;

    }

    /*
     *  Get the ENABLE_FZLVL_EXTEND flag from the prefs.tbl.  A setting 
     *  of TRUE means the open FZLVL contours should be extended to 
     *  intersect FA Area bounds.  The default is FALSE.
     */
    ctb_pfstr ( "ENABLE_FZLVL_EXTENSION", cval, &ier );
    if ( ier >= 0 ) {
        if( strcasecmp( cval, "TRUE" ) == 0 ) {
            extend = TRUE;      
        } 
    }


    /* 
     *  Loop through every area and every contour of each area.
     *  Clip the FZLVL line against the contour and put the 
     *  result into fmt.
     */
    for ( ii = 0; ii <	NUM_FA_AREAS; ii++ ) {

	for ( jj = 0; jj < bnds[ ii ].num_contours; jj++ ) {

	    G_MALLOC ( xPoly, float, bnds[ ii ].contour[ jj ].num_vertices + 1, 
	    	       "AF_FMTOENFZLVL xPoly" );
	    G_MALLOC ( yPoly, float, bnds[ ii ].contour[ jj ].num_vertices + 1, 
	    	       "AF_FMTOENFZLVL yPoly" );
	    G_MALLOC ( latPoly, float, bnds[ ii ].contour[ jj ].num_vertices + 1, 
	    	       "AF_FMTOENFZLVL xPoly" );
	    G_MALLOC ( lonPoly, float, bnds[ ii ].contour[ jj ].num_vertices + 1, 
	    	       "AF_FMTOENFZLVL yPoly" );

 	    /*
	     *  Get all points in the contour and close it.
	     */
	    for ( kk = 0; kk < bnds[ ii ].contour[ jj ].num_vertices; kk++ ) {

		xPoly[ kk ] = bnds[ ii ].contour[ jj ].vertex[ kk ].x;
		yPoly[ kk ] = bnds[ ii ].contour[ jj ].vertex[ kk ].y;

	    }

	    xPoly[ bnds[ ii ].contour[ jj ].num_vertices ] = xPoly[ 0 ];
	    yPoly[ bnds[ ii ].contour[ jj ].num_vertices ] = yPoly[ 0 ];

	    npoly = bnds[ ii ].contour[ jj ].num_vertices + 1;

	    /*
	     *  Put the polygon (FA Area) in map coordinates and round off
	     *  to ensure the results are the same when using coordinates 
	     *  normalized for widely differing projections.
	     */
	    gtrans ( sys_N, sys_M, &npoly, xPoly, yPoly, latPoly, 
	    	     lonPoly, &ier, strlen(sys_N), strlen(sys_M) );

            for( ctr=0; ctr<npoly; ctr++ ) {
                latPoly[ctr] = ( (float)( 
			( (int)( ROUNDUP( latPoly[ctr] * 1000 ) ) ) ) ) / 1000.0;
                lonPoly[ctr] = ( (float)( 
			( (int)( ROUNDUP( lonPoly[ctr] * 1000 ) ) ) ) ) / 1000.0;
	    }




	    /*
	     *  Check if it is necessary to extend the line.
	     */
	    if( extend ) {
 	        af_verifyFzlvlEndPts ( npoly, latPoly, lonPoly, 
		   elIn->elem.gfa.info.npts, &(elIn->elem.gfa.latlon[ 0 ]),
	    	   &(elIn->elem.gfa.latlon[ elIn->elem.gfa.info.npts ]), 
		   &newPts, &latNewLine, &lonNewLine, &ier );
            }
	    else {
	        newPts = elIn->elem.gfa.info.npts;
                G_MALLOC( latNewLine, float, newPts, "AF_FMTOPENFZLVL latNewLine" );
                G_MALLOC( lonNewLine, float, newPts, "AF_FMTOPENFZLVL latNewLine" );

                for( ctr=0; ctr<newPts; ctr++ ) {
		    latNewLine[ ctr ] = elIn->elem.gfa.latlon[ ctr ];
		    lonNewLine[ ctr ] = elIn->elem.gfa.latlon[ ctr + newPts ];
		}
	    }


	    /*
	     *  Clip the line against the FA Area
	     */
            cgr_linepoly ( newPts, latNewLine, lonNewLine,
                           npoly, latPoly, lonPoly,
                           &nclip, &xClip, &yClip, &clipInOut, &ier );
                                                                                               
            G_FREE ( latNewLine, float );
            G_FREE ( lonNewLine, float );
            G_FREE ( xPoly, float );
            G_FREE ( yPoly, float );
            G_FREE ( latPoly, float );
            G_FREE ( lonPoly, float );
                                                                                               
            af_addFzlvl ( elIn, _FA_Area[ ii ], nclip, xClip, yClip,
                          clipInOut, numFmt, fmt );
                                                                                               
            G_FREE ( xClip, float );
            G_FREE ( yClip, float );
            G_FREE ( clipInOut, Boolean );
        }
                                                                                               
    }

}

/*=====================================================================*/

void af_fmtClosedFzlvl ( const gpc_polygon *bnds, VG_DBStruct *elIn, 
			      int *numFmt, GFA_Elem_Format **fmt, int *iret)
/************************************************************************
 * af_fmtClosedFzlvl	                                                *
 *                                                                      *
 * This routine formats a closed FZLVL element and puts it in the gfa	*
 * format structure.							*
 *                                                                      *
 * void af_fmtClosedFzlvl ( bnds, elIn, numFmt, fmt, iret)       	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *bnds		gpc_polygon     Array of area bounds polygons	*
 *	*elIn		VG_DBStruct	the gfa element			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numFmt 	int		# of GFAs in the format struct	*
 *	**fmt		GFA_Elem_Format	gfa format structure		*
 *	*iret		int 	 	0: normal                       *
 *	 				-1: not a closed FZLVL       	*
 *                                                                      *
 * Return parameters:                                                   *
 *     			None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	05/06	initial coding				*
 * E. Safford/SAIC	08/06	rm localEl variable			*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	08/06	rm unused allPtsIn variable		*
 * E. Safford/SAIC	09/06	use sys_M and rounding to ensure same	*
 *				 results on all projections		*
 * B. Yin/SAIC		03/07	reduce points before doing anything	*
 *				remove the smear section		*
 * B. Yin/SAIC		04/07	Close the contour before reducing points*
 * B. Yin/SAIC		06/07	remove the points reduction code	*
 ***********************************************************************/
{
    int		ier, ii, jj, kk, ctr, nclip = 0, newPts = 0;
    int		nReorder = 0, nout, npoly = 0;

    float	*xClip, *yClip, *xOut, *yOut;
    float	*latNewLine, *lonNewLine, *latPoly, *lonPoly;
    float       *xReorder, *yReorder;

    char 	tmpStr[ 32 ];

    Boolean	*inout, *inOutReordr, *inOutClip, openLine, inArea;
/*--------------------------------------------------------------------*/

    /*
     *  If the GFA is not a closed FZLVL, return -1.
     */
    cvg_getFld ( elIn, TAG_GFA_AREATYPE, tmpStr, &ier );

    if ( ! ( ( strcasecmp ( tmpStr, "FZLVL" ) == 0 ) &&
	     ( elIn->hdr.closed == 1 ) ) ) {
	*iret = -1;
	return;
    }

    /* 
     *  Loop through every area and every contour of each area.
     *  Clip the FZLVL line against the contour and put the 
     *  result into fmt.
     */
    for ( ii = 0; ii <	NUM_FA_AREAS; ii++ ) {

	for ( jj = 0; jj < bnds[ ii ].num_contours; jj++ ) {

	    inArea = G_FALSE;

	    af_loadAreaPts( bnds, ii, jj, &latPoly, &lonPoly, &npoly, &ier );

            /*
	     *  Load points for FZLVL contour
	     */
            newPts = elIn->elem.gfa.info.npts;

	    G_MALLOC ( latNewLine, float, newPts, "AF_FMTCLOSEDFZLVL latNewLine" );
	    G_MALLOC ( lonNewLine, float, newPts, "AF_FMTCLOSEDFZLVL lonNewLine" );

	    for( ctr = 0; ctr < newPts; ctr++ ) {
	        latNewLine[ ctr ] = elIn->elem.gfa.latlon[ ctr ];
	        lonNewLine[ ctr ] = 
			elIn->elem.gfa.latlon[ elIn->elem.gfa.info.npts + ctr ];
            }

	    /*
	     *  Clip the line against the FA Area
	     */
  	    cgr_linepoly ( newPts, latNewLine, lonNewLine, npoly, 
    		   latPoly, lonPoly, &nclip, &xClip, &yClip, &inOutClip, &ier );


	    G_MALLOC ( xOut, float, nclip, "AF_FMTCLOSEDFZLVL xOut" );
	    G_MALLOC ( yOut, float, nclip, "AF_FMTCLOSEDFZLVL yOut" );
	    G_MALLOC ( inout, Boolean, nclip, "AF_FMTCLOSEDFZLVL inout" );

	    /*
	     *  Check for gaps shorter than FZLVL_MAX_GAP and put them back
	     *  in the clipped contour.
	     */
  	    af_verifyFzlInOutSegLen( nclip, xClip, yClip, inOutClip, True, 
	    			&nout,  xOut, yOut, inout, &ier );

            G_FREE( xClip, float );		
	    G_FREE( yClip, float );
	    G_FREE( inOutClip, Boolean );

	    G_MALLOC ( xReorder, float, nout, "AF_FMTCLOSEDFZLVL xReorder" );
	    G_MALLOC ( yReorder, float, nout, "AF_FMTCLOSEDFZLVL yReorder" );
	    G_MALLOC ( inOutReordr, Boolean, nout, "AF_FMTCLOSEDFZLVL yReorder" );

            openLine = !(af_allInArea( nout, inout, &ier ));
            inArea   = af_inArea( nout, inout, &ier ); 

            if( inArea ) {

	        /*
	         *  Reorder the line if the Area clipped it -- we might have 
	         *  clipped the middle away.
	         */
		if( openLine ) {
                    af_reorderFzlvl( nout, xOut, yOut, inout, &nReorder, 
	    			xReorder, yReorder, inOutReordr, &ier );
                }
		else {
                    for( kk=0; kk<nout; kk++ ) {
			xReorder[kk]    = xOut[kk];
			yReorder[kk]    = yOut[kk];
			inOutReordr[kk] = inout[kk];
		    }
		    nReorder = nout;
		}

	        af_addFzlvl ( elIn, _FA_Area[ ii ], nReorder, xReorder, yReorder, 
	    		  inOutReordr, numFmt, fmt );
            }

  	    G_FREE ( latPoly, float ); 		
	    G_FREE ( lonPoly, float ); 
  	    G_FREE ( latNewLine, float ); 	
	    G_FREE ( lonNewLine, float ); 

	    G_FREE ( xOut, float );
	    G_FREE ( yOut, float );
	    G_FREE ( inout, Boolean );

  	    G_FREE ( xReorder, float );
	    G_FREE ( yReorder, float );
	    G_FREE ( inOutReordr, Boolean ); 
	}
    }

}

/*=====================================================================*/

static int af_verifyFzlvlEndPts( int nPoly, float *latPoly, float *lonPoly, 
				 int npts, const float *latPts, 
				 const float *lonPts, int *noutPts, 
				 float **latOut, float **lonOut, int *iret)
/************************************************************************
 * af_verifyFzlvlEndPts                                                 *
 *                                                                      *
 * This routine extends the FZLVL line if necessary. If either of the 	*
 * end points iseinside of the polygon and the distance from the point 	*
 * to the nearest polygon segment is less than 20 nautical miles, 	*
 * the FZLVL line needs to be extended. All points are in the map 	*
 * coordinate system. The polygon must be closed.			*
 *									*
 * Note: latOut and lonOut are allocated in the routine.		*
 *	 the calling program need free the memory.			*
 *                                                                      *
 * static int af_verifyFzlvlEndPts ( nPoly, latPoly, lonPoly, npts, 	*
 * 		 latPts, lonPts, noutPts, latOut, lonOut, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	nPoly		int		# of points in the poly plus 1	*
 *      *latPoly	float		latitudes of points in the poly	*
 *      *lonPoly	float		longitudes of points in the poly*
 *	npts		int		# of points in the FZLVL line	*
 *	*latPts		float		latitudes of points in the line	*
 *	*lonPts		float		longitudes of points in the line*
 *                                                                      *
 * Output parameters:                                                   *
 *	*noutPts	int		# of points in the result line	*
 *	**latOut	float		latitudes of points of results	*
 *	**lonOut	float		longitudes of points of results	*
 *	*iret		int		normal: 0			*
 *                                                                      *
 * Return parameters:                                                   *
 *			int		# of new points			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 1/06	Created					*
 * B. Yin/SAIC           4/06   Don't extend if the line is too short.  *
 * E. Safford/SAIC	05/06	Fix bug -- use verifyFzlInOutSegLen to  *
 *				 treat out sections as in if they are   *
 *				 shorter than the _maxFzlGap 		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	09/06	param change for cgr_linepoly        	*
 ***********************************************************************/
{
    int         ii, newPts, nClip, ier;
    int		points;

    float       lat, lon, length, *xClip, *yClip;
    Boolean     *inout, startLen, endLen;

    float	*newLat, *newLon;
    Boolean	*newInOut;
/*--------------------------------------------------------------------*/

    *iret    = 0;
    newPts   = 0;
    startLen = False;
    endLen   = False;
    points   = npts;
    *noutPts = points;

    /*
     *  Get the min FZLVL length.
     */
    if ( _minFzlvlLen < 0. ) {

	af_getFzlvlLen();

    }

    /*
     *  Clip the line against the FA area.
     */
    cgr_linepoly ( npts, (float*)latPts, (float*)lonPts,
                   nPoly, latPoly, lonPoly,
                   &nClip, &xClip, &yClip, &inout, &ier );

    G_MALLOC ( newLat, float, nClip, "AF_VERIFYFZLVLENDPTS: newLat" );
    G_MALLOC ( newLon, float, nClip, "AF_VERIFYFZLVLENDPTS: newLon" );
    G_MALLOC ( newInOut, Boolean, nClip, "AF_VERIFYFZLVLENDPTS: newLon" );

    /*
     *  Verify the lengths of any outside segments.
     */
    af_verifyFzlInOutSegLen ( nClip, xClip, yClip, inout, False,
                                &points, newLat, newLon, newInOut, &ier );

    /*
     *  Check if the start section in the area is too short.
     */
    if ( newInOut[ 0 ] == 1 ) { 

       for ( ii = 1; ii < nClip; ii++ ) {

           if ( newInOut[ ii ] == 0 ) break;

       }

       cgr_linelen ( xClip, yClip, ii, &length, &ier );

       if ( length > _minFzlvlLen ) startLen = True;

    }

    /*
     *  Check if the end section in the area is too short.
     */
    if ( newInOut[ nClip - 1 ] == 1 ) {

       for ( ii = nClip - 2; ii >= 0; ii-- ) {

           if ( newInOut[ ii ] == 0 ) break;

       }

       cgr_linelen ( &xClip[ ii + 1 ], &yClip[ ii + 1 ], nClip - ii - 1,
                     &length, &ier );

       if ( length > _minFzlvlLen ) endLen = True;

    }

    G_FREE ( xClip, float );
    G_FREE ( yClip, float );
    G_FREE ( inout, Boolean );

    G_FREE ( newLat, float );
    G_FREE ( newLon, float );
    G_FREE ( newInOut, Boolean );

    G_MALLOC ( *latOut, float, *noutPts + 2, "AF_VERIFYFZLVLENDPTS: latOut" );
    G_MALLOC ( *lonOut, float, *noutPts + 2, "AF_VERIFYFZLVLENDPTS: lonOut" );


    /*
     *  Extend the start point if necessary.
     */
    if ( startLen && af_needExtend ( latPts[ 0 ], lonPts[ 0 ], nPoly,
                         latPoly, lonPoly ) ) {

        af_extendFzlvl ( latPts[ 1 ],  lonPts[ 1 ], latPts[ 0 ], lonPts[ 0 ],
                         &lat, &lon );
        newPts++;

        (*latOut)[ 0 ] = lat;
        (*lonOut)[ 0 ] = lon;

        memcpy ( &((*latOut)[ 1 ] ), latPts, npts * sizeof ( float ) );
        memcpy ( &((*lonOut)[ 1 ] ), lonPts, npts * sizeof ( float ) );

    }

    else {

        memcpy ( &((*latOut)[ 0 ] ), latPts, npts * sizeof ( float ) );
        memcpy ( &((*lonOut)[ 0 ] ), lonPts, npts * sizeof ( float ) );

    }

    /*
     *  Extend the last point if necessay.
     */
    if ( endLen && af_needExtend ( latPts[ npts - 1 ], lonPts[ npts - 1 ], nPoly,
                         latPoly, lonPoly ) ) {

        af_extendFzlvl ( latPts[ npts - 2 ], lonPts[ npts - 2 ],
                         latPts[ npts - 1 ], lonPts[ npts - 1 ],
                         &lat, &lon );

        newPts++;

        (*latOut)[ npts + newPts - 1 ] = lat;
        (*lonOut)[ npts + newPts - 1 ] = lon;

    }

    *noutPts += newPts;

    return newPts;

}

/*=====================================================================*/

static void af_verifyFzlInOutSegLen ( int inPts, const float inLat[], 
				const float inLon[],
				Boolean inInOut[], Boolean closed,
				int *outPts, float outLat[], float outLon[],
				Boolean outInOut[], int *iret )

/************************************************************************
 * af_verifyFzlInOutSegLen                                              *
 *                                                                      *
 * This routine takes as input the points and inout array from a      	*
 * freezing level contour and checks those segments that are outside	*
 * to see if they are shorter than the maximum gap length.  If they are *
 * shorter, then the in/out (meaning the points are inside or outside   *
 * of an FA Area) flags for the points of that segment are switched     *
 * out to in.                						* 
 *                                                                      *
 * static void af_verifyFzlInOutSegLen ( inPts, inLat, inLon, inInOut,  *
 *			outPts, outLat, outLon, outInOut, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	inPts		int	number of input lat/lon points		*
 *	inLat[]		float   input array of lat points		*
 *	inLon[]		float   input array of lon points		*
 *	inInOut[]	Boolean	input array of in/out flags		*
 *      closed		Boolean	closed fzlvl flag			*
 *                                                                      *
 * Output parameters:                                             	*
 *	*outPts		int	number of output lat/lon points		*
 *	outLat[]	float   output array of lat points		*
 *	outLon[]	float   output array of lon points		*
 *	outInOut[]	Boolean	output array of in/out flags		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	05/06	initial coding				*
 * E. Safford/SAIC	06/06	fix errors in outPts and outLon		*
 * E. Safford/SAIC	08/06	add anyIn check to avoid adding a short *
 *				  segment to the start of the line	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	08/06	add closed param, fix handling of gaps  *
 *				  spanning end/start			* 
 ***********************************************************************/
{
    int		ii, jj, kk, ier;
    int		startPt, endPt, gapPts, *gapIdx;
    float	length, *gapLat, *gapLon;
    Boolean	processSeg;
/*---------------------------------------------------------------------*/

    *iret   = 0;
    *outPts = inPts;

    /*
     *  Get the max gap length and the min FZLVL length.
     */
    if ( ( _maxFzlvlGap < 0. ) || ( _minFzlvlLen < 0. ) ) {
	af_getFzlvlLen();	
    }


    /*
     *  The startPt is the first point inside the area.
     *  The endPt is the last point inside the area.
     *
     *  There may be gaps that are outside the area between startPt and endPt.
     */   
    startPt = -1; 
    endPt   = -1;
    for( ii=0; ii<inPts; ii++ ) {
        if( inInOut[ii] ) {
            if( startPt < 0 ) startPt = ii;
	    endPt = ii;	    
        }
	
    }
    

    /*
     *  Process initial points outside of the area, if any.
     */
    for( ii=0; ii<startPt; ii++ ) {
        outLat[ ii ]   = inLat[ ii ];
        outLon[ ii ]   = inLon[ ii ];
        outInOut[ ii ] = inInOut[ ii ]; 
    }



    /*
     *  If the outside part is shorter than _maxFzlvlGap, switch the inout flag
     *  so that they are treated as if they are inside.
     */
    if( startPt > -1 && endPt > -1 ) {
        for ( ii = startPt; ii <= endPt; ii++ ) {

            if ( inInOut[ ii ] == 1 ) {      	/* in the area */

                outLat[ ii ]   = inLat[ ii ];
                outLon[ ii ]   = inLon[ ii ];
                outInOut[ ii ] = inInOut[ ii ];
            }

            else {                                  /* out of the area */
 
	        processSeg = False;

                for ( jj = ii; jj < inPts; jj++ ) {

                    if ( inInOut[ jj ] == 1 ) {
		        processSeg = True;
	                break;
	            }
                }


	        /*
	         * Check length of segment.  
	         */
	        if( processSeg ) {
                    cgr_linelen ( (float*)&(inLat[ ii ]), (float*)&(inLon[ ii ]),
                            jj - ii, &length, &ier );
                }
                else {
	            length = _maxFzlvlGap * 2;
	        }


                /*
	         *  Make the outInOut flag True if the total length is
	         *  less than _maxFzlvlGap.  
	         */
                if ( length < _maxFzlvlGap ) {

                    for ( kk = ii; kk < jj; kk++ ) {
                        outLat[ kk ]   = inLat[ kk ];
                        outLon[ kk ]   = inLon[ kk ];
                        outInOut[ kk ] = True;
                    }
                    ii = jj-1;
                }
                else {

                    for ( kk = ii; kk < jj; kk++ ) {

                        outLat[ kk ]   = inLat[ kk ];
                        outLon[ kk ]   = inLon[ kk ];
                        outInOut[ kk ] = inInOut[ kk ];
                    }

                    ii = jj-1;
                }
            }
        }
    }


    /*
     *  Process final points outside of the area, if any.
     */
    for( ii=endPt+1; ii<inPts; ii++ ) {
        outLat[ ii ]   = inLat[ ii ];
        outLon[ ii ]   = inLon[ ii ];
        outInOut[ ii ] = inInOut[ ii ];
    }



    /*
     *  Handle a gap over end points when closed.
     */
    if( closed && ( startPt > 0 ) ) {

	/*
	 *
	 */
        G_MALLOC ( gapLat, float, inPts, "AF_VERIFYFZLINOUTSEGLEN gapLat" );
        G_MALLOC ( gapLon, float, inPts, "AF_VERIFYFZLINOUTSEGLEN gapLon" );
        G_MALLOC ( gapIdx, int,   inPts, "AF_VERIFYFZLINOUTSEGLEN gapIdx" );

	gapPts = 0;

	if( (endPt + 1) < inPts ) {
	    for( ii=endPt+1; ii<inPts; ii++ ) {
                gapLat[gapPts] = inLat[ii];
                gapLon[gapPts] = inLon[ii];
		gapIdx[gapPts] = ii;
	        gapPts++;      
	    }
	}
        if( startPt >= 0 ) {
	    ii=0;
            do {
                gapLat[gapPts] = inLat[ii];
		gapLon[gapPts] = inLon[ii];
		gapIdx[gapPts] = ii;
		gapPts++;
		ii++;
	    } while( ii <= startPt );
	}

        cgr_linelen ( gapLat, gapLon, gapPts, &length, &ier );


        if ( length < _maxFzlvlGap ) {

	    for( ii=0; ii<gapPts; ii++ ) {
		outInOut[ gapIdx[ii] ] = True;
	    }
	}

	G_FREE( gapLat, float )
	G_FREE( gapLon, float )
	G_FREE( gapIdx, int )

    }
}

/*=====================================================================*/

static void af_reorderFzlvl( int npts, const float *xIn, const float *yIn,
			     const Boolean *inInOut, 
			     int *nReorder, float *xReorder, 
			     float *yReorder, Boolean *outInOut, int *iret )
/************************************************************************
 * af_reorderFzlvl	                                                *
 *                                                                      *
 * This routine reorders the points of a clipped freezing level contour *
 * as necessary.  This routine is to be called after clipping.  The     *
 * inInOut array is used to determine if each point lies inside or      *
 * outside of a given area.  If the ends of the contour are determined  *
 * to be inside an area, but the middle is outside, then points must be *
 * reordered to construct a continuous line.				*
 *                                                                      *
 * static void af_reorderFzlvl( npts, xIn, yIn, inInOut, nReorder, 	*
 *				xReorder, yReorder, outInOut, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	npts 		int		# of points in the input arrays	*
 *	*xIn		float		x values                       	*
 *	*yIn		float		y values                       	*
 *	*inInOut	Boolean		input in/out flags array  	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*nReorder	int		number or reordered points	*
 *	*xReorder	float		reordered x values		*
 *	*yReorder	float		reordered y values		*
 *	*outInOut	Boolean		output in/out flags array	*
 *	*iret		int		return code			*
 *					   0 = normal			*
 *					  -1 = number of points is <= 0 *
 *					  -2 = bad input array(s)	*
 *					  -3 = bad output array(s)	* 
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	06/06 	Initial coding				*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    Boolean	initialSegment = False, finalSegment = False;
    Boolean 	anyOut 	       = False, anyIn        = False;

    int		ctr, bk, fw;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if( npts <= 0 ) {
	*iret = -1;
    }
    else if( !xIn || !yIn || !inInOut ) {
	*iret = -2;
    }
    else if( !xReorder || !yReorder || !outInOut ) {
	*iret = -3;
    }
    
    if( *iret < 0 ) return;


    /*
     *  Re-order the points if the clipping took out the middle of 
     *  the contour (re-unite tail and head of remainder).
     */
    for( ctr = 0; ctr < npts; ctr++ ) {
        if( !anyIn && inInOut[ctr] == 1 ) anyIn = True;
	if( !anyOut && inInOut[ctr] == 0 ) anyOut = True;
    }

    /*
     *  Check for initial segment inside area.
     */
    if( inInOut[0] == 1 && inInOut[1] == 1 ){
        initialSegment = True; 
    }

    /*
     *  Check for final segment inside area.
     */
    if( (inInOut[npts-1] == 1) && (inInOut[npts - 2] == 1) ) {
        finalSegment = True;
    }

    /*
     *  If an initial and final segment are in, reorder to remove 
     *  middle gap.
     */
    *nReorder=0;
    for( ctr = 0; ctr<npts-1; ctr++ ) {
	outInOut[ ctr ] = 0;
    }

    if( initialSegment && finalSegment && anyOut ) {
    
        /*
	 *  From end step back until we find the first out point.
	 */
   	for( bk=npts-1; bk>=0; bk-- ) {
	    if( inInOut[bk] == 0 ) break;
	}

        bk++;

	for( fw=bk; fw <npts-1; fw++ ) {
            xReorder[ *nReorder ] = xIn[fw];
            yReorder[ *nReorder ] = yIn[fw];
	    outInOut[ *nReorder ] = 1;
	    *nReorder += 1;
	}

	for( fw=0; fw<bk; fw++ ) {
            xReorder[ *nReorder ] = xIn[fw];
            yReorder[ *nReorder ] = yIn[fw];
	    outInOut[ *nReorder ] = inInOut[fw];
	    *nReorder += 1;
	}
    }
    else {     		/* copy to the Reorder arrays with no change */
        for( fw=0; fw<npts; fw++ ) {
            xReorder[ fw ] = xIn[fw];
            yReorder[ fw ] = yIn[fw];
	    outInOut[ fw ] = inInOut[fw];
	    *nReorder += 1;
	} 
    }
}


/*=====================================================================*/

static void af_addFzlvl ( const VG_DBStruct *elIn, const char *FAarea, 
			  int nLine, const float *latLine, 
			  const float *lonLine, const Boolean *inout,
			  int *numFmt, GFA_Elem_Format **fmt )
/************************************************************************
 * af_addFzlvl		                                                *
 *                                                                      *
 * This routine goes through the input line and test which parts of the *
 * line are in the FA area and put those parts in the gfa format 	*
 * structure.						 		*
 *                                                                      *
 * static void af_addFzlvl ( elIn, FAarea, nLine, latLine, lonLine,  	*
 *		      	     inout, numFmt, fmt )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*elIn		VG_DBStruct	the gfa element			*
 *      *FAarea		char		FA area				*
 *	nLine		int		# of points in the line		*
 *	*latPts		float		latitudes of points in the line	*
 *	*lonPts		float		longitudes of points in the line*
 *	*inout		Boolean		indicators as of in or out	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numFmt 	int		# of GFAs in the format struct	*
 *	**fmt		GFA_Elem_Format	gfa format structure		*
 *                                                                      *
 * Return parameters:                                                   *
 *	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 1/06	Created					*
 * E. Safford/SAIC	 2/06	Snapped end points             		*
 * B. Yin/SAIC           4/06   remove short FZLVL lines and add back   *
 *                              short gaps.                             *
 * E. Safford/SAIC	08/06	improve openLine flag logic		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	08/06	rm unused saveClosed variable		*
 * E. Safford/SAIC	10/06	override openLine flag for open contours*
 * E. Safford/SAIC	02/07	fix uninitialized memory condition      *
 * B. Yin/SAIC          04/07   remove repeated border points		*
 ***********************************************************************/
{
    int         ii, jj, kk, npts, ier, localPts, startPt;
    float       *xtmp, *ytmp, newX, newY, *latLocal, *lonLocal, length;
    float	*latTmp, *lonTmp;

    Boolean     segStart, *inoutLocal, openLine, *inoutTmp;
/*--------------------------------------------------------------------*/

    /*
     *  Get the max gap length and the min FZLVL length.
     */
    if ( ( _maxFzlvlGap < 0. ) || ( _minFzlvlLen < 0. ) ) {
	af_getFzlvlLen();	
    }

    /*
     *  Determine the starting point
     */
    G_MALLOC ( latTmp, float, nLine, "AF_ADDFZLVL latTmp" );
    G_MALLOC ( lonTmp, float, nLine, "AF_ADDFZLVL lonTmp" );
    G_MALLOC ( inoutTmp, Boolean, nLine, "AF_ADDFZLVL inoutLocal" );
  
    for( ii=0; ii<nLine; ii++ ) {
        latTmp[ii]   = latLine[ ii];
        lonTmp[ii]   = lonLine[ ii];
	inoutTmp[ii] = inout[ii];
    }
  
    af_getFzlvlStartPt( latTmp, lonTmp, nLine, inoutTmp, &startPt, &ier ); 

    if( startPt < 0 ) {
        G_FREE( latTmp, float );
        G_FREE( lonTmp, float );
        G_FREE( inoutTmp, Boolean );
	return;
    }


    /*
     *  If the outside part is shorter than _maxFzlvlGap, switch the inout flag
     *  so that they are treated as if they are inside.
     */
    G_CALLOC ( latLocal, float, nLine, "AF_ADDFZLVL latLocal" );
    G_CALLOC ( lonLocal, float, nLine, "AF_ADDFZLVL lonLocal" );
    G_CALLOC ( inoutLocal, Boolean, nLine, "AF_ADDFZLVL inoutLocal" );


    localPts = 0;
    af_removeGaps( nLine, startPt, latTmp, lonTmp, inoutTmp,
			  &localPts, latLocal, lonLocal, inoutLocal, &ier );

    G_FREE( latTmp, float );
    G_FREE( lonTmp, float );
    G_FREE( inoutTmp, Boolean );


    /*
     *  The openLine flag is used to determine if closed FZLVLs should be
     *  opened.  
     *
     *  This is done by analyzing the inoutLocal[] array of flags which
     *  indicate which points on the contour are inside/oustide of the 
     *  FA area.  For open contours the flag is reset to True before the call
     *  to af_fzlvl2fmt() is made.  Open contours are never closed.  Closed
     *  contours are opened if they are intersected by FA bounds and the gap
     *  that the intersection causes is greater than FZLVL_MAXIMUM_GAP_LENGTH.
     */
    openLine = False;

    for( ii=0; ii<nLine && !openLine; ii++ ) {
        if( !(inoutLocal[ii]) ) {
	    openLine = True;
	}
    }

    if ( *numFmt == 0 ) *fmt = NULL;

    G_MALLOC ( xtmp, float, localPts, "AF_ADDFZLVL xtmp" );
    G_MALLOC ( ytmp, float, localPts, "AF_ADDFZLVL ytmp" );

    segStart = False;
    npts     = 0;

    for ( kk = 0; kk < localPts; kk++ ) {

        if ( !segStart ) {

           if ( inoutLocal[ kk ] ) {            /* segment starts */

              segStart  = True;
              xtmp[ 0 ] = latLocal[ kk ];  
              ytmp[ 0 ] = lonLocal[ kk ];

              npts++;
          }

        }

        else {

          if ( !inoutLocal[ kk ] || kk == localPts - 1 ) {      /* segment ends */

             if ( kk == localPts - 1 ) {

                xtmp[ npts ] = latLocal[ kk ]; 
                ytmp[ npts ] = lonLocal[ kk ];
                npts++;
             }

             cgr_linelen( xtmp, ytmp, npts, &length, &ier );

             if ( length > _minFzlvlLen ) {

		/*
		 *  Remove repreated border points that are introduced by area clipping.
		 */
		for ( jj = 0; jj < npts - 1; jj++ ) {

		    if ( G_DIFF( xtmp[ jj ], xtmp[ jj + 1 ] ) &&
		    	 G_DIFF( ytmp[ jj ],  ytmp[ jj + 1 ] ) ) {

			if ( jj + 1 == npts - 1 ) {  /* last point */

			   npts = npts - 2;
			   break;

			} 
			else {
				
			  memmove( &xtmp[ jj ], &xtmp[ jj + 2 ],
			           ( npts - jj - 2 ) * sizeof( float ) );
			  memmove( &ytmp[ jj ], &ytmp[ jj + 2 ],
			           ( npts - jj - 2 ) * sizeof( float ) );

		          npts = npts - 2;

			}

		    }

		}

                G_REALLOC ( (*fmt), GFA_Elem_Format, (*numFmt+1),
                            "AF_FMTOPENFZLVL: fmt" );

                /*
                 *  Snap the end points
                 */
                clo_snapOnePt( xtmp[0], ytmp[0], &newX, &newY, &ier );
                if( ier >= 0 ) {
                   xtmp[0] = newX;
                   ytmp[0] = newY;
                }

                clo_snapOnePt( xtmp[npts-1], ytmp[npts-1], &newX, &newY, &ier );
                if( ier >= 0 ) {
                   xtmp[npts-1] = newX;
                   ytmp[npts-1] = newY;
                }

		if( !elIn->hdr.closed ) {
		    openLine = True;
		}


                af_fzlvl2fmt ( elIn, FAarea, openLine, npts, xtmp, ytmp, numFmt,
                               &((*fmt)[*numFmt]), &ier );
             }

             segStart = False;
             npts = 0;

          }

          else {                /* segment continues */

             xtmp[ npts ] = latLocal[ kk ]; 
             ytmp[ npts ] = lonLocal[ kk ];
             npts++;
          }

       }

    }

    G_FREE ( xtmp, float );
    G_FREE ( ytmp, float );

    G_FREE ( latLocal, float );
    G_FREE ( lonLocal, float );
    G_FREE ( inoutLocal, Boolean );

}

/*=====================================================================*/

static Boolean af_needExtend ( float latPt, float lonPt, int npoly,
			       float * latPoly, float *lonPoly )
/************************************************************************
 * af_needExtend                                                        *
 *                                                                      *
 * This routine tests if the FZLVL line needs to be extended. If the	*
 * the input point (latPt, lonPt) is inside of the polygon and the	*
 * distance from the point to the nearest polygon segment is more than 	*
 * NEAR_FA_BOUND nautical miles, the FZLVL line needs to be extended. 	*
 * All points are in map coordinate system. The polygon must be closed.	*
 *                                                                      *
 * static Boolean af_needExtend ( latPt1, lonPt1, latPt2, lonPt2, 	*
 *				  npoly, latPoly, lonPoly )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      latPt		float		latitude of the test point	*
 *      lonPt		float		longitude of the test point	*
 *	npoly		int		# of points in the poly plus 1	*
 *      *latPoly	float		latitudes of points in the poly	*
 *      *lonPoly	float		longitudes of points in the poly*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *				True:	need  extend			*
 *				False:	need not extend			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 1/06	Created					*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	09/06	use sys_N not sys_D   			*
 * E. Safford/SAIC	09/06	rm all gtrans, simplify			*
 ***********************************************************************/
{
    int		inout, one, ier, vrt1, vrt2;

    float	dist;
    float	latNearest, lonNearest;
/*--------------------------------------------------------------------*/

    inout = 0;
    one   = 1;

    /*
     *  Determine if point is inside or outside the polygon.
     *
     *  If it's outside return false.
     */
    cgr_inpoly ( sys_M, &one, &latPt, &lonPt, sys_M, &npoly, 
    		 latPoly, lonPoly, &inout, &ier );

    if ( inout == 0 ) return False;

    /*
     *  Get the nearest point in the polygon.
     */
    cgr_segdist ( &npoly, latPoly, lonPoly, &latPt, &lonPt, &dist,
                  &vrt1, &vrt2, &latNearest, &lonNearest, &ier );

    /*
     *  Calculate the distance from the point to the nearest
     *  point in the polygon.  Dist will be in meters.
     */
    clo_dist ( &latNearest, &lonNearest, &one, &latPt, &lonPt, 
    	       &dist, &ier );

    /*
     *  Return true if the distance is greater than 
     *  NEAR_FA_BOUND (converted to meters).
     */
    if ( dist <= NEAR_FA_BOUND * NM2M ) return False;
    else return True;
    
}

/*=====================================================================*/

static void af_extendFzlvl ( float latPt1, float lonPt1, float latPt2,
			     float lonPt2, float *latExt, float *lonExt )
/************************************************************************
 * af_extendFzlvl                                                       *
 *                                                                      *
 * This routine extends the line (pt1, pt2) by EXT_DIST nautical miles  *
 * from pt2. All points are in the map coordinate system.	        *
 *                                                                      *
 * static void af_extendFzlvl ( latPt1, lonPt1, latPt2, lonPt2,		*
 *			 	latOut, lonOut )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      latPt1		float		latitude of the first point	*
 *      lonPt1		float		longitude of the first point	*
 *      latPt2		float		latitude of the second point	*
 *      lonPt2		float		longitude of the second point	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *latExt		float		latitude of extended point	*
 *      *lonExt		float		longitude of extended point	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 1/06	Created					*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	09/06	use sys_N not sys_D  			*
 * E. Safford/SAIC	09/06	rm all gtrans, simplify			*
 ***********************************************************************/
{
    int		ier, one;
    float	dist, angle;
    float	distMap;
/*--------------------------------------------------------------------*/

    one = 1;
    *latExt = latPt1;  *lonExt = lonPt1;


    clo_dist ( &latPt1, &lonPt1, &one, &latPt2, &lonPt2, &distMap, &ier );
    if( ier < 0 ) return;

    cgr_dang ( &latPt1, &lonPt1, &latPt2, &lonPt2, &angle, &ier );
    if( ier < 0 ) return;

    angle *= DTR;

    dist = EXT_DIST * NM2M / distMap ;
    
    *latExt = latPt2 + dist * cos ( (float) angle );
    *lonExt = lonPt2 + dist * sin ( (float) angle );

}

/*=====================================================================*/

static void af_getFzlvlLen( void )
/************************************************************************
 * af_getFzlvlLen                                           		*
 *                                                                      *
 * This routine reads the maximum gap length and the minimum length of	*
 * FZLVL from pref.tbl.  The results are stored in two global variables.*
 *                                                                      *
 * static void af_getFzlvlLen ( void ) 			 		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	None                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 * 	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int		ier;
    char        lengthStr[ 8 ];
/*---------------------------------------------------------------------*/

    _maxFzlvlGap = FZLVL_MAX_GAP;

    ctb_pfstr ( "FZLVL_MAXIMUM_GAP_LENGTH", lengthStr, &ier );

    if ( ier >= 0 ) {

       _maxFzlvlGap = atoi ( lengthStr );

    }

    _minFzlvlLen = FZLVL_MIN_LEN;

    ctb_pfstr ( "FZLVL_MINIMUM_LENGTH", lengthStr, &ier );

    if ( ier >= 0 ) {

       _minFzlvlLen = atoi ( lengthStr );

    }

}

/*=====================================================================*/

static void af_fzlvl2fmt( const VG_DBStruct *elIn, const char *FAarea, 
			  Boolean openLine, int npts, 
			  float *latPts, float *lonPts,
			  int *numFmt, GFA_Elem_Format *fmt, int *iret)
/************************************************************************
 * af_fzlvl2fmt		                                                *
 *                                                                      *
 * This routine constructs the gfa format structure using the 		*
 * attributes of the input gfa element and the points in the input line.*
 *                                                                      *
 * static void af_fzlvl2fmt ( elIn, FAarea, npts, openContour, latPts,  *
 *                            lonPts, numFmt, fmt, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*elIn		VG_DBStruct	the gfa element			*
 *      *FAarea		char		FA area				*
 *	npts		int		# of points in the FZLVL line	*
 *	openLine	Boolean		open/closed contour flag	*
 *	*latPts		float		latitudes of points in the line	*
 *	*lonPts		float		longitudes of points in the line*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numFmt 	int		# of GFAs in the format struct	*
 *	*fmt		GFA_Elem_Format	gfa format structure		*
 *	*iret		int		normal: 0			*
 *                                                                      *
 * Return parameters:                                                   *
 *	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		  1/06	Created					*
 * J. Wu/SAIC         	 02/06   Add new point reduction algorithm      *
 * E. Safford/SAIC	 04/06  Add different point reduction algorithm *
 * D.W.Plummer/NCEP	 07/06	Add clo_snapPt to snap points		*
 * E. Safford/SAIC	 08/06	add openLine param,process closed fzlvl *
 * E. Safford/SAIC	 08/06	send map cords to cgr_reduceLinePoly    *  
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * D.W.Plummer/NCEP	09/06	Add tolerance parm to clo_snapPt	*
 * E. Safford/SAIC	09/06	use sys_M coords throughout		*
 * J. Wu/SAIC		10/06   change calling seq of cgr_reducepts	*
 * E. Safford/SAIC	10/06	set reduceFlgs on cgr_reducepts call	*
 * E. Safford/SAIC	11/06	use af_ptReduceOpenFzlvl()		*
 * E. Safford/SAIC	12/06	modify construction of reduceopt	*
 * E. Safford/SAIC	12/06	repeat point reduction until able to 	*
 *				 format on 3 lines			*
 * E. Safford/SAIC	01/07	change alg_choice to 4               	*
 * J. Wu/SAIC		01/07   use float input in cgr_reducepts	*
 * D.W.Plummer/NCEP     04/07   Add reduceFlg set to NULL               *
 * B. Yin/SAIC		03/07	remove the reduce points section	*
 * B. Yin/SAIC		04/07	use the input points as final points	*
 * B. Yin/SAIC		06/07	add the reduce points section		*
 * J. Wu/SAIC		10/07	Initialize origInfo & wording to NULL	*
 ***********************************************************************/
{
    int		ier, nblock, blockLen, maxpts;
    int		ii, nFinal;

    float	*xPts, *yPts, *latFinal, *lonFinal;
    float       slat, slon;
/*--------------------------------------------------------------------*/
 
    *iret  = 0;
    maxpts = MAXPTS;

    G_MALLOC ( xPts, float, npts, "AF_FZLVL2FMT xPts" );
    G_MALLOC ( yPts, float, npts, "AF_FZLVL2FMT yPts" );


    for( ii=0; ii<npts; ii++ ) {
        xPts[ii] = latPts[ii];
        yPts[ii] = lonPts[ii];
    }

    if( openLine ) {			/* process open contours */

        G_MALLOC ( latFinal, float, npts, "AF_FZLVL2FMT latFinal" );
        G_MALLOC ( lonFinal, float, npts, "AF_FZLVL2FMT lonFinal" );

        af_ptReduceOpenFzlvl( npts, xPts, yPts, &nFinal, 
				  latFinal, lonFinal, &ier );

    } 

    else {
        /* 
         *  Snap the final points -- point reduction may have moved some 
         *  or all points.
         */

         nFinal = npts;
         G_MALLOC ( latFinal, float, nFinal, "AF_FZLVL2FMT latFinal" );
         G_MALLOC ( lonFinal, float, nFinal, "AF_FZLVL2FMT lonFinal" );

    	 af_ptReduceClosedFzlvl ( npts, xPts, yPts, &nFinal, 
			     	  latFinal, lonFinal, &ier );

         for( ii=0; ii<nFinal; ii++ ) {
         	clo_snapPtGFA( ii, latFinal[ ii ], lonFinal[ ii ], ii, 0, NULL,
			NULL, nFinal, latFinal, lonFinal, False, True, 3.0F, 
			&slat, &slon, &ier );
		latFinal[ii] = slat;
		lonFinal[ii] = slon;

    	}
    }



    /*
     * Fill the fmt structure.
     */	
    fmt->delete = G_FALSE;
    strcpy ( fmt->area, FAarea );

    if ( ( strcasecmp ( FAarea, "SLC" ) == 0 ) || 
    	 ( strcasecmp ( FAarea, "SFO" ) == 0) ) {
	
       fmt->region = 'W';

    }
    else if ( ( strcasecmp ( FAarea, "DFW" ) == 0 ) || 
     	      ( strcasecmp ( FAarea, "CHI" ) == 0 ) ) {

       fmt->region = 'C';

    }
    else if ( ( strcasecmp ( FAarea, "BOS" ) == 0 ) || 
    	      ( strcasecmp ( FAarea, "MIA" ) == 0 ) ) {

       fmt->region = 'E';

    }

    fmt->adjarea[0] = '\0';


    /*
     * Copy the element
     */
    nblock = elIn->elem.gfa.info.nblocks;
    blockLen = nblock * STD_STRLEN * sizeof ( char );

    fmt->el.elem.gfa.info.nblocks = nblock;

    G_MALLOC ( fmt->el.elem.gfa.info.blockPtr[ 0 ],
   	       gfaBlock_t, nblock, "af_fzlvl2fmt: fmt.el.blockPtr" );

    memcpy ( &(fmt->el.hdr), &(elIn->hdr), sizeof( VG_HdrStruct ) );

    memcpy ( fmt->el.elem.gfa.info.blockPtr[ 0 ], 
     	     elIn->elem.gfa.info.blockPtr[ 0 ], blockLen );
					     		    	
    fmt->el.hdr.recsz = (int) ( sizeof( VG_HdrStruct ) 
	      	+ 2 * sizeof( int ) + blockLen
                + ( sizeof( float ) * (size_t)( 2 * nFinal )));	
		
    fmt->el.elem.gfa.info.npts = nFinal;

    memcpy ( fmt->el.elem.gfa.latlon, latFinal,
    		nFinal * sizeof ( float ) );
    memcpy ( &(fmt->el.elem.gfa.latlon[ nFinal ]), lonFinal,
    		nFinal * sizeof ( float ) );

    fmt->el_poly  = NULL;	
    fmt->openLine = openLine;
    fmt->fzlvlContour = True;


    fmt->el.hdr.closed = !openLine;
    
    /*
     *  Fill other fields in the format structure.
     */
    fmt->reduceFlg = (int *)NULL;
    fmt->origInfo = (GFA_SmrOlk_Grp *)NULL;
    fmt->wording = (char *)NULL;

    (*numFmt)++;

    G_FREE ( xPts, float );
    G_FREE ( yPts, float );

    G_FREE ( latFinal, float );
    G_FREE ( lonFinal, float );

}

/*=====================================================================*/

static void af_getFzlvlStartPt  ( float *latPts, float *lonPts, 
				int npts, Boolean *inout, 
				int *startPt, int *iret ) 
/************************************************************************
 * af_getFzlvlStartPt	                                                *
 *                                                                      *
 * This routine determines the proper starting point for a formatted    *
 * freezing level contour within an FA Area.  Normaly this starting     *
 * point is the first point that lies within the FA Area, however under *
 * certain specific circumstances, this is not the case.  		*
 *									*
 * The start/end points of a freezing level contour should be the same  *
 * as it crosses from one FA Area to another: the last point in the     *
 * first FA Area and the first point in the second FA Area should be    *
 * the same.  This routine checks the length of the contour within the  * 
 * FA Area and throws it out if that length is shorter than the table   *
 * driven _minFzlvlLen.  This will ensure that any initial short        *
 * segment(s) of the contour are handled by the first area, not both    *
 * areas, and thus the start/end points will match.			*
 *									*
 * The only exception to this is if the freezing level contour point    *
 * immediately before the first point inside the FA area actually lies  *
 * outside of the international bounds.  In this case the first point   *
 * in the FA area is always returned.					*
 *                                                                      *
 *                                                                      *
 * static void af_getFzlvlStartPt ( *latPts, *lonPts, npts, *inout, 	*
 *					*startPt, *iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*latPts		float	latitudes of points in the line		*
 *	*lonPts		float	longitudes of points in the line	*
 *	npts		int	# of points in the FZLVL line		*
 *									*
 * Input/Output:							*
 *	*inout		int	array of inside/outside flags for pts	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*startPt	int	starting lat/lon point			*
 *	*iret		int	return code:  normal = 0		*
 *                                no points in area  = 1		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	09/06	initial coding               		*
 ***********************************************************************/
{
    int		ii, lastPt, ier, newStart, np, nc;
    int		one = 1, inoutPt;
    float	length = 0.0, testLat, testLon, *xnormal, *ynormal;
    float	*latIntl, *lonIntl;

    Boolean	done;
    gpc_vertex_list	*cntr;
/*---------------------------------------------------------------------*/

    done     = False;
    *startPt = -1;
    lastPt   = -1;
    *iret    =  0;
    newStart =  0;

 
    while( !done ) {

        for( ii=newStart; ii<npts; ii++ ) {
	    if( *startPt < 0 ) {
                if( inout[ii] == True ) {
	            *startPt = ii;
		    break;
	        }
	    }
        }

        /*
         *  No points are in the area.  
         */
        if( *startPt < 0 ) {
	    done = True;
	    *iret = 1;
	    return;
        }


	/* 
	 *  Examine the point before the start point.  If that's outside of
	 *  the US, then do not worry about the length of any short segments.
	 *  They will be included for now, and screened out later if they don't 
	 *  connect with the other parts that may be inside the FA.
	 */
	if( *startPt > 1 ) {
	
	    testLat = latPts[ *startPt - 2 ];
	    testLon = lonPts[ *startPt - 2 ];

	    /*
	     *  Find the correct contour (not a hole).
	     */
	    nc = _intlBndPoly.num_contours;

	    cntr = NULL;
	    for( ii=0; ii<nc; ii++ ) {
		if( _intlBndPoly.hole[ii] != 1 ) {
		    cntr = &( _intlBndPoly.contour[ii] );
		    break;
		}
	    }
	    if( !cntr ) {
		done = True;
		break;
	    }

	    np = cntr[0].num_vertices + 1;

            G_MALLOC ( xnormal, float, np, "af_getFzlvlStartPt: xnormal" );
            G_MALLOC ( ynormal, float, np, "af_getFzlvlStartPt: ynormal" );

	    /*
	     *  Find out if the last point before this area is inside or
	     *  outside of the US bounds.  If it's outside then we're done,
	     *  because we can't adjust the start point.  If it's inside, then 
	     *  we need to continue and test the length of the segment(s) 
	     *  inside the area. 
	     */                   
            af_getPolyVerts ( cntr, &np, xnormal, ynormal, &ier ); 

	    xnormal[np] = xnormal[0];
	    ynormal[np] = ynormal[0];

            G_MALLOC ( latIntl, float, np, "af_getFzlvlStartPt: latIntl" );
            G_MALLOC ( lonIntl, float, np, "af_getFzlvlStartPt: lonIntl" );

	    gtrans ( sys_N, sys_M, &np, xnormal, ynormal, latIntl, 
	    	     lonIntl, &ier, strlen(sys_N), strlen(sys_M) );

	    G_FREE( xnormal, float );
	    G_FREE( ynormal, float );

            for( ii=0; ii<np; ii++ ) {
                latIntl[ii] = ( (float)( 
			( (int)( ROUNDUP( latIntl[ii] * 1000 ) ) ) ) ) / 1000.0;
                lonIntl[ii] = ( (float)( 
			( (int)( ROUNDUP( lonIntl[ii] * 1000 ) ) ) ) ) / 1000.0;
	    }


	    cgr_inpoly( sys_M, &one, &testLat, &testLon, 
	    		sys_M, &np, latIntl, lonIntl, &inoutPt, &ier ); 
            
	    G_FREE( latIntl, float );
	    G_FREE( lonIntl, float );

	    if( inoutPt == 0 ) {
		done = True;
	    }
	}


        /*
         *  Some points are in area, check for a short initial segment.
	 *  If it's too short we will skip over it (it will end up in 
	 *  the adjacent FA area) and we'll look for the next segment.
         */
        if( !done && *startPt >= 0 ) {
            for( ii=*startPt; ii<npts; ii++ ) {
                if( inout[ii] == True ) {
                    lastPt = ii;
	        }
	        else {
		    break;
	        }
            }

	    if( lastPt >= 0 ) {
                cgr_linelen ( &latPts[*startPt], &lonPts[*startPt], 
	    		lastPt - *startPt + 1, &length, &ier );

	        /*
	         *  if length > _minFzlvlLen then the intial segment is long
	         *  enough to qualify as a separate fzlvl contour.  If it's too
	         *  short, then advance to find the next segment that's long
	         *  enough.
	         */
                if ( length > _minFzlvlLen ) {
                    done = True;
                } 
	    }
        }

        if( !done ) {
	    if( ( lastPt + 1 ) < npts ) {
                newStart = lastPt + 1;
	    }
	    else {
		done = True;
	    }

	    *startPt = -1;
	    lastPt   = -1;

	}
    }

}

/*=====================================================================*/

static void af_ptReduceOpenFzlvl( int inPts, float inLat[], float inLon[], 
				  int *outPts, float outLat[], float outLon[], 
				  int *iret )

/************************************************************************
 * af_ptReduceOpenFzlvl                                                 *
 *                                                                      *
 * static void af_ptReduceOpenFzlvl( inPts, *inLat, *inLon, *outPts, 	*
 *				  *outLat, *outLon, *iret )		*
 *                                                                      *
 * Reduce the points in an open FZLVL contour to at least the 		*
 * FZLVL_REDUCE_THRESHOLD (in prefs.tbl).  Reduction below that value   *
 * will be done if the basic shape of the line can be preserved in the  * 
 * process.								*
 * 									*
 * This routine does no memory allocation.  The outLat[] and outLon[]   *
 * arrays must be allocated to at least inPts length by the calling     *
 * routine.  The actual number of point values returned is found in     *
 * outPts. 								* 
 *									*
 * The coordinate system for inLat[] and inLon[] must be sys_M.		*
 *									*
 *									*
 * Input parameters:                                                    *
 *	inPts		int	number of input lat/lon points		*
 *	inLat[]		float	latitudes of input points in the line	*
 *	inLon[]		float	longitudes of input points in the line	*
 *									*
 * Output parameters:                                                   *
 *	*outPts		int	number of output lat/lon points		*
 *	outLat[]	float	latitudes of output points in the line	*
 *	outLon[]	float	longitudes of output points in the line	*
 *	*iret		int	return code:  normal    = 0		*
 *					      inPts < 0 = -1		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/06	extracted from af_fzlvl2fmt, rm cv_rduc	*
 * B. Yin/SAIC		04/07	make sure xFinal/yFinal are big enough	*
 ***********************************************************************/
{
    int		ier, fzlvlThres, nptsTmp, npts;
    int		istrt, iend, ncv, maxpts, nrduc;

    int		*reduceFlg, *orig, ii, nFinal;
    
    float	*xFinal, *yFinal, *latFinal, *lonFinal;
    float	dens, crvscl, *xCv, *yCv, *xRduc, *yRduc;

    float	tolerance, slat, slon, *latTmp, *lonTmp;

    char	thresStr[ 5 ], reduceopt[ 128 ], tmpopt[ 32 ];
/*---------------------------------------------------------------------*/


    if( inPts <= 1 ) {
	*iret = -1;
	return;
    }

    *iret  = 0;
    maxpts = MAXPTS;
    npts   = inPts;

    G_MALLOC ( xFinal, float, npts, "AF_FZLVL2FMT xFinal" );
    G_MALLOC ( yFinal, float, npts, "AF_FZLVL2FMT yFinal" );


    /*
     *  Get the FZLVL_REDUCE_THRESHOLD value from the user's prefs.tbl.
     *	
     *  FZLVL_THRESH is the local default in case the uers's prefs.tbl doesn't
     *  have any value.
     */
    fzlvlThres = FZLVL_THRES;
    ctb_pfstr ( "FZLVL_REDUCE_THRESHOLD", thresStr, &ier );

    if ( ier >= 0 ) {
        fzlvlThres = atoi ( thresStr );
    }


    /*
     *  Reduce points for the FZLVL contour.  Point reduction must be at 
     *  least to the FZLVL_REDUCE_THRESHOLD (from prefs.tbl), and may be
     *  further reduced if the points lie along a sufficiently straight 
     *  line that some can be removed while keeping the basic shape.
     *
     *  Point reduction is performed in 5 steps:
     *    1. cv_prmt() takes the points and fits a parametric curve to them,
     *         which adds many additional points.
     *    2. cgr_reduceLinePoly() checks for points lying along a generally
     *         straight line and removes any unnecessary points. 
     *    3. cgr_reducePts() removes enough points to get the contour to the
     *         FZLVL_REDUCE_THRESHOLD.
     *    4. clo_snapPt() snaps the points to valid snap locations.
     *    5. cgr_reduceLinePoly() -- again to remove any extra points along a
     *	       generally straight line in the hope the the line might be 
     *         reduced below the FZLVL_REDUCE_THRESHOLD.
     */
    if ( npts > fzlvlThres ) {


        /*
         *  Step 1 -- fit parametric curve to points of FZLVL contour.
         */

        dens   = 5.0F;
        istrt  = 1;
        iend   = npts;

        gqcvsc( &crvscl, &ier );  /* get device curve scaling factor */
        if ( crvscl <= 0.0F )  crvscl = 25.0;

        G_MALLOC ( xCv, float, maxpts, "AF_FZLVL2FMT xCv" );
        G_MALLOC ( yCv, float, maxpts, "AF_FZLVL2FMT yCv" );

        cv_prmt( &npts, inLat, inLon, &dens, &maxpts, &crvscl, &istrt, &iend,
		&ncv, xCv, yCv, &ier );


 	/*
         *  Step 2 -- thin the points with cgr_reduceLinePoly.                    
	 */

	G_MALLOC( xRduc, float, maxpts, "AF_FZLVL2FMT xRduc" );
	G_MALLOC( yRduc, float, maxpts, "AF_FZLVL2FMT yRduc" );

        tolerance = FZLVL_TOL;   
        cgr_reduceLinePoly( ncv, xCv, yCv, tolerance, &nrduc, xRduc, yRduc, &ier );

        G_FREE ( xCv, float );
        G_FREE ( yCv, float );


        /*
         *  Step 3 -- reduce to maximum allowable number with cgr_reducePts.
         */

        G_MALLOC( reduceFlg, int, maxpts, "AF_FZLVL2FMT reduceFlg" );
        G_MALLOC( orig, int, maxpts, "AF_FZLVL2FMT reduceFlg" );

        for( ii=0; ii< maxpts; ii++ ) {
            reduceFlg[ii] = TRUE;
            orig[ii]      = 0;
        }
        reduceFlg[0] = FALSE;
        reduceFlg[nrduc-1] = FALSE;

        sprintf ( reduceopt, "<alg_choice>1</alg_choice>" );	  
        sprintf ( tmpopt, "<reduce_num>%d</reduce_num>", fzlvlThres );	  
        strcat ( reduceopt, tmpopt);

	/*
	 *  Make sure xFinal and yFinal are big enough.
	 */
	if ( nrduc > npts ) {

	   G_REALLOC( xFinal, float, nrduc, "AF_FZLVL2FMT xFinal" );
	   G_REALLOC( yFinal, float, nrduc, "AF_FZLVL2FMT yFinal" );

	}

        cgr_reducePts ( reduceopt, nrduc, xRduc, yRduc, reduceFlg,
	                &nptsTmp, xFinal, yFinal, orig, &ier );

        G_FREE ( reduceFlg, int );
        G_FREE ( orig, int );

        G_FREE ( xRduc, float );
        G_FREE ( yRduc, float );

    }
    else {

	nptsTmp = npts;
	memcpy ( xFinal, inLat, npts * sizeof( float ) );
	memcpy ( yFinal, inLon, npts * sizeof( float ) );
    }



    /*
     *  Step 4 -- snap all the remaining points.
     */

    G_MALLOC( latTmp, float, nptsTmp, "AF_FZLVL2FMT latTmp" );
    G_MALLOC( lonTmp, float, nptsTmp, "AF_FZLVL2FMT lonTmp" );

    for ( ii = 0; ii < nptsTmp; ii++ )  {
        clo_snapPtGFA( ii, xFinal[ ii ], yFinal[ ii ], ii, 0, NULL, NULL, nptsTmp, 
		    xFinal, yFinal, False, False, 3.0F, &slat, &slon, &ier );
        latTmp[ ii ] = slat;
        lonTmp[ ii ] = slon;
    }

    G_FREE ( xFinal, float );
    G_FREE ( yFinal, float );

    G_MALLOC( latFinal, float, nptsTmp, "AF_FZLVL2FMT latFinal" );
    G_MALLOC( lonFinal, float, nptsTmp, "AF_FZLVL2FMT lonFinal" );


    /*
     * Step 5 -- Apply final reduction to remove any redundant points.
     */

    tolerance = FZLVL_TOL;  
    cgr_reduceLinePoly( nptsTmp, latTmp, lonTmp, tolerance, &nFinal, 
            latFinal, lonFinal, &ier );

    G_FREE ( latTmp, float );
    G_FREE ( lonTmp, float );


    *outPts = nFinal;
    for( ii=0; ii<nFinal; ii++ ) {
        outLat[ii] = latFinal[ii];
        outLon[ii] = lonFinal[ii];
    }

    G_FREE ( latFinal, float );
    G_FREE ( lonFinal, float );

}
/*=====================================================================*/

static int af_getPointOrder( int inPts, float inLat[], float inLon[], 
				int *iret )

/************************************************************************
 * af_getPointOrder                                                     *
 *                                                                      *
 * static int af_getPointOrder( inPts, inLat[], inLon[], *iret )   	*
 *                                                                      *
 * Determine the order of the input points, returning either CW (clock-	*
 * wise) or CCW (counter clockwise).					*
 * 									*
 * The coordinate system for inLat[] and inLon[] is assumed to be sys_M.*
 *									*
 *									*
 * Input parameters:                                                    *
 *	inPts		int	number of input lat/lon points		*
 *	inLat[]		float	latitudes of input points in the line	*
 *	inLon[]		float	longitudes of input points in the line	*
 *									*
 * Output parameters:                                                   *
 *	*iret		int	return code:  0 = normal		*
 *					     -1 = inPts < 2 		*
 *					     -2 = err on centroid calc  * 
 *					     -3 = err on rol calc       * 
 * Return:								*
 *	int		CW (1) or CCW (-1) or error (0)			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding                         	*
 ***********************************************************************/
{
    float	xcent, ycent, xcentNorm, ycentNorm, area, tol = 0.0F;
    float	*xnormal, *ynormal;

    int		ier, rol, npts = 3, closed = G_TRUE, onePt = 1;
/*=====================================================================*/

    *iret = 0;

    if( inPts < 2 || !inLat || !inLon ) {
	*iret = -1;
	return( 0 );
    }


    /*
     *  Get the centroid then determine if it is left or right of the 
     *  line.  Left is CCW, right is CW.
     */
    cgr_centroid( inLat, inLon, &inPts, &xcent, &ycent, &area, &ier );
    if( ier < 0 ) {
        *iret = -2;
	return( 0 );
    }

    gtrans ( sys_M, sys_N, &onePt, &xcent, &ycent, &xcentNorm, &ycentNorm, &ier,  
		                     strlen(sys_M), strlen(sys_N) );

    npts = inPts; 

    G_MALLOC( xnormal, float, npts, "AF_GETPOINTORDER xnormal" );
    G_MALLOC( ynormal, float, npts, "AF_GETPOINTORDER ynormal" );

    gtrans ( sys_M, sys_N, &npts, inLat, inLon, xnormal, ynormal, &ier,  
		                     strlen(sys_M), strlen(sys_N) );

    cgr_qrol( &npts, xnormal, ynormal, &closed, &xcentNorm, &ycentNorm, 
    		&tol, &rol, &ier ); 
    if( ier < 0 ) {
	*iret = -3;
	return( 0 );
    } 

    G_FREE( xnormal, float );
    G_FREE( ynormal, float );

    return( rol );
}

/*=====================================================================*/

static Boolean af_inArea( int npts, Boolean inoutFlg[], int *iret )

/************************************************************************
 * af_inArea                                                            *
 *                                                                      *
 * static Boolean af_inArea( npts, inoutFlg[], iret )                  	*
 *                                                                      *
 * Check the input flag array and return True if any flags indicate     *
 * they are within the area (True).  This verifies that at least a      *
 * portion of the line lies within the FA area.				*
 * 									*
 * The coordinate system for inLat[] and inLon[] is assumed to be sys_M.*
 *									*
 *									*
 * Input parameters:                                                    *
 *	npts		int	number of input flag values   		*
 *	inoutFlg[]	Boolean	array of in/out flags			*
 *									*
 * Output parameters:                                                   *
 *	*iret		int	return code:  0 = normal		*
 *					     -1 = inoutFlg is NULL	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding                         	*
 ***********************************************************************/
{
    Boolean 	rc = G_FALSE;
    int		ii;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if( !inoutFlg ) {
	*iret = -1;
        return( rc );
    }

    for( ii=0; ii<npts; ii++ ) {
        if( inoutFlg[ii] ) { 
            rc = G_TRUE;
	    break;
        }
    }

    return( rc );

}

/*=====================================================================*/

static Boolean af_allInArea( int npts, Boolean inoutFlg[], int *iret )

/************************************************************************
 * af_allInArea                                                         *
 *                                                                      *
 * static Boolean af_allInArea( npts, inoutFlg[], iret )              	*
 *                                                                      *
 * Check the input flag array and return True if all flags indicate     *
 * they are within the area (True). 					*
 * 									*
 * Input parameters:                                                    *
 *	npts		int	number of input flag values   		*
 *	inoutFlg[]	Boolean	array of in/out flags			*
 *									*
 * Output parameters:                                                   *
 *	*iret		int	return code:  0 = normal		*
 *					     -1 = inoutFlg is NULL	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding                         	*
 ***********************************************************************/
{
    Boolean 	rc = G_TRUE;
    int		ii;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if( !inoutFlg ) {
	*iret = -1;
        return( rc );
    }

    for( ii=0; ii<npts; ii++ ) {
        if( !inoutFlg[ii] ) { 
            rc = G_FALSE;
	    break;
        }
    }

    return( rc );

}


/*=====================================================================*/

static void af_loadAreaPts( const gpc_polygon *bnds, int area, int contour, 
    		            float **lat, float **lon, int *npts, int *iret )
/************************************************************************
 * af_loadAreaPts                                                       *
 *                                                                      *
 * This routine gets loads the lat/lon coordinates for a given FA area  *
 * in the area *bnds structure.  Space is allocated for the lat/lon     *
 * points and must be freed by the calling routine.			*
 *                                                                      *
 * void af_loadAreaPts ( *bnds, area, contour, **lat, **lon, *npts,     *
 *			 *iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*bnds		gpc_polygon	array of FA Area bounds		*
 *	area		int		desired area to load		*
 *	contour		int		specified contour to load	*
 *                                                                      *
 * Output parameters:                                                   *
 *      **lat	        float           lat array of area points        *
 *      **lon	        float           lon array of area points        *
 *	*iret		int		return code 0: normal		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding          		*
 ***********************************************************************/

{
    float	*xPoly, *yPoly;
    int		kk, ier, numPts;
/*---------------------------------------------------------------------*/

    *npts = 0;
    *lat = NULL;
    *lon = NULL;

    if( bnds == NULL ) {
	*iret = -1;
	return;
    }


    /*
     *  Load points for FA Area
     */
    numPts = *npts = bnds[ area ].contour[ contour ].num_vertices + 1;

    G_MALLOC ( xPoly, float, *npts, "AF_LOADAREAPTS xPoly" );
    G_MALLOC ( yPoly, float, *npts, "AF_LOADAREAPTS yPoly" );

    G_MALLOC ( *lat, float, *npts, "AF_LOADAREAPTS lat" );
    G_MALLOC ( *lon, float, *npts, "AF_LOADAREAPTS lon" );


    /*
     *  Get all points in the contour and close it.
     */
    for ( kk = 0; kk < numPts-1; kk++ ) {

	xPoly[ kk ] = bnds[ area ].contour[ contour ].vertex[ kk ].x;
	yPoly[ kk ] = bnds[ area ].contour[ contour ].vertex[ kk ].y;
    }

    xPoly[ bnds[ area ].contour[ contour ].num_vertices ] = xPoly[ 0 ];
    yPoly[ bnds[ area ].contour[ contour ].num_vertices ] = yPoly[ 0 ];


    /*
     *  Put the polygon (FA Area) in map coordinates and round off
     *  to ensure the results are the same when using coordinates 
     *  normalized for widely differing projections.
     */
    gtrans ( sys_N, sys_M, &numPts, xPoly, yPoly, (*lat), 
   	     (*lon), &ier, strlen(sys_N), strlen(sys_M) );

    for( kk=0; kk<numPts; kk++ ) {
        (*lat)[kk] = ( (float)( 
		( (int)( ROUNDUP( (*lat)[kk] * 1000 ) ) ) ) ) / 1000.0;
	(*lon)[kk] = ( (float)( 
		( (int)( ROUNDUP( (*lon)[kk] * 1000 ) ) ) ) ) / 1000.0;
    }

    G_FREE ( xPoly, float ); 		
    G_FREE ( yPoly, float );

}

/*=====================================================================*/

static void af_revPointOrdr( int npts, float *xPts, float *yPts, int *iret )
/************************************************************************
 * af_revPointOrdr                                                      *
 *                                                                      *
 * This routine reversed the point order of the input xpt and ypt       *
 * arrays.                                                              *
 *                                                                      *
 * void af_revPointOrdr ( npts, *xPts, *yPts, *iret )                   *
 *                                                                      *
 * Input parameters:                                                    *
 *	npts		int		number of input points 		*
 *									*
 * Input/Output parameters:						*
 *	*xPts		float		array of x coordinates 		*
 *	*yPts		float		array of y coordinates    	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		return code          		*
 *						 0 = normal		*
 *						-1 = NULL xpt param	*
 *						-2 = NULL xpt param	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding          		*
 ***********************************************************************/
{
    float	*xtmp, *ytmp;
    int		ii, jj;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if( !xPts ) {
	*iret = -1;
	return;
    }

    if( !yPts ) {
	*iret = -2;
	return;
    }


    G_MALLOC( xtmp, float, npts, "af_fzlvl2fmt: xtmp" );
    G_MALLOC( ytmp, float, npts, "af_fzlvl2fmt: ytmp" );

    for( ii=0; ii<npts; ii++ ) {
	xtmp[ii] = xPts[ii];
	ytmp[ii] = yPts[ii];
    }

    for( ii=0, jj=npts-1; ii<npts; ii++, jj-- ) {
        xPts[ii] = xtmp[jj];
        yPts[ii] = ytmp[jj];
    }

    G_FREE( xtmp, float );
    G_FREE( ytmp, float );

}

/*=====================================================================*/

static void af_removeGaps( int npts, int startPt, float inLat[], float inLon[],
			  Boolean inputFlgs[], int *ptsInArea, 
			  float outLat[], float outLon[], 
			  Boolean outputFlgs[], int *iret )

/************************************************************************
 * af_removeGaps                                                        *
 *                                                                      *
 * static void af_removeGaps( npts, startPt, inLat[], inLon[], 		*
 *			inputFlgs[], *ptsInArea, outputFlgs[], *iret )  *
 *                                                                      *
 * Measure each portion of the line which is outside of an FA area.  If *
 * any outside portion is shorter than _maxFzlvlGap, switch the inout   *
 * flag(s) so that the gap is treated as inside.			*
 * 									*
 * The outLat/outLon arrays will contain the same points as inLat/inLon *
 * minus any beginning section that is outside of the area (and longer  *
 * than _maxFzlvlGap).							*
 *									*
 * The coordinate system for inLat[] and inLon[] is assumed to be sys_M.*
 *									*
 *									*
 * Input parameters:                                                    *
 *	npts		int	number of input lat/lon points		*
 *	startPt		int	starting point on line			*
 *	inLat[]		float	latitudes of input points in the line	*
 *	inLon[]		float	longitudes of input points in the line	*
 *	inputFlgs[]	Boolean	array of in/out flags			*
 *									*
 * Output parameters:                                                   *
 *	*ptsInArea	int	number points now considered "in" area  *
 *	latInArea[]	float	array of lat coordinates for pts in area*
 *	lonInArea[]	float	array of lon coordinates for pts in area*
 *	outputFlgs[]	Boolean	array of output in/out flags		*
 *	*iret		int	return code:  0 = normal		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/06	initial coding (moved from af_addFzlvl) *
 ***********************************************************************/
{

    int		localPts, ii, jj, kk, ier;
    float       length;

/*=====================================================================*/

    *iret      = 0;
    *ptsInArea = 0;
    localPts   = 0;


    if( startPt > npts-1 ) {
        *iret = -1;
	return;
    }


    for ( ii = startPt; ii < npts; ii++ ) {

        if ( ( inputFlgs[ ii ] == 1 ) || ( ii == 0 ) ||
             ( inputFlgs[ ii - 1 ] == 0 ) ) {  
						/* in the area or first seg */
           outLat[ localPts ]     = inLat[ ii ];
           outLon[ localPts ]     = inLon[ ii ];
           outputFlgs[ localPts ] = inputFlgs[ ii ];
           localPts++;

        }
        else {                                    /* out of the area */

           for ( jj = ii; jj < npts; jj++ ) {
               if ( inputFlgs[ jj ] == 1 ) break;
           }

           if ( jj == npts ) {                 /* rest of the line is outside */

              for ( kk = ii; kk < npts; kk++ ) {
                  outLat[ localPts ]     = inLat[ kk ];
                  outLon[ localPts ]     = inLon[ kk ];
                  outputFlgs[ localPts ] = inputFlgs[ kk ];
                  localPts++;
              }
              break;

           }
           else {

              cgr_linelen ( (float*)&(inLat[ ii ]), (float*)&(inLon[ ii ]),
                            jj - ii, &length, &ier );

              if ( length < _maxFzlvlGap ) {		/* short gap found */

                 for ( kk = ii; kk < jj - 1; kk++ ) {	/* set outputFlgs True */
                     outLat[ localPts ]     = inLat[ kk + 1 ];
                     outLon[ localPts ]     = inLon[ kk + 1 ];
                     outputFlgs[ localPts ] = G_TRUE;
                     localPts++;
                 }

                 ii = jj;

              }

              else {				/* copy inputFlgs to outputFlgs */

                 for ( kk = ii; kk < jj; kk++ ) {
                     outLat[ localPts ]     = inLat[ kk ];
                     outLon[ localPts ]     = inLon[ kk ];
                     outputFlgs[ localPts ] = inputFlgs[ kk ];
                     localPts++;
                 }

                 ii = jj - 1;

              }

           }

        }

    }

    *ptsInArea = localPts;
}

static void af_ptReduceClosedFzlvl( int inPts, float inLat[], float inLon[],
					int *outPts, float outLat[], float outLon[],
					int *iret )
/************************************************************************
 * af_ptReduceColsedFzlvl                                               *
 *                                                                      *
 * This routine reduces points for closed fzlvl.			*
 *                                                                      *
 * static void af_ptReduceClosedFzlvl ( inPts, inLat, inLon, outPts, 	*
 *				  	outLat, outLon, iret )   	*
 *                                                                      *
 * Input parameters:                                                    *
 *	inPts		int		number of input points		*
 *      inLat[]		float		latitudes of input points	*
 *      inLon[]		float		longitudes of input points	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*outPts		int		number of output points		*
 *      outLat[]	float		latitudes of output points	*
 *      outLon[]	float		longitudes of output points	*
 *	*iret		int		return code. 			*
 *					 0 : normal			*
 *					 1 : no reduction needed        *
 *					-1 : unable to reduce points    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 3/07	Moved from af_fzlvl2fmt			*
 * B. Yin/SAIC		 4/07	call cgr_canBeFormatted after re-order	*
 ***********************************************************************/
{

    int		ier, ier1, tmpPts, maxpts, npts;
    int		*reduceFlg = NULL, ii, *origPt, rol;

    float	reducePct, reduceDst, tval;
    float       *xtmp, *ytmp;

    char	cval[ 32 ], reduceopt[ 256 ], tmpopt[ 64 ];
    Boolean	done, reordrPts = G_FALSE;
/*--------------------------------------------------------------------*/

    *iret = 0;
    maxpts = MAXPTS;
    npts  = inPts;

    /* 
     *  Determine the point order of the line and reorder 
     *  (flip) the points if it's CCW.
     */
    reordrPts = G_FALSE;
    rol = af_getPointOrder( inPts, inLat, inLon, &ier );

    if ( rol == CCW_LINE ) {

       reordrPts = G_TRUE;
       af_revPointOrdr( inPts, inLat, inLon, &ier );

    }

    /*
     * Retrieve the maximum size increase and distance allowed.
     */        
    reducePct = REDUCE_INCR_PCT;
    ctb_pfstr ( "SMEAR_INCR_PCT", cval, &ier );

    if ( ier >= 0 ) {

       cst_crnm ( cval, &tval, &ier );

       if ( ier == 0 ) {

           reducePct = tval;

       }

    }

    reduceDst = REDUCE_INCR_DST;
    ctb_pfstr ( "SMEAR_INCR_DST", cval, &ier );

    if ( ier >= 0 ) {

       cst_crnm ( cval, &tval, &ier );

       if ( ier == 0 ) {

                reduceDst = tval;

       }

    }
   
    G_MALLOC( xtmp, float, inPts, "af_fzlvl2fmt: xtmp" );
    G_MALLOC( ytmp, float, inPts, "af_fzlvl2fmt: ytmp" );
    G_MALLOC( origPt, int, inPts, "af_fzlvl2fmt: origPt" );
    G_MALLOC( reduceFlg, int, maxpts, "AF_FZLVL2FMT reduceFlg" );

    done = G_FALSE;

    while ( !done ) {

	/*
	 *  Rebuild the repuceopt string each time in case we've
	 *  had to increase the reducePct beyond the prefs.tbl value.
	 */	
        sprintf ( reduceopt, "<alg_choice>4</alg_choice>" );	  
        sprintf ( tmpopt, "<incr_pct>%10.2f</incr_pct>", reducePct );	 
        strcat ( reduceopt, tmpopt );

        sprintf ( tmpopt, "<incr_dst>%10.2f</incr_dst>", reduceDst );	  
        strcat ( reduceopt, tmpopt );

	sprintf(  tmpopt, "<coord_sys>%s</coord_sys>", sys_M );
        strcat ( reduceopt, tmpopt );

        strcat ( reduceopt, "<format_prefix>CLOSED FZLVL</format_prefix>" );    

        for ( ii=0; ii < npts; ii++ ) {

	    reduceFlg[ ii ] = G_TRUE;
	    origPt[ ii ]    = 0;

	}  

        reduceFlg[ 0 ] = reduceFlg[ npts - 1 ] = G_FALSE; 

	cgr_reducePts ( reduceopt, npts, inLat, inLon, reduceFlg, 
			&tmpPts, xtmp, ytmp, origPt, &ier );
    
	/*
	 *  If ier from cgr_reducePts == 2 then the reduction 
	 *  failed to format on 3 lines.  Increment the reducePct
	 *  and re-run the point reduction.  As a sanity check, make
	 *  done True if we are ever over MAX_REDUCE_PCT, and return
	 *  the starting points.
	 */ 
	if ( ( ier == 2 && reducePct < MAX_REDUCE_PCT ) ) {

            for ( ii=0; ii < tmpPts; ii++ ) {

		    inLat[ ii ]   = xtmp[ ii ];
		    inLon[ ii ]   = ytmp[ ii ];

	    }

	    for( ii = 0; ii < npts; ii++ ) {

		    xtmp[ ii ] = 0;
		    ytmp[ ii ] = 0;

	    }

	    npts = tmpPts;
	    reducePct++;

        }
	else {

	    done = G_TRUE;

	    if ( ier == 1 ) *iret = 1;

	    /*
	     *  If we've quit because we can't format this thing due to
	     *  insufficient room to expand then give up and use the
	     *  original points.  
	     */
	    if ( reducePct > MAX_REDUCE_PCT )  {

               for ( ii = 0; ii < inPts; ii++ ) {
		
                   xtmp[ ii ] = inLat[ii];
                   ytmp[ ii ] = inLon[ii];

               }

	       tmpPts = inPts;
	       *iret = -1;

	    }

	}


    }

    for ( ii = 0; ii < tmpPts; ii++ ) {

        outLat[ ii ] = xtmp[ ii ];
        outLon[ ii ] = ytmp[ ii ];

    }

    *outPts = tmpPts; 

    if( reordrPts ) {

	af_revPointOrdr( *outPts, outLat, outLon, &ier1 );

    }

    /*
     *  Since cgr_reducePts requires a closkwise contour while FZLVLs
     *  can be counter clockwise, the resulting from line might be more 
     *  than 3 lines after point reduction. The following code is to 
     *  remove one more point so that the from lines are less than 3.
     */
    if ( ier >= 0 ) {
		
       if ( !cgr_canBeFormatted ( *outPts, outLat, outLon, "CLOSED FZLVL" ) ) {
		 
	  memmove( &outLat[ 1 ], &outLat[ 2 ], (*outPts - 1) * sizeof( float ) );
	  memmove( &outLon[ 1 ], &outLon[ 2 ], (*outPts - 1) * sizeof( float ) );
	  (*outPts)--;

	}

    }

    G_FREE( xtmp, float );
    G_FREE( ytmp, float );
    G_FREE( origPt, int );
    G_FREE( reduceFlg, int );

}
