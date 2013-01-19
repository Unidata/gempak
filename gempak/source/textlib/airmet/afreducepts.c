#include "afcmn.h"

#define SAME_LATLON(lt1,ln1,lt2,ln2,sllt)	( G_DIFFT ( lt1, lt2, sllt ) && G_DIFFT ( ln1, ln2, sllt ) )

#define		TOL		0.001

/*
 * Structure to hold bisection distances for sorting
 */
typedef struct distInfo_t
{
	float	dist;
	int	indx1;
	int	indx2;
} DInfo_t;

/*
 * Structure to hold bisection midpoint information
 */
typedef struct midpInfo_t
{
	float	midp_lat;
	float	midp_lon;
	int	status;
} MInfo_t;
#define		UNUSED	0
#define		USED	1

/*
 * Bisection midpoint information
 */
int		_n_midP = 0;
MInfo_t		*_midP = (MInfo_t*)NULL;

/* Private qsort function for sorting bisection distances */
static int	_afsdist_sort ( DInfo_t *d1, DInfo_t *d2 );

/************************************************************************
 * afreducepts.c                                             		*
 *                                                                      *
 * This module contains routines used for point reduction algorithm. 	*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *      af_reducePts		- reduces number of polygon points	*
 *                                                                      *
 *   private functions:                                                 *
 *	_reducePtsUpdateFmt	- updates fmt after point reduction	*
 *	_reducePtsClipFAArea	- clips fmt structure against area bound*
 *	_reducePtsBisectSimple	- bisectss a fmt struct into two	*
 *	_reducePtsSegIntPoly	- check if a segment intersect a polygon*
 *	_reducePtsFindSegment	- find the shortest segment that does 	*
 *      			  does not intersect the polygon	*
 *	_reducePtsGetInt	- find intersection points with extended*
 *      			  area bounds				*
 ***********************************************************************/

/*
 *  Private functions
 */
static void _reducePtsRemoveUnusedMidPts ( int nin, 
	                            GFA_Elem_Format **fmt_in );

static void _reducePtsUpdateFmt ( int npts, float *xin, float *yin, 
           	int *origPt, GFA_Elem_Format *fmt_in, int *iret );

static void _reducePtsClipFAArea ( int index, int *nin, 
		GFA_Elem_Format **fmt_in,  int *iret );

static void _reducePtsBisectSimple ( int index, int round, int *nin, 
		GFA_Elem_Format **fmt_in,  int *iret );
 
static Boolean _reducePtsSegIntPoly ( int npts, float *polyLat, 
		float *polyLon, float firstLat, float firstLon, 
		float secondLat, float secondLon );

static void _reducePtsFindSegment ( int index, int slack, 
		int npts, float *polyLat, float *polyLon, 
		int *endIndex, float *length, int *iret );

static void _reduceptsGetInt ( VG_DBStruct el, int *nip,  float *ipx, 
		float *ipy, float *sipx, float *sipy, int *iret );

/*=====================================================================*/

void af_reducePts ( int *nin, GFA_Elem_Format **fmt_in, int *iret )
/************************************************************************
 * af_reducePts                                                     	*
 *                                                                      *
 * This routine reduces the number of points in each format structure	*
 * to allow it to be represented on three 65-character lines of text.	*
 * 									*
 * The point reduction goes through two phases:				*
 * 									*
 * Phase I - It first tries to remove allowable points, one at a time, 	*
 * based on the impact their individule removal would have on the 	*
 * size of the polygon.  Specifically, remove points that increase	*
 * the size of the polygon the least,  while not increasing the overall	*
 * size of the polygon SMEAR_INCREASE_PCT and not allowing any new 	*
 * points to be SMEAR_INCR_DST distance from the original polygon 	*
 * points. The parameters SMEAR_INCREASE_PCT and SMEAR_INCR_DST are set *
 * in prefs.tbl. SMEAR_INCREASE_PCT refers to the areal percentage 	*
 * increase when a single point is removed from the polygon. Point	*
 * reduction continues until the polygon can be represented on three 	*
 * 65-character lines of text, or no more points can be removed under 	*
 * the above criteria.							*
 *                                                                      *
 * Phase II - If the polygon still cannot be formatted on three 	*
 * 65-character lines, logically divide it into two or more sections:	*
 * 									*
 * 1. FA Area Boundary Clip: First, try to divide the polygon along the	*
 * FA Area boundary. If this can be accomplished, check the overall size*
 * of each part. Any part less than 3K sq nm will remain with the larger*
 * portion; any part greater than 3K sq nm will become another AIRMET.	*
 *									*
 * Note that the areal clipping behaves the same as the regional	*
 * clipping and we should make intact the boundary between two FA areas,*
 * which means if a polygon is clipped into two sections in two areas, 	*
 * they must share the exact same points along the boundary.		*
 *									*
 * 2. Simple Bisection: Check the results from the FA Area Boundary 	*
 * Bisection. Any section that cannot be formatted in three 65-character*
 * lines of text must be divided. Choose as a dividing segment the two	*
 * closest opposite points in the polygon array which also does not 	*
 * intersect any part of the polygon. Determine the midpoint of this 	*
 * segment and snap to the closest snap point. Bisect the polygon using	*
 * this three-point segment into two new AIRMET polygons. Again, check	*
 * to make sure these two polygons can be formatted. If not, re-divide	*
 * again except without a midpoint. Two bisections should be sufficient	*
 * to format correctly. If not, the user must break it up manually	*
 * (training issue).							*	
 *                                                                      *
 * Note: "fmt_in" may be reallocated in this routine and must be freed	*
 *	  by the caller.                                               	*
 *                                                                      *
 * void af_reducePts ( nin, fmt_in, iret )				*
 *                                                                      *
 * Input parameters:                                             	*
 *      *nin		int     	number of GFA format structures	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      **fmt_in	GFA_Elem_Format Array of GFA format structures	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          08/05   	Created                         	*
 * B. Yin/SAIC	       01/06	skip if GFA is an open Line		*
 * J. Wu/SAIC          02/06  	snap the reduced GFA points		*
 * J. Wu/SAIC          02/06  	free origPt				*
 * D.W.Plummer/NCEP	4/06	Chg calling seq of clo_snapPt		*
 * E. Safford/SAIC	8/06	modify check for fzlvl contours		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * D.W.Plummer/NCEP	09/06	Add tolerance parm to clo_snapPt	*
 * J. Wu/SAIC		10/06   overhaul with the new algorithm		*
 * E. Safford/SAIC	10/06	fix bug with reduceopt overrun		*
 * J. Wu/SAIC		10/06   add areal clipping & simple bisection	*
 * J. Wu/SAIC		11/06   add opitons to turn on/off the areal 	*
 *			        clipping & set the number of rounds for *
 *			        simple bisection			*
 * J. Wu/SAIC		12/06   do areal clipping using the original	* 
 *			        polygon if it still cannot be formatted	*
 *				after the regular point reduction	*
 * J. Wu/SAIC		01/07   use float input for cgr_reducePts	*
 * D.W.Plummer/NCEP	03/07	add prefs opt REDUCEPTS_INCR_PCT_ORIG	*
 * J. Wu/SAIC           03/07   load preference from af_utils*          *
 * D.W.Plummer/NCEP	03/07	add pt reduction after FA & bi splits	*
 * J. Wu/SAIC           05/07   restore debugging on areal clipping & 	*
 *				bisection				*
 ***********************************************************************/
{
    int 		ii, jj, ier, np, tmpPts, *origPt, nout;
    int			subType, ier1, ival, roundIn;
    int			ntmp, cbf, round, rounds, areaClip, *reduceFlg;
    float		*xtmp, *ytmp, *xd, *yd;
    char		cval[32], reduceopt[256], loopopt[256];
    char		tmpopt[64], prefix[32];
    Boolean		bisect;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
    
    /*
     * Retrieve the settings for areal clipping and bisection.
     */
    areaClip = G_TRUE;
    ctb_pfstr ( "GFA_AF_AREACLIP", cval, &ier );
    if ( ier == 0 && strcasecmp ( cval, "FALSE" ) == 0 ) {
        areaClip = G_FALSE;
    }

    rounds = 2;
    ctb_pfstr ( "GFA_AF_ROUNDS", cval, &ier );
    if ( ier >= 0 ) {
        cst_numb ( cval, &ival, &ier );
        if ( ier == 0 ) {
            if ( ival >= 0 ) {
	        rounds = ival;
            }
	    else { 
	        rounds = 0; 
	    }
	}
    }
        

    /*
     * Put the maximum size increase and distance allowed into a string.
     */
    sprintf ( loopopt, "<alg_choice>3</alg_choice>" );	  
    sprintf ( tmpopt, "<incr_pct>%10.2f</incr_pct>", _reducePct );	  
    strcat ( loopopt, tmpopt);
    sprintf ( tmpopt, "<incr_pct_orig>%10.2f</incr_pct_orig>", _reducePctOrig );	  
    strcat ( loopopt, tmpopt);
    sprintf ( tmpopt, "<incr_dst>%10.2f</incr_dst>", _reduceDst );	  
    strcat ( loopopt, tmpopt);
    

    /*
     * Reduce points based desired limitations.  Note that freezing level 
     * contours are point reduced in af_fzlvl2fmt().
     */    
    nout = *nin;
    ntmp = 0;
    bisect = False;
    for ( round = 0; round <= rounds; round++ ) {

      for ( ii = ntmp; ii < *nin; ii++ ) {

	if( (*fmt_in)[ii].fzlvlContour ) continue;
	if( (*fmt_in)[ii].delete == G_TRUE ) continue;

        np = (*fmt_in)[ii].el.elem.gfa.info.npts;
        
        G_MALLOC ( xd, float, np, "af_reducePts: xd" );
        G_MALLOC ( yd, float, np, "af_reducePts: yd" );
        G_MALLOC ( xtmp, float, np, "af_reducePts: xtmp" );
        G_MALLOC ( ytmp, float, np, "af_reducePts: ytmp" );
        G_MALLOC ( origPt, int, np, "af_reducePts: origPt" );

	gtrans ( sys_M, sys_D, &np, 
	         (*fmt_in)[ii].el.elem.gfa.latlon, 
		 &((*fmt_in)[ii].el.elem.gfa.latlon[np]), xd, yd,
                 &ier, strlen(sys_M), strlen(sys_D) );
	
	cvg_getFld ( &((*fmt_in)[ii].el), TAG_GFA_SUBTYPE, tmpopt, &ier );

        subType = atoi( tmpopt ) - atoi( tmpopt ) / 10 * 10;
	
	strcpy( reduceopt, loopopt );
	prefix[ 0 ] = '\0';
	if ( subType == GFA_USER_SMEAR || subType == GFA_SYSTEM_SMEAR ) {            
	    strcat ( reduceopt, "<format_prefix>FROM</format_prefix>" );
	    strcpy ( prefix, "FROM" );
	}
	else if ( subType == GFA_USER_OUTLOOK || 
	          subType == GFA_SYSTEM_OUTLOOK ) {		  
            strcat ( reduceopt, "<format_prefix>BOUNDED BY</format_prefix>");
	    strcpy ( prefix, "BOUNDED BY" );
	}	  

        G_MALLOC ( reduceFlg, int, np, "Error alloc reduceFlg" );
	for ( jj = 0; jj < np; jj++ )  {
	    reduceFlg[jj] = (*fmt_in)[ii].reduceFlg[jj];
	}
	cgr_reducePts ( reduceopt, np, xd, yd, reduceFlg, 
			&tmpPts, xtmp, ytmp, origPt, &ier );
	
	/*
         *  Add the new contour to the GFA format structure if it has been 
	 *  point-reduced.  If it still cannot be formatted in 3 lines of text,
	 *  do areal clipping against its primary and adjacent areas.
         */        
	cbf = G_FALSE;
	if ( ier == 0 ) {
	    cbf = G_TRUE; 	    
	    for ( jj = 0; jj < tmpPts; jj++ )  {
	        (*fmt_in)[ii].reduceFlg[jj] = reduceFlg[jj];
            }
	    _reducePtsUpdateFmt( tmpPts, xtmp, ytmp, origPt, &(*fmt_in)[ii], &ier1 );	
	}
	else {	    
	    if ( ier != 2 ) {
	        cbf = cgr_canBeFormatted ( np, (*fmt_in)[ii].el.elem.gfa.latlon, 
	               &((*fmt_in)[ii].el.elem.gfa.latlon[np]), prefix );	
	    }
	}    
	
	if ( cbf == G_FALSE ) {
	    if ( round == 0 && areaClip == G_TRUE )  {
		_reducePtsClipFAArea ( ii, &nout, fmt_in,  &ier );	    
	    }
	    else  {
	        for ( jj = 0; jj < tmpPts; jj++ )  {
	            (*fmt_in)[ii].reduceFlg[jj] = reduceFlg[jj];
                }
	        _reducePtsUpdateFmt( tmpPts, xtmp, ytmp, origPt, &(*fmt_in)[ii], &ier1 );	
	        
		
		/*
		  *   Only do bisection if required and roundIn should be either 1 or 2.
		  */
		if ( rounds > 0 ) {
		    roundIn = round;
		    if ( areaClip != G_TRUE ) {
		        roundIn = round + 1;                  
		    }
		     
		    if ( roundIn <= rounds ) {
		        _reducePtsBisectSimple( ii, roundIn, &nout, fmt_in, &ier );	
		        bisect = True;
	            }
		}
	    }
	}	

        G_FREE ( reduceFlg, int );
		        
        G_FREE ( origPt, int );
	G_FREE ( xd, float );
        G_FREE ( yd, float );
        G_FREE ( xtmp, float );
        G_FREE ( ytmp, float );
	
      }

      ntmp = *nin;
      *nin = nout;

    }
    

    /*
     * If bisection happens, search all non-deleted structures for 
     * unused inserted midpoints.
     */
    if ( bisect )  {
        _reducePtsRemoveUnusedMidPts ( *nin, fmt_in );
    }
     
    
    /*
     *  Update the total number of format structures.
     */
    *nin = nout;

    G_FREE ( _midP, MInfo_t );
    _n_midP = 0;
       
}

/*=====================================================================*/

static void _reducePtsRemoveUnusedMidPts ( int nin, 
	                            GFA_Elem_Format **fmt_in )
/************************************************************************
 * _reducePtsRemoveUnusedMidPts                                         *
 *                                                                      *
 * This routine removes unused midpoints from any undeleted polygons.	*
 * The removal is done 'in place', no new structure is created.		*
 *                                                                      *
 * static void _reducePtsRemoveUnusedMidPts ( nin, fmt_in )		*
 *                                                                      *
 * Input parameters:                                             	*
 *      nin		int     	number of input structures	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      fmt_in		GFA_Elem_Format Format struct to be updated	*
 *                                                                      *
 * Output parameters:                                             	*
 * 					0 - normal			*
 * 									*
 * D.W.Plummer/NCEP	03/07	Created					*
 ***********************************************************************/
{
int	ii, jj, kk, npts, nbytes, found;
float	lat, lon;
/*--------------------------------------------------------------------*/

    if ( _n_midP == 0 )  return;

    for ( ii = 0; ii < nin; ii++ )  {
	if ( (*fmt_in)[ii].delete == G_FALSE )  {
	    npts = (*fmt_in)[ii].el.elem.gfa.info.npts;
	    found = G_FALSE;
	    for ( jj = 0; jj < npts && found==G_FALSE; jj++ )  {
		lat = (*fmt_in)[ii].el.elem.gfa.latlon[jj     ];
		lon = (*fmt_in)[ii].el.elem.gfa.latlon[jj+npts];
		for ( kk = 0; kk < _n_midP && found==G_FALSE; kk++ )  {
	            if ( _midP[kk].status == UNUSED  &&
			 G_DIFFT(lat,_midP[kk].midp_lat,TOL) && 
			 G_DIFFT(lon,_midP[kk].midp_lon,TOL) ) {

			/* Move the rest of the lats */
			nbytes = ( npts - jj - 1 ) * sizeof(float);
			memmove ( &((*fmt_in)[ii].el.elem.gfa.latlon[jj  ]),
				  &((*fmt_in)[ii].el.elem.gfa.latlon[jj+1]), nbytes );

			/* Move the forst part of the lons */
			nbytes = ( jj ) * sizeof(float);
			memmove ( &((*fmt_in)[ii].el.elem.gfa.latlon[npts-1]),
				  &((*fmt_in)[ii].el.elem.gfa.latlon[npts]), nbytes );

			/* Move the rest of the lons */
			nbytes = ( npts - jj - 1 ) * sizeof(float);
			memmove ( &((*fmt_in)[ii].el.elem.gfa.latlon[npts+jj-1]),
				  &((*fmt_in)[ii].el.elem.gfa.latlon[npts+jj+1]), nbytes );

			/* Move the rest of the reduceFlg */
			nbytes = ( npts - jj - 1 ) * sizeof(float);
			memmove ( &((*fmt_in)[ii].reduceFlg[jj  ]),
				  &((*fmt_in)[ii].reduceFlg[jj+1]), nbytes );

			/* Decrement the number of points */
			(*fmt_in)[ii].el.elem.gfa.info.npts -= 1;

			found = G_TRUE;

		    }
		}
	    }
	}
    }

}

/*=====================================================================*/

static void _reducePtsUpdateFmt ( int npts, float *xin, float *yin, 
                    int *orig, GFA_Elem_Format *fmt_in, int *iret )
/************************************************************************
 * _reducePtsUpdateFmt                                                  *
 *                                                                      *
 * This routine loads the input points into the polygon and the element *
 * in the format structure and updates related information as well.	*
 *                                                                      *
 * static void _reducePtsUpdateFmt ( npts, xin, yin, orig, fmt_in, iret)*
 *                                                                      *
 * Input parameters:                                             	*
 *      npts		int     	number of new polygon points	*
 *      *xin		float     	x-coords of new polygon point	*
 *      *yin		float     	y-coords of new polygon point	*
 *      *orig		int     	if a pt is an original point	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *fmt_in		GFA_Elem_Format Format struct to be updated	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *iret           int             Return code                     *
 * 					0 - normal			*
 * 									*
 * J. Wu/SAIC          10/06   	Created                         	*
 ***********************************************************************/
{
    int 		jj, ier, nblock, jj2;
    float		*xnormal, *ynormal;
    float		*elat, *elon, *slat, *slon;
    
    gpc_vertex_list	verts;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;
              
    
    /*
     *  Allocate working space
     */	
    G_MALLOC ( elat, float, npts + 1, "af_updateFmt: elat" );
    G_MALLOC ( elon, float, npts + 1, "af_updateFmt: elon" );
    G_MALLOC ( slat, float, npts, "af_updateFmt: slat" );
    G_MALLOC ( slon, float, npts, "af_updateFmt: slon" );
    G_MALLOC ( xnormal, float, npts, "af_updateFmt: xnormal" );
    G_MALLOC ( ynormal, float, npts, "af_updateFmt: ynormal" );
	    

    /*
     *  Convert input points into map coordinate and snap
     */	
    gtrans ( sys_D, sys_M, &npts, xin, yin, 
	     elat, elon, &ier, strlen(sys_D), strlen(sys_M) );

    for ( jj = 0; jj < npts; jj++ ) {
        if ( orig != NULL && orig[jj] == G_FALSE && 
	     fmt_in->reduceFlg != (int *)NULL && 
	     fmt_in->reduceFlg[jj] == G_TRUE ) {		
		    
	     jj2 = jj;
	     clo_snapPt( jj, elat[ jj ], elon[ jj ], jj2, npts, 
			 elat, elon, True, 3.0F, 
			 &slat[ jj ], &slon[jj], &ier );		    
	}
        else {
	    slat[ jj ] = elat[ jj ];
	    slon[ jj ] = elon[ jj ];		
	}
    }
           
    
    /*
     *   Update points in the GFA element.
     */ 	    
    fmt_in->el.elem.gfa.info.npts = npts;
    for ( jj = 0; jj < npts; jj++ ) {
        fmt_in->el.elem.gfa.latlon[ jj        ] = slat[ jj ];
        fmt_in->el.elem.gfa.latlon[ jj + npts ] = slon[ jj ];
    }
            	    
    nblock = fmt_in->el.elem.gfa.info.nblocks;
    fmt_in->el.hdr.recsz = (int) ( sizeof( VG_HdrStruct ) 
		+ 2 * sizeof( int ) + 
		+ nblock * STD_STRLEN * sizeof ( char ) +
                + ( sizeof( float ) * (size_t)( 2 * npts )));	
        

    /*
     *   Update points in the format structure's polygon.
     */ 	    
    gtrans ( sys_M, sys_N, &npts, slat, slon, xnormal, ynormal, 
             &ier, strlen(sys_D), strlen(sys_N) );

    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;	    
    gpc_cvlist ( npts, xnormal, ynormal, &verts, &ier );

    free ( fmt_in->el_poly->contour[0].vertex ); 
    
    fmt_in->el_poly->num_contours = 0;
    
    gpc_add_contour ( fmt_in->el_poly, &verts, G_FALSE );
    
    free ( verts.vertex ); 
 	    
    G_FREE ( elat, float );
    G_FREE ( elon, float );
    G_FREE ( slat, float );
    G_FREE ( slon, float );
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );
    
}

/*=====================================================================*/

static void _reducePtsClipFAArea ( int index, int *nin,  
			GFA_Elem_Format **fmt_in, int *iret )
/************************************************************************
 * _reducePtsClipFAArea                                            	*
 *                                                                      *
 * This routine clips the polygon in a format structure against its area*
 * and adjacent area if the points on the polygon cannot be represented	*
 * on three 65-character lines of text.  The original format structure	*
 * will be flagged as "deleted".  Each clipped part with an area > 3K 	*
 * sqnm	produces a new format structure at the end of the format array.	*
 * The total number of newly-added format structures is returned in 	*
 * "newfmt".  It is the caller's responsiblity to check	and free those	*
 * additional memory.							*
 *                                                                      *
 * static void _reducePtsClipFAArea ( index, nin, fmt_in, iret )	*
 *                                                                      *
 * Input parameters:                                             	*
 *      index		int     	index of the fmt struct to be	*
 *                                      clipped				*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *nin		int     	total # of fmt structs		*
 *      **fmt_in	GFA_Elem_Format Array of GFA format structures	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/06   initial coding				*
 * J. Wu/SAIC		10/06   turn off areal clipping temporarily 	*
 * J. Wu/SAIC		11/06   turn on areal clipping 			*
 * J. Wu/SAIC		11/06   fix a bug that processes a fmt structure*
 *			        twice to generate duplicate fmt structs	*
 * J. Wu/SAIC		11/06   fix a bug that generates AIRMET outside	*
 *				of the given FA area 			*
 * J. Wu/SAIC           12/06   create a fmt struct only if it intesects*
 *				a snapshot with an area >= 3K sq nm	*
 * J. Wu/SAIC		01/07   clip against extended FA area bounds and*
 *				snap the intersection points		*
 * J. Wu/SAIC           03/07   add de-clustering capability            *
 * D.W.Plummer/NCEP	03/07	keep track of reducePts array entries	*
 * D.W.Plummer/NCEP	04/07	Correct keep track of reducePts array 	*
 * D.W.Plummer/NCEP	04/07	Use unique counter kk1 for internal loop*
 * J. Wu/SAIC           01/08   initialize "twin" to -1 for new fmt	*
 ***********************************************************************/
{
    int 		ii, jj, kk, kk1, nn, ier, ns, nip, npjj, npkk;
    int			areaIdx, nsnap, nfmt, total, total1, total2;
    float		area;
    float	        ipx[MAXPTS], ipy[MAXPTS], sipx[MAXPTS], sipy[MAXPTS];

    Boolean		addFmt, intsct;
    
    gpc_polygon		gpc_tmp_poly, gpc_clip_poly, poly_area,
    			gpc_diff_poly, poly_test, gpc_poly_1;
    gpc_vertex_list     contour_tmp1;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *   Delete the original format structure and its twins, if any.
     */
    if ( (*fmt_in)[ index ].delete == G_TRUE ) {
        return;
    }
   
    (*fmt_in)[ index ].delete = G_TRUE;
        
    if ( index < (*fmt_in)[ index ].twin ) {
        (*fmt_in)[ (*fmt_in)[ index ].twin ].delete = G_TRUE; 
    }


    /*
     *  If the original format structure does not have a twin 
     *  format structure resulting from the area rules (af_areaRules), 
     *  do not clip it along the area boundary.  Simply regenerate a
     *  format structure use the polygon in the the original structure.
     */    
    if ( (*fmt_in)[ index ].twin < 0 ) {
	G_REALLOC ( (*fmt_in), GFA_Elem_Format, (*nin + 1), 
		     "af_reducePtClipFAArea: fmt_in" );

	af_poly2fmt ( (*fmt_in)[ index ].el_poly->contour[0], 
	              (*fmt_in)[ index ].el,
                      (*fmt_in)[ index ].region, 
		       &(*fmt_in)[ *nin ], &ier );
	
	(*fmt_in)[ *nin ].twin = -1;
				      			
	strcpy ( (*fmt_in)[ *nin ].area,  (*fmt_in)[ index ].area );
	
	if ( (*fmt_in)[ index ].origInfo != (GFA_SmrOlk_Grp *)NULL ) {
	     (*fmt_in)[ *nin ].origInfo = (*fmt_in)[ index ].origInfo;
	}

	for ( ii = 0; ii < (*fmt_in)[ index ].el.elem.gfa.info.npts; ii++ ) {
	    (*fmt_in)[ *nin ].reduceFlg[ii] = (*fmt_in)[ index ].reduceFlg[ii];
	}
		
	(*nin)++;            
        
	return;
    }
    
        
    
    /*
     * Find the two areas within the given FA region. 
     */
    if ( (*fmt_in)[ index ].region == 'W' ) {
	areaIdx = 0;
    }
    else if ( (*fmt_in)[ index ].region == 'C' ) {
	areaIdx = 2;
    }
    else {
	areaIdx = 4;
    }

        
    /*
     *  Find the intersection points with the extended FA area bounds
     *  and snap them.  
     */
    nip = 0;
    _reduceptsGetInt ( (*fmt_in)[ index ].el, &nip, ipx, ipy, sipx, sipy, &ier );
    
    
    /*
     *  Do areal clipping. The strategy is the same as regional clipping.  
     *  We clip the polygon against the two EXTENDED FA areas in the region.  
     *  For each resulting part,  if any, check the size.  If it is >= 3K sqnm,
     *  it will become its own AIRMET or OUTLOOK (another element in the 
     *  fmt array) and it gets subtracted from the original polygon.  
     *  If it is <= 3K sqnm, it remains with the original polygon.  
     *  The result may be any number of new polygons and each produces 
     *  its own AIRMET or OUTLOOK.
     */    
    total = *nin;
    gpc_diff_poly.num_contours = 1;
    nsnap = (*fmt_in)[ index ].origInfo->nsnapshot;
    
    for ( ii = areaIdx; ii <= (areaIdx + 1); ii++ ) {
    	
	af_elm2poly ( (*fmt_in)[ index ].el, &gpc_clip_poly, &ier );	
   		
	/*
	  *  Clipping
	  */
	for ( jj = areaIdx; jj <= (areaIdx + 1); jj++ ) {     	       	    
	    
	    if ( jj != ii ) { 	       
	        
		gpc_polygon_clip ( GPC_INT, &gpc_clip_poly,
		                   &_areaXBndPoly[ jj ], &gpc_tmp_poly );                           

               	        				
		for ( kk = 0; kk < gpc_tmp_poly.num_contours; kk++ )  {
	    	    
	            area = af_gpcPolyArea( &gpc_tmp_poly.contour[ kk ], sys_N );
		    
		    if ( area >= AREA_LIMIT )  {

			gpc_diff_poly.contour = &gpc_tmp_poly.contour[ kk ];
		        
			gpc_polygon_clip ( GPC_DIFF, &gpc_clip_poly, 
				           &gpc_diff_poly, &gpc_clip_poly );		         
		        

		    }
	        }
		
		gpc_free_polygon ( &gpc_tmp_poly );		            
	    }            	
	}    
		
	
	/*
	  *  Produce new format structures for polygons with size >= 3K sqnm 
	  *  intersecting at least one of the snapshots (user-drawn AIRMET/OUTLOOK
	  *  do not need to intersect snapshots) with an area >= 3K sqnm.
	  */	
        if ( gpc_clip_poly.num_contours > 0  ) {
    	    	    	    
	    for ( kk = 0; kk < gpc_clip_poly.num_contours; kk++ ) {
	            
		 area = af_gpcPolyArea( &gpc_clip_poly.contour[kk], sys_N ); 
		 
		 addFmt = False;
		 if ( area >= AREA_LIMIT ) {
						     		     
	             /*
	                *  Check if the contour intersects with the area bound.
	                */
	             gpc_diff_poly.contour = &gpc_clip_poly.contour[ kk ];
                     gpc_polygon_clip ( GPC_INT, &_areaBndPoly[ ii ], &gpc_diff_poly,
                                      		 &poly_area );
	             
		     intsct = False;
		     if ( poly_area.num_contours > 0 ) {
		         for ( nn = 0; nn < poly_area.num_contours; nn++ ) {
			     if ( poly_area.hole[ nn ] == G_FALSE ) {
			         intsct = True;
				 break;
			     }
			 }
		     }
		     		     
		     gpc_free_polygon ( &poly_area );
 		     
		     if ( !intsct ) {
		         continue;
		     }
		     
		     /*
			*  Check if this part is eligible to be added in - either
			*  a user-drawn AIRMET/OUTLOOK or intersecting at least
			*  one of the snapshots with an area >= 3K sqnm.
			*/
		     if ( nsnap <= 0 ) {
		         addFmt = True;
		     }
		     else {
		         for ( nn = 0; nn < nsnap; nn++ ) {
			     af_elm2poly ( *((*fmt_in)[ index ].origInfo->snapshots[ nn ]), 
			                   &poly_test, &ier ); 
			     gpc_polygon_clip ( GPC_INT, &poly_test, &gpc_clip_poly,
                                      		&gpc_poly_1 );

			     nfmt = gpc_poly_1.num_contours;
			     if ( nfmt > 0 ) {
				 for ( ns = 0; ns < nfmt; ns++ ) {
				     if ( af_gpcPolyArea( &gpc_poly_1.contour[ns], sys_N )
					                          >= AREA_LIMIT)  {
				         addFmt = True;
				         break;
				     }
				  }
			     }
				    
			     gpc_free_polygon ( &gpc_poly_1 );
                             gpc_free_polygon ( &poly_test );        
			     
			     if ( addFmt ) break;
				    
		         }
		    }

		            
		    /*
		       *  Increase the size of the format array by 1 and add the part in.
		       */
		    if ( addFmt ) {
	                			
			G_REALLOC ( (*fmt_in), GFA_Elem_Format, (total + 1), 
			                   "af_reducePtClipFAArea: fmt_in" );


			/*
			 *  Replace the intersection points with the snapped matches.
			 */
			 af_replaceIntPt ( gpc_clip_poly.contour[ kk ], nip, ipx, ipy,
					   sipx, sipy, &contour_tmp1, &ier );

			/*
 			 *  Create a new format structure with the contour.
 			 */
			af_poly2fmt ( contour_tmp1, (*fmt_in)[ index ].el,
                                      (*fmt_in)[ index ].region, 
				      &(*fmt_in)[ total ], &ier );
			
			(*fmt_in)[ total ].twin = -1;      			
						
			strcpy ( (*fmt_in)[ total ].area, _FA_Area[ ii ] );
			if ( (*fmt_in)[ index ].origInfo != (GFA_SmrOlk_Grp *)NULL ) {
			    (*fmt_in)[ total ].origInfo = (*fmt_in)[ index ].origInfo;
			}
                        npjj = (*fmt_in)[ total ].el.elem.gfa.info.npts;
                        npkk = (*fmt_in)[ index ].el.elem.gfa.info.npts;
                        for ( jj = 0; jj < npjj; jj++ )  {
                            (*fmt_in)[ total ].reduceFlg[jj] = G_FALSE;
                            for ( kk1 = 0; kk1 < npkk; kk1++ )  {
                                if ( G_DIFFT((*fmt_in)[ total ].el.elem.gfa.latlon[jj],
                                            (*fmt_in)[ index ].el.elem.gfa.latlon[kk1], TOL) &&
                                     G_DIFFT((*fmt_in)[ total ].el.elem.gfa.latlon[jj+npjj],
                                            (*fmt_in)[ index ].el.elem.gfa.latlon[kk1+npkk], TOL) )  {
                                    (*fmt_in)[ total ].reduceFlg[jj] =
                                        (*fmt_in)[ index ].reduceFlg[kk1];
                                }
                            }
                        }
								        
			total++;        

			free ( contour_tmp1.vertex );
		        
		    }
		}       /* End of inner "if ( area >= AREA_LIMIT )  */			
	    }		/* End of kk loop  */
	}		/* End of "if ( gpc_clip_poly.num_contours > 0 ) " */
        
	gpc_free_polygon ( &gpc_clip_poly );	
    }				   

    /*
     * If we've split into two new polygons, check the two new polygons against
     * each other for common points and set all of them to non-reduceable.
     */
    if ( ( total - *nin ) == 2 )  {
	total1 = total - 1;
	total2 = total - 2;
	npjj = (*fmt_in)[ total1 ].el.elem.gfa.info.npts;
        npkk = (*fmt_in)[ total2 ].el.elem.gfa.info.npts;
	for ( jj = 0; jj < npjj; jj++ )  {
    	    for ( kk = 0; kk < npkk; kk++ )  {
	        if ( G_DIFFT((*fmt_in)[ total1 ].el.elem.gfa.latlon[jj],
			     (*fmt_in)[ total2 ].el.elem.gfa.latlon[kk], TOL) &&
		     G_DIFFT((*fmt_in)[ total1 ].el.elem.gfa.latlon[jj+npjj],
		             (*fmt_in)[ total2 ].el.elem.gfa.latlon[kk+npkk], TOL) )  {
		    (*fmt_in)[ total1 ].reduceFlg[jj] = G_FALSE;
		    (*fmt_in)[ total2 ].reduceFlg[kk] = G_FALSE;
		}
	    }
	}	    
    }
                  	        
    *nin = total;
}

/*=====================================================================*/

static void _reducePtsBisectSimple ( int index, int round, int *nin,  
			GFA_Elem_Format **fmt_in, int *iret )
/************************************************************************
 * _reducePtsBisectSimple                                            	*
 *                                                                      *
 * This routine divides the polygon in the input format structure into 	*
 * two polygons if it cannot be represented on 3 lines of 65-character	*
 * text. The original format structure is deleted and two new format	*
 * structures are added to the end of the format structure array. So it	*
 * is up to the caller to free those additional space allocated. For the*
 * first round division, we divide the splitting segment into two 	*
 * segments at the middle (snap it to the closest VOR point) and check 	*
 * if these two segments intersect the original polygon.  If not,  the	*
 * snapped middle point will be added to the splitted polygons.  For 	*
 * the second round and on,  no middle points are inserted.		*
 *	                                                          	*
 * static void _reducePtsBisectSimple ( index, nin, fmt_in, iret )	*
 *                                                                      *
 * Input parameters:                                             	*
 *      index		int     	index of the fmt struct to be	*
 *                                      bisected			*
 *      round		int     	round of bisection (1 or 2)	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *nin		int     	total # of fmt structs		*
 *      **fmt_in	GFA_Elem_Format Array of GFA format structures	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/06   initial coding				*
 * J. Wu/SAIC		11/06   fix memory leaks			*
 * J. Wu/SAIC		12/06   improve the algorithm			*
 * D.W.Plummer/NCEP	03/07	improve simple split algorithm		*
 * J. Wu/SAIC           03/07   prevent the middle point from clustering*
 * D.W.Plummer/NCEP	03/07	keep track of bisection midpoints	*
 * J. Wu/SAIC           01/08   initialize "twin" to -1 for new fmt	*
***********************************************************************/
{
    int 		ii, jj, kk, np, ier, subType, part2, splitSeg[2];
    int			cbf, np1, np2, npjj, npkk, one = 1, nd, found, tntmp;
    int			nin1, ntmp, *secondIndex, nslack;
    float		*tplat, *tplon, area, areaOrig, pct;
    float		*xd, *yd, minDist, *tmpLat, *tmpLon;
    float		*xd1, *yd1, *xd2, *yd2, xmid, ymid;
    float		mlat, mlon, *sdist;
    float		prefPctMin, prefPctMax;
    char		prefix[32], value[32];
    
    Boolean		addMidPt, qualify1, qualify2;
    
    gpc_vertex_list	verts1, verts2;
    DInfo_t		*distInfo;
    /* The Earth's radius in nautical miles*/
    float       	_radius = RADIUS*M2NM;
/*--------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  First check if the polygon can be represented on 3 lines of text.
     */
    cvg_getFld ( &((*fmt_in)[index].el), TAG_GFA_SUBTYPE, value, &ier );

    subType = atoi( value ) - atoi( value ) / 10 * 10;
    
    prefix[ 0 ] = '\0';
    if ( subType == GFA_USER_SMEAR || subType == GFA_SYSTEM_SMEAR ) {            
	strcpy ( prefix, "FROM" );
    }
    else if ( subType == GFA_USER_OUTLOOK || 
	     subType == GFA_SYSTEM_OUTLOOK ) {		  
	strcpy ( prefix, "BOUNDED BY" );
    }	  
	
    np = (*fmt_in)[ index ].el.elem.gfa.info.npts;	 
    cbf = cgr_canBeFormatted ( np, (*fmt_in)[ index ].el.elem.gfa.latlon, 
	               &((*fmt_in)[ index ].el.elem.gfa.latlon[np]), prefix );
    
    /*
     *  Return if it can be formatted already.
     */
    if ( cbf == G_TRUE ) {
        return;
    }

            
    /*
     *  Make a CLOSED polygon and keep only two digits after the float point.
     */
    ntmp= np + 1;
                                                                  
    G_MALLOC ( tmpLat, float, ntmp, "_reducePtsBisectSimple: tmpLat" );
    G_MALLOC ( tmpLon, float, ntmp, "_reducePtsBisectSimple: tmpLon" );
    
    for ( ii = 0; ii < np; ii++ ) {
        tmpLat[ ii ] = rint((double)(*fmt_in)[ index ].el.elem.gfa.latlon[ ii ] * 100 ) / 100;
        tmpLon[ ii ] = rint((double)(*fmt_in)[ index ].el.elem.gfa.latlon[ ii + np ] * 100 ) / 100;       
    }
    
    tmpLat[ np ] = tmpLat[ 0 ];
    tmpLon[ np ] = tmpLon[ 0 ];       
    

    /*
     *  Find the splitting segment - the one has the shortest length among
     *  those segments that do not intersect the polygon.
     */
    splitSeg[ 0 ] = -1;
    splitSeg[ 1 ] = -1;
    
    minDist = -1.0F;

    prefPctMax = 60.0F;
    ctb_pfstr ( "GFA_AF_BISECT_PCT", value, &ier );
    if ( ier == 0 )  {
	prefPctMax = atoi ( value );
	if ( prefPctMax < 50.0F )  {
	    prefPctMax = 100.0F - prefPctMax;
	}
    }
    prefPctMin = 100.0F - prefPctMax;

    nslack = (np/2) - 1;
    
    G_MALLOC ( secondIndex,   int, (nslack*2)+1, "Error alloc secondIndex" );
    G_MALLOC (       sdist, float, (nslack*2)+1,       "Error alloc sdist" );
    G_MALLOC ( distInfo, DInfo_t, (np/2)*(nslack*2+1), "Error alloc distInfo" );

    nd = 0;
    for ( ii = 0; ii < (np/2); ii++ ) {

	_reducePtsFindSegment ( ii, nslack, ntmp, tmpLat, tmpLon,
			       secondIndex, sdist, &ier );

	for ( jj = 0; jj < (nslack*2)+1; jj++ )  {

	    if ( secondIndex[jj] != IMISSD )  {
	        distInfo[nd].dist = sdist[jj];
	        distInfo[nd].indx1 = ii;
	        distInfo[nd].indx2 = secondIndex[jj];
	        nd += 1;
	    }
	}
    }

    /*
     * Sort the distInfo array by distance using qsort
     */
    qsort ( distInfo, nd, sizeof(DInfo_t),
	    (int(*)(const void*, const void*))_afsdist_sort );

    /*
     * Allocate space for polygon size testing.
     */
    G_MALLOC ( tplat, float, ntmp, "Error allocating tplat" );
    G_MALLOC ( tplon, float, ntmp, "Error allocating tplon" );

    /*
     * Compute the size of the original polygon.
     */
    cgr_sphpolyarea ( &ntmp, tmpLat, tmpLon, &_radius, &areaOrig, &ier );

    /*
     * Now loop over all the possible splitting segments, from shortest
     * to longest, and find the shortest segment which divides the original 
     * polygon in an acceptable proportion as defined in the prefs table.
     */
    found = G_FALSE;
    for ( ii = 0; ii < nd; ii++ ) {

	/*
	 * Generate the polygon starting from indx1 and ending at indx2.
	 */
	jj = distInfo[ii].indx1;
	tntmp = 0;
	do {
	    tplat [ tntmp ] = tmpLat [ jj ];
	    tplon [ tntmp ] = tmpLon [ jj ];
	    tntmp += 1;
	    jj = ( jj + 1 ) % ntmp;
	} while ( jj != (distInfo[ii].indx2+1)%ntmp );

	/*
	 * Compute and compare it's size to the original polygon 
	 * as a percentage.
	 */
	cgr_sphpolyarea ( &tntmp, tplat, tplon, &_radius, &area, &ier );
	pct = 100.0F * area / areaOrig;

	/*
	 * If the percentage is within the prefs limits, 
	 * we've found our splitting segment.
	 */
	if ( pct >= prefPctMin && pct <= prefPctMax )  {
	    found = G_TRUE;
	    splitSeg[ 0 ] = distInfo[ii].indx1;
	    splitSeg[ 1 ] = distInfo[ii].indx2;
	    break;
	}
    }

    G_FREE ( tplat, float );
    G_FREE ( tplon, float );

    G_FREE (    distInfo, DInfo_t );
    G_FREE (       sdist,   float );
    G_FREE ( secondIndex,     int );
    
    /*
     *  Process the splitting segment.
     */
    if ( found == G_TRUE ) {

    /*
     *  Find the correct number of points for new polygons.  For first round
     *  division, the middle point on the splitting segment is calculated
     *  and snapped to be used later.  The splitting segment is thus divided
     *  into two segments at the middle point.  If either oneof them intersects
     *  with the original polygon,  the middle point will not be added.
     */
    np1 = splitSeg[ 1 ] - splitSeg[ 0 ] + 1;
    np2 = np - np1 + 2 ;
    
    addMidPt = False;
    if ( round == 1 ) {
	xmid = ( tmpLat[ splitSeg[0] ] + tmpLat[ splitSeg[1] ] ) / 2.0;
	ymid = ( tmpLon[ splitSeg[0] ] + tmpLon[ splitSeg[1] ] ) / 2.0;
        
	clo_snapOnePt( xmid, ymid, &mlat, &mlon, &ier );
        
	if ( ier == 0 &&
		!clo_isCluster( &mlat, &mlon, &tmpLat[ splitSeg[0] ],
		    		&tmpLon[ splitSeg[0] ], _clusterDst )  &&
		!clo_isCluster( &mlat, &mlon, &tmpLat[ splitSeg[1] ], 
		                &tmpLon[ splitSeg[1] ], _clusterDst ) ) {
	    
	    qualify1 = _reducePtsSegIntPoly ( ntmp, tmpLat, tmpLon, 
		           tmpLat[ splitSeg[0] ], tmpLon[ splitSeg[0] ], mlat, mlon );
            
	    if ( qualify1 ) {
	        
		qualify2 = _reducePtsSegIntPoly ( ntmp, tmpLat, tmpLon, 
		                 mlat, mlon, tmpLat[ splitSeg[1] ], tmpLon[ splitSeg[1] ] );
	    
	        if ( qualify2 ) {
	            
		    gtrans ( sys_M, sys_N, &one, &mlat, &mlon, &xmid, &ymid, 
                             &ier, strlen(sys_M), strlen(sys_N) );	            
	
	            addMidPt = True;
	    
	            np1++;
                    np2++;
	        }
	    }
	}   
    }
    

    /*
     *  Now split the original polygon into two along the splitting segment.
     */
    G_MALLOC ( xd, float,  np,  "_reducePtsBisectSimple: xd" );
    G_MALLOC ( yd, float,  np,  "_reducePtsBisectSimple: yd" );
    G_MALLOC ( xd1, float, np1, "_reducePtsBisectSimple: xd1" );
    G_MALLOC ( yd1, float, np1, "_reducePtsBisectSimple: yd1" );
    G_MALLOC ( xd2, float, np2, "_reducePtsBisectSimple: xd2" );
    G_MALLOC ( yd2, float, np2, "_reducePtsBisectSimple: yd2" );
        
    gpc_gvlist ( (*fmt_in)[ index ].el_poly->contour, &np, xd, yd, &ier );    

    
    /*
     *  First polygon - from splitSeg[0] to splitSeg[1], plus a middle point.
     */    
    for ( ii = 0; ii < np1 - 1; ii++ ) {
	xd1[ ii ] = xd[ splitSeg[ 0 ] + ii ];   
        yd1[ ii ] = yd[ splitSeg[ 0 ] + ii ];   
    }
    
    if ( addMidPt ) {
	xd1[ np1 - 1 ] = xmid;   
        yd1[ np1 - 1 ] = ymid;   				    
    }
    else {
	xd1[ np1 - 1 ] = xd[ splitSeg[ 0 ] + np1 - 1 ];   
        yd1[ np1 - 1 ] = yd[ splitSeg[ 0 ] + np1 - 1 ];   
    }
    
  		        	
    /*
     *  Second polygon - from 0 to splitSeg[0], then middle point, 
     *  continue from splitSeg[1] to np.
     */
    for ( ii = 0; ii <= splitSeg[ 0 ]; ii++ ) {
	xd2[ ii ] = xd[ ii ];   
        yd2[ ii ] = yd[ ii ];   
    }
        
    part2 = splitSeg[ 0 ] + 1;
    if ( addMidPt ) {
	xd2[ part2 ] = xmid;   
        yd2[ part2 ] = ymid;   				    
        part2++;
    } 
            
    for ( ii = part2; ii < np2; ii++ ) {
	xd2[ ii ] = xd[ ii + splitSeg[ 1 ] - part2 ];   
        yd2[ ii ] = yd[ ii + splitSeg[ 1 ] - part2 ];   
    }

    if ( addMidPt ) {
	/*
	 * Add midpoint to list of generated midpoints.
	 */
	G_REALLOC ( _midP, MInfo_t, (_n_midP+1)*sizeof(MInfo_t), 
		"Error reallocating _midP" );
	_midP[_n_midP].midp_lat = mlat;
	_midP[_n_midP].midp_lon = mlon;
	_midP[_n_midP].status = UNUSED;
	_n_midP += 1;
    }
    else  {
	/*
	 * Check if one of the endpoints is in the list of 
	 * previously generated midpoints. If so, flag as USED.
	 */
	for ( ii = 0; ii < _n_midP; ii++ )  {
	    if ( ( G_DIFFT(tmpLat[splitSeg[0]],_midP[ii].midp_lat,TOL) &&
		   G_DIFFT(tmpLon[splitSeg[0]],_midP[ii].midp_lon,TOL) )  ||
	         ( G_DIFFT(tmpLat[splitSeg[1]],_midP[ii].midp_lat,TOL) &&
		   G_DIFFT(tmpLon[splitSeg[1]],_midP[ii].midp_lon,TOL) ) )  {
		_midP[ii].status = USED;
	    }
	}
    }

    	                			
    /*
     *  Create two new format structures from the split polygons.
     */
    G_REALLOC ( (*fmt_in), GFA_Elem_Format, (*nin + 2), 
			                   "_reducePtBisectSimple: fmt_in" );
    
    verts1.vertex          = (gpc_vertex*)NULL;
    verts1.num_vertices    = 0;

    gpc_cvlist ( np1, xd1, yd1, &verts1, &ier );
    af_poly2fmt ( verts1, (*fmt_in)[ index ].el,
                  (*fmt_in)[ index ].region, &(*fmt_in)[ *nin ], &ier );    
    
    (*fmt_in)[ *nin ].twin = -1;
    strcpy ( (*fmt_in)[ *nin ].area, (*fmt_in)[ index ].area );
    if ( (*fmt_in)[ index ].origInfo != (GFA_SmrOlk_Grp *)NULL ) {
	 (*fmt_in)[ *nin ].origInfo = (*fmt_in)[ index ].origInfo;
    }
    npjj = (*fmt_in)[  *nin ].el.elem.gfa.info.npts;
    npkk = (*fmt_in)[ index ].el.elem.gfa.info.npts;
    for ( jj = 0; jj < npjj; jj++ )  {
	(*fmt_in)[ *nin ].reduceFlg[jj] = G_FALSE;
	found = G_FALSE;
	for ( kk = 0; kk < npkk; kk++ )  {
	    if ( G_DIFFT((*fmt_in)[  *nin ].el.elem.gfa.latlon[jj],
			(*fmt_in)[ index ].el.elem.gfa.latlon[kk], TOL ) &&
		 G_DIFFT((*fmt_in)[  *nin ].el.elem.gfa.latlon[jj+npjj],
		        (*fmt_in)[ index ].el.elem.gfa.latlon[kk+npkk], TOL ) )  {
		(*fmt_in)[ *nin ].reduceFlg[jj] = (*fmt_in)[ index ].reduceFlg[kk];
		found = G_TRUE;
	    }
	}
    }
    
    free ( verts1.vertex );
   

    verts2.vertex          = (gpc_vertex*)NULL;
    verts2.num_vertices    = 0;

    gpc_cvlist ( np2, xd2, yd2, &verts2, &ier );
    
    af_poly2fmt ( verts2, (*fmt_in)[ index ].el,
                  (*fmt_in)[ index ].region, &(*fmt_in)[ *nin + 1 ], &ier );
    
    (*fmt_in)[ *nin + 1 ].twin = -1;
    strcpy ( (*fmt_in)[ *nin + 1 ].area, (*fmt_in)[ index ].area );
    if ( (*fmt_in)[ index ].origInfo != (GFA_SmrOlk_Grp *)NULL ) {
	 (*fmt_in)[ *nin + 1 ].origInfo = (*fmt_in)[ index ].origInfo;
    }

    nin1 = *nin + 1;
    npjj = (*fmt_in)[  nin1 ].el.elem.gfa.info.npts;
    npkk = (*fmt_in)[ index ].el.elem.gfa.info.npts;
    for ( jj = 0; jj < npjj; jj++ )  {
	(*fmt_in)[ nin1 ].reduceFlg[jj] = G_FALSE;
	found = G_FALSE;
	for ( kk = 0; kk < npkk; kk++ )  {
	    if ( G_DIFFT((*fmt_in)[  nin1 ].el.elem.gfa.latlon[jj],
			(*fmt_in)[ index ].el.elem.gfa.latlon[kk], TOL ) &&
		 G_DIFFT((*fmt_in)[  nin1 ].el.elem.gfa.latlon[jj+npjj],
		        (*fmt_in)[ index ].el.elem.gfa.latlon[kk+npkk], TOL ) )  {
		(*fmt_in)[ nin1].reduceFlg[jj] = (*fmt_in)[ index ].reduceFlg[kk];
		found = G_TRUE;
	    }
	}
    }
    
    free ( verts2.vertex );
    
    G_FREE ( xd, float );
    G_FREE ( yd, float );	            
    G_FREE ( xd1, float );
    G_FREE ( yd1, float );	        
    G_FREE ( xd2, float );
    G_FREE ( yd2, float );	            
    
    
    /*
     *  Update the total number format structures in the array and delete
     *  the original format strucure.
     */
    (*nin) += 2;
    (*fmt_in)[ index ].delete = G_TRUE;        
     
    }

    G_FREE ( tmpLat, float );
    G_FREE ( tmpLon, float );
 
}

/*=====================================================================*/

static int	_afsdist_sort ( DInfo_t *d1, DInfo_t *d2 )
/************************************************************************
 *                                                                      *
 * _afsdist_sort                                             		*
 *                                                                      *
 * This private fxn sorts the simple division structure by distance.	*
 *                                                                      *
 * static int _afsdist_sort ( INTPOINT *i1, INTPOINT *i2 )              *
 *                                                                      *
 * Input parameters:                                                    *
 * *d1          DInfo_t        Distance 1				*
 * *d2          DInfo_t        Distance 2				*
 *                                                                      *
 * Output parameters:                                                   *
 * none                                                                 *
 *                                                                      *
 * Returned parameters:                                                 *
 * _afsdist_sort        int             Logical return                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     03/07                                           *
 ***********************************************************************/
{
    if ( d1->dist > d2->dist )
	return (  1 );
    else
	return ( -1 );
}

/*=====================================================================*/

static Boolean _reducePtsSegIntPoly ( int npts, float *polyLat, 
		float *polyLon, float firstLat, float firstLon, 
		float secondLat, float secondLon )
/************************************************************************
 * _reducePtsSegIntPoly 	                                   	*
 *                                                                      *
 * This function verifies if the given segment intersects with any 	*
 * segments on the polygon other than the 4 segments starting or ending	*
 * from the given two points. 						*
 *                                                                      *
 * Note: a closed polygon is assumed - the first point is the same as	*
 *       the last point.  If not, the result may not be correct.	*
 *		                                                        *
 * static Boolean _reducePtsSegIntPoly ( npts, polyLat, polyLon,	*
 *                      firstLat, firstLon, secondLat, secondLon )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      npts		int		number of polygon points        *
 *     *polyLat		float		lats of all smear points       	*
 *     *polyLon		float		lons of all smear points	*
 *      firstLat	float           lat of the 1st point of segment	*
 *      firstLon	float          	lon of the 1st point of segment	*
 *      secondLat	float           lat of the 2nd point of segment	*
 *      secondLon	float           lon of the 2nd point of	segment	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      Boolean         False- no intersection        	*
 *                                      True - intesecting 		*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          10/06    inital coding          			*
 * J. Wu/SAIC          12/06    improve the algorithm          		*
 ***********************************************************************/
{
    int         ii, ier, one = 1;
    int         inout[1], intsct;
                                                                                
    float       pLat[ 2 ], pLon[ 2 ], xint, yint;
    float       lineLat[ 2 ], lineLon[ 2 ], mlat, mlon;
                                                                              
    Boolean     qualify;
/*---------------------------------------------------------------------*/
                                                                             
    lineLat[ 0 ] = firstLat ;
    lineLat[ 1 ] = secondLat;
    lineLon[ 0 ] = firstLon;
    lineLon[ 1 ] = secondLon;   
                                                                                

    /*
     *  First check if the segment fully lies outside of the polygon.
     *  If so,  the segment cannot be used to bisect the polygon.
     *  
     *  Then check if the segment intersects other segments of
     *  the polygon - except the four segments that share either 
     *  the starting or ending point with the segment on check.   
     */
    qualify = True;

    mlat = ( firstLat +  secondLat ) / 2.0;
    mlon = ( firstLon +  secondLon ) / 2.0;
    
    cgr_inpoly ( sys_M, &one, &mlat, &mlon, 
		 sys_M, &npts, polyLat, polyLon, inout, &ier );
     
    if ( inout[ 0 ] == G_FALSE ) {
	qualify = False;
    }
    else {
         for ( ii = 0; ii < npts - 1; ii++ ) {
	     pLat[ 0 ] = polyLat[ ii ];
	     pLon[ 0 ] = polyLon[ ii ];
	     pLat[ 1 ] = polyLat[ ii + 1 ];
	     pLon[ 1 ] = polyLon[ ii + 1 ];
            
	     cgr_segint ( sys_M, lineLat, lineLon, sys_M, pLat, pLon, 
	                  sys_M, &xint, &yint, &intsct, &ier);	    
	
	     if ( intsct == G_TRUE &&
	          !SAME_LATLON( xint, yint, lineLat[0], lineLon[0], 0.001 ) &&
	          !SAME_LATLON( xint, yint, lineLat[1], lineLon[1], 0.001 ) ) {
	         qualify = False;
	         break;
	    }
	}
    }
 
    return qualify;
                     
}

/*=====================================================================*/

static void _reducePtsFindSegment ( int index, int slack, int nin, float *inLat, 
		float *inLon, int *endIndex, float *length, int *iret )
/************************************************************************
 * _reducePtsFindSegment 	                                   	*
 *                                                                      *
 * This function finds the segment that does not intersects with other	*
 * segments on the polygon (except the 4 segments that shares either	*
 * starting or ending point with the segment on check) and has the 	*
 * shortest length as well.  "2*slack+1" segments are checked - each	*
 * starting at "index"-th vertex of the ploygon & ends at a vertex	*
 * within "index + (nin-1)/2 - slack" to "index + (nin-1)/2 + slack". 	*
 *                                                                      *
 * Note: a closed polygon is assumed - the first point is the same as	*
 *       the last point.  If not, the result may not be correct.	*
 *                                                                      *
 * static void _reducePtsFindSegment ( index, npts, polyLat, polyLon,	*
 *                      endIndex, length, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      index		int		index of the starting point    	*
 *      slack		int		number of points to be checked	*
 *					before/after the index    	*
 *      nin		int		number of polygon points        *
 *     *inLat		float		lats of all smear points       	*
 *     *inLon		float		lons of all smear points	*
 *                                                                      *
 * Output parameters:                                                   *
 *     *endIndex	int		index of the ending point    	*
 *     *length		float		length of the segment    	*
 *     *iret		int		Error code    			*
 *                      		 0 - normal                  	*
 *                      		-1 - no segment found           *
 * Return parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          12/06    inital coding          			*
 * D.W.Plummer/NCEP    03/07	make arrays of endIndex and length	*
 * J. Wu/SAIC          03/07    force the length of the splitting       *
 *                              segment to be greater than _clusterDst  *
 ***********************************************************************/
{
    int         ii, npts, ier, one = 1, mid, *indxArray;
    float       lat1, lon1, lat2, lon2, dist, nseg;
                                                                              
    Boolean     qualify;
/*---------------------------------------------------------------------*/
                                                                             
    *iret	= 0;
       
    /*
     *  Find the index of three ending points to form segments - 
     *  middle point of the array and the points before and after it.
     */    
    lat1 = inLat[ index ];
    lon1 = inLon[ index ];
    
    npts = nin - 1;
    
    mid = npts / 2;
    
    nseg = (2*slack + 1);
    
    G_MALLOC ( indxArray, int,  nseg,  "_reducePtsFindSeg: indxArray" );
    
    for ( ii = 0; ii < nseg; ii++ ) {
	indxArray [ ii ] = ( ii - slack + index + mid + npts ) % npts;
    }
    
    /*
     *  Find out which segment does not intersect other segments
     *  of the polygon and has the shorest length (but is greater
     *  than the clustering distance ) as well.
     */
    for ( ii = 0; ii < nseg; ii++ ) {
        
	lat2 = inLat[ indxArray[ ii ] ];
        lon2 = inLon[ indxArray[ ii ] ];
        
	qualify = _reducePtsSegIntPoly ( nin, inLat, inLon, 
				lat1, lon1, lat2, lon2 );
	         
        if ( qualify ) {	    
	    
	    clo_dist ( &lat1, &lon1, &one, &lat2, &lon2, &dist, &ier );
	    
	    if ( ier == 0 && dist*M2NM > _clusterDst ) {
		endIndex[ii] = indxArray[ ii ];
		length[ii] = dist;
	    }
	    else {
		endIndex[ii] = IMISSD;
		length[ii]   = RMISSD;
	    }
	}
	else  {
	    endIndex[ii] = IMISSD;
	    length[ii]   = RMISSD;
	}
    }

    G_FREE ( indxArray, int );
}

/*=====================================================================*/
static void _reduceptsGetInt ( VG_DBStruct el, int *nip,  float *ipx, 
		float *ipy, float *sipx, float *sipy, int *iret )
/************************************************************************
 * _reduceptsGetInt                                                  	*
 *                                                                      *
 * This routine finds the intersection points of a GFA with the common	*
 * border of the extended SLC and SFO areas, as well as those with the 	*
 * common border of CHI/BOS and DFW/MIA areas.  The points are then 	*
 * inserted into the GFA polygon and snapped individually to points 	*
 * outside of the GFA polygon.  Both the intersection points and their 	*
 * snapped matches are returned in map coordinate.			*
 *                                                                      *
 * static void _reduceptsGetInt ( el, nip, ipx, ipy, sipx, sipy, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *       el	VG_DBStruct	GFA element	             		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nip	int         	Total number of common intersection pt 	*
 *      *ipx	float         	X-coords of intersection pt		*
 *      *ipy	float         	Y-coords of intersection pt 		*
 *      *sipx	float         	snapped matches of ipx outside of GFA 	*
 *      *sipy	float         	snapped matches of ipy outside of GFA	*
 *      *iret	int             Return code                    		*
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          01/07   	Created                         	*
 * J. Wu/SAIC          03/07    prevent adding clustering points        *
 * J. Wu/SAIC          05/07    add more for clustering processing	*
 * J. Wu/SAIC	       01/08   Correct longtitude from ipx to ipy	*
***********************************************************************/
{
    int 	ii, jj, kk, maxp, np, nout[4], ier, kk2, skip;
    int 	*bp1[4], *ap1, *bp2, *ap2, *gipB; 
    int         ier1, ier2, qmatch, one = 1, n4snap;
    int 	closed, rol, *inout, npp, nchk, addOne; 
    float       qdist, qdist1, qdist2, *chkLat, *chkLon, tol;
    float 	*elat, *elon, *xout[4], *yout[4], slat, slon;        
#define	LLSCAL	100
/*--------------------------------------------------------------------*/
                
    *iret = 0;
    *nip = 0;
    
    /*
     *  Allocate space for working arrays.
     */
    maxp = MAX_BOUND_PT; 
    for ( ii = 0; ii < 4; ii++ ) {
        G_MALLOC ( xout[ii], float, maxp, "af_getGFAInt: xout" );
        G_MALLOC ( yout[ii], float, maxp, "af_getGFAInt: yout" );
        G_MALLOC ( bp1[ii], int, maxp, "af_getGFAInt: bp1" );
    }
    
    G_MALLOC ( ap1, int, maxp, "af_getGFAInt: ap1" );
    G_MALLOC ( bp2, int, maxp, "af_getGFAInt: bp2" );
    G_MALLOC ( ap2, int, maxp, "af_getGFAInt: ap2" );
    G_MALLOC ( gipB, int, maxp, "af_getGFAInt: gwcB" );

    G_MALLOC ( inout, int, maxp, "af_getGFAInt: inout" );
    G_MALLOC ( chkLat, float, maxp, "af_getGFAInt: chkLat" );
    G_MALLOC ( chkLon, float, maxp, "af_getGFAInt: chkLon" );
    
    
    /*
     *   GFA polygon should be additional space for cgr_intersect.
     */
    np = el.elem.gfa.info.npts;
    G_MALLOC ( elat, float, np + 1, "af_getGFAInt: elat" );
    G_MALLOC ( elon, float, np + 1, "af_getGFAInt: elon" );
    for ( ii = 0; ii < np; ii++ ) {
        elat[ ii ] = rint ( (double)el.elem.gfa.latlon[ ii ] * LLSCAL ) / LLSCAL; 
        elon[ ii ] = rint ( (double)el.elem.gfa.latlon[ ii+np ] * LLSCAL ) / LLSCAL;
    }
    
    elat[ np ] = elat[ 0 ];    
    elon[ np ] = elon[ 0 ];    
            
    np++;
    
    
    /*
     *   Find intersection points of el with the common part of
     *   extended SFO/SLC bound.
     */
    cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_slcP, _slcX,
                    _slcY, &maxp, sys_M, &nout[0], xout[0], yout[0], 
		    bp1[0], ap1, bp2, ap2, &ier );
    *nip = 0;    
    if ( nout[0] > 0 ) {
	
        for ( ii = 0; ii < nout[0]; ii++ ) {
            xout[0][ ii ] = rint ( (double)xout[0][ ii ] * LLSCAL ) / LLSCAL; 
            yout[0][ ii ] = rint ( (double)yout[0][ ii ] * LLSCAL ) / LLSCAL;
        }
	
        cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_sfoP, _sfoX,
                        _sfoY, &maxp, sys_M, &nout[1], xout[1], yout[1], 
		        bp1[1], ap1, bp2, ap2, &ier );

	if ( nout[ 1 ] > 0 ) {
            for ( ii = 0; ii < nout[1]; ii++ ) {
                xout[1][ ii ] = rint ( (double)xout[1][ ii ] * LLSCAL ) / LLSCAL; 
                yout[1][ ii ] = rint ( (double)yout[1][ ii ] * LLSCAL ) / LLSCAL;
            }
	    
	    for ( jj = 0; jj < nout[ 0 ]; jj++ ) {

		skip = 0;
		for ( kk = 0; kk < _sfoP; kk++ ) {
		    if ( G_DIST ( xout[0][jj], yout[0][jj], _sfoX[kk],
				  _sfoY[kk] ) < SMALLF ) {
			skip = 1;
			break;
		    }
		}

		if ( skip == 1 ) continue;

		for ( kk = 0; kk < nout[ 1 ]; kk++ ) {
		    if ( G_DIST( xout[0][jj], yout[0][jj],
				 xout[1][kk], yout[1][kk] ) < SMALLF ) {
			ipx [ *nip ] = xout[0][jj];
			ipy [ *nip ] = yout[0][jj];
			gipB[ *nip ] = bp1[0][jj];

			(*nip)++;
		    }
		}
	    }
	} 
    }
    
    
    /*
     *   Find intersection points of el with the common part of
     *   extended CHI-BOS and DFW-MIA bounds.
     */
    cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_chibosP, _chibosX,
                    _chibosY, &maxp, sys_M, &nout[2], xout[2], yout[2], 
		    bp1[2], ap1, bp2, ap2, &ier );
        	    	 	
    if ( nout[ 2 ] > 0 ) {
        for ( ii = 0; ii < nout[2]; ii++ ) {
            xout[2][ ii ] = rint ( (double)xout[2][ ii ] * LLSCAL ) / LLSCAL; 
            yout[2][ ii ] = rint ( (double)yout[2][ ii ] * LLSCAL ) / LLSCAL;
        }
    
        cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_dfwmiaP, _dfwmiaX,
                        _dfwmiaY, &maxp, sys_M, &nout[3], xout[3], yout[3], 
		        bp1[3], ap1, bp2, ap2, &ier );
        
 	if ( nout[ 3 ] > 0 ) {
            for ( ii = 0; ii < nout[3]; ii++ ) {
                xout[3][ ii ] = rint ( (double)xout[3][ ii ] * LLSCAL ) / LLSCAL; 
                yout[3][ ii ] = rint ( (double)yout[3][ ii ] * LLSCAL ) / LLSCAL;
            }
	    
	    for ( jj = 0; jj < nout[ 2 ]; jj++ ) {

                skip = 0;
                for ( kk = 0; kk < _dfwmiaP; kk++ ) {
                    if ( G_DIST ( xout[2][jj], yout[2][jj], _dfwmiaX[kk],
                                  _dfwmiaY[kk] ) < SMALLF ) {
                        skip = 1;
                        break;
                    }
                }

                if ( skip == 1 ) continue;

                for ( kk = 0; kk < nout[ 3 ]; kk++ ) {
                    if ( G_DIST( xout[2][jj], yout[2][jj],
                                 xout[3][kk], yout[3][kk] ) < SMALLF ) {
                        ipx [ *nip ] = xout[2][jj];
                        ipy [ *nip ] = yout[2][jj];
                        gipB[ *nip ] = bp1[2][jj];

                        (*nip)++;
                    }
                }
	    }
	} 
   
    }
    
            
    /*
     *   Now insert those common intersection points into the el polygon
     *   and snap those points outside of the el polygon.
     */
    npp = *nip;
    if ( *nip > 0 ) {
        
        /*
          *  Find the common boundary points (extended areas) inside the polygon
	  *  to ensure the new snap points will not cluster with these points.
          */
        closed = 1;
        tol = 0.001F;
        nchk = 0;
	for ( ii = 0; ii < _nextdw; ii++ ) {

	    cgr_qrol ( &np, elon, elat, 
	           &closed, &(_extdwY[ii]), &(_extdwX[ii]), &tol, &rol, &ier );
	    	    
	    if ( rol > 0 ) {
	        chkLat[ nchk ] = _extdwX[ ii ];    
	        chkLon[ nchk ] = _extdwY[ ii ];    
                nchk++;
            }
        }

	for ( ii = 0; ii < _nextdc; ii++ ) {

	    cgr_qrol ( &np, elon, elat, 
	           &closed, &(_extdcY[ii]), &(_extdcX[ii]), &tol, &rol, &ier );
	    	    
	    if ( rol > 0 ) {
	        chkLat[ nchk ] = _extdcX[ ii ];    
	        chkLon[ nchk ] = _extdcY[ ii ];    
                nchk++;
            }
        }


        /*
          *  Snap each intersection point.
          *
	  *  If an intersection point is not within the clustering distance of 
	  *  the point before it (Pb) or point after it (Pa), simply insert it into 
	  *  the polygon and snap it. Otherwise, do the following:
	  *  
	  *  1. Pick the closer one of Pb and Pa as Pn.  
	  *  2. Check if Pn is within the clustering distance of the common boundary
	  *     points inside the FROM line.  
	  *  3. If so, snap Pn to the closest point not within the clustering distance
	  *     and match both the intersection point and Pn to the new point. 
	  *  4. If not, match the intersection point with Pn.
          */
	np = el.elem.gfa.info.npts;
        G_REALLOC ( elat, float, np+2, "af_getGFAInt: elat" );
        G_REALLOC ( elon, float, np+2, "af_getGFAInt: elon" );        
        
	for ( ii = 0; ii < *nip; ii++ ) {

            clo_dist ( &(ipx[ii]), &(ipy[ii]), &one,
                       &(el.elem.gfa.latlon[ gipB[ii] ]),
                       &(el.elem.gfa.latlon[ gipB[ii] + np]),
                       &qdist1, &ier1 );

            clo_dist ( &(ipx[ii]), &(ipy[ii]), &one,
                       &(el.elem.gfa.latlon[(gipB[ii] + 1 + np)%np]),
                       &(el.elem.gfa.latlon[(gipB[ii] + 1 + np)%np + np ]),
                       &qdist2, &ier2 );

	    addOne = -1;
            qmatch = -1;
            if ( ier1 == 0 && ier2 == 0 ) {

		qdist1 = qdist1 * M2NM; 				
		qdist2 = qdist2 * M2NM; 				
		
		if ( qdist1 < qdist2 )  {
		    qmatch = gipB[ii]; 
		    qdist  = qdist1;
		}
		else {
		    qmatch = (gipB[ii] + 1 + np)%np;
		    qdist  = qdist2;
		}

		if ( qdist <= _clusterDst ) {		    
		    
		    for ( kk = 0; kk < nchk; kk++ ) {
                        clo_dist ( &(el.elem.gfa.latlon[ qmatch ]), 
			           &(el.elem.gfa.latlon[ qmatch + np]),
                                   &one, &chkLat[ kk ], &chkLon[ kk ],
                                   &qdist1, &ier );
	                    
			if ( qdist1*M2NM <= _clusterDst ) {
			    addOne = qmatch;
		            break;
			}			     
		    }
		    
		    if ( addOne < 0 ) {
	                sipx[ ii ] = el.elem.gfa.latlon[qmatch];
	                sipy[ ii ] = el.elem.gfa.latlon[qmatch + np];		        
			continue;
		    }
		}
				
	    }


	    /*
	      *  Snap the point.
	      */
	    if ( addOne >= 0 ) {
	        kk = addOne;
		kk2 = kk;
	        for ( jj = 0; jj < np; jj++ ) {
                    elat[ jj ] = el.elem.gfa.latlon[ jj ];
                    elon[ jj ] = el.elem.gfa.latlon[ jj + np ];
	        } 	    
		
		elat[ np ] = elat[ 0 ];
                elon[ np ] = elon[ 0 ];
	        
		n4snap = np + 1;
	    }
	    else {
	        for ( jj = 0; jj <= gipB[ ii ]; jj++ ) {
                    elat[ jj ] = el.elem.gfa.latlon[ jj ];
                    elon[ jj ] = el.elem.gfa.latlon[ jj + np ];
	        }
            	    
	        elat[ gipB[ii] + 1 ] = ipx[ ii ];
                elon[ gipB[ii] + 1 ] = ipy[ ii ];
	    
	        for ( jj = gipB[ ii ] + 2; jj < (np + 1); jj++ ) {
                    elat[ jj ] = el.elem.gfa.latlon[ jj - 1 ];
                    elon[ jj ] = el.elem.gfa.latlon[ jj - 1 + np ];
	        }            	    	    
	    
	        kk = gipB[ ii ] + 1;
	        kk2 = kk;
		
		elat[ np + 1 ] = elat[ 0 ];
                elon[ np + 1 ] = elon[ 0 ];
	        
		n4snap = np + 2;
	    }

            
	    clo_snapPtGFA( kk, elat[ kk ], elon[ kk ], kk2, 
	    		   nchk, chkLat, chkLon, np+1, elat, elon, 
			   True, True, 3.0F, &slat, &slon, &ier );

            if ( ier != 0 ) {
	        slat = ipx[ ii ];
	        slon = ipy[ ii ];	    
	    }
	    
	    sipx[ ii ] = slat;
	    sipy[ ii ] = slon;
	    
	    if ( addOne >= 0 ) {
	        ipx[ npp ] = el.elem.gfa.latlon[addOne];
	        ipy[ npp ] = el.elem.gfa.latlon[addOne+np];	    
	        sipx[ npp ] = slat;
	        sipy[ npp ] = slon;
	    
	        npp++;    
	    }
	    
	}	
    }
    
    
    /*
     *   Clean up.
     */
    for ( ii = 0; ii < 4; ii++ ) {
        G_FREE ( xout[ii], float );
        G_FREE ( yout[ii], float );
        G_FREE ( bp1[ii], int );
    }
    
    G_FREE ( ap1, int );
    G_FREE ( bp2, int );
    G_FREE ( ap2, int );
    G_FREE ( gipB, int );

    G_FREE ( inout, int );
    G_FREE ( chkLat, float );
    G_FREE ( chkLon, float );

    G_FREE ( elat, float );
    G_FREE ( elon, float );

}

/*=====================================================================*/
