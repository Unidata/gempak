#include "afcmn.h"


/************************************************************************
 * affrom.c                                             		*
 *                                                                      *
 * This module contains the subroutines to used to process the FROM	*
 * line.                                         			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *	af_fmtFROMLines		- clip "GFA_SmrOlk_Grp"s into fmt struct*
 *									*
 *   private functions:							*
 *	af_FROMLineRules 	- clip/prepare GFA elem for FA region	*
 *	af_addIntFlg		- set point-reduction flag for regional*
 *				  clipping 				*
 *      af_getGFAInt		- find & snap intersection point with	*
 *				  common boundaries (W&C, C&E)		*
 *	af_addIntFlg4Intl	- set point-reduction flag for 		*
 *				  international clipping 		*
 *	af_cleanContour		- remove consective duplicate points & 	*
 *				  reduce ABA case to A			*
 ***********************************************************************/

/*
 *  Private function prototypes
 */
static void af_FROMLineRules	( gpc_polygon *bnds, VG_DBStruct *el, 
				  int nsnap, VG_DBStruct **snaps,
				 int *numFmt, GFA_Elem_Format **fmt, int *iret );

static void af_addIntFlg	( GFA_Elem_Format *fmt,  int nip, float *ipx, 
				 float *ipy, float *sipx, float *sipy, 
				 int *iret );

static void af_getGFAInt	( VG_DBStruct el, int *nip,  float *ipx, 
		                  float *ipy, float *sipx, float *sipy, 
				 int *iret );

static void af_getIntlPt	( VG_DBStruct *el, int *nip,  float *xip, 
		          	float *yip, float *sxip, float *syip, int *iret );

static void af_addIntFlg4Intl	( GFA_Elem_Format *fmt, int intl_np, float *intl_ipx,
				 float *intl_ipy, float *intl_sipx, float *intl_sipy, 
				 int *iret );
static void af_cleanContour	( gpc_vertex_list ctr_in, 
		       		 gpc_vertex_list *ctr_out, int *iret );
				 
static Boolean af_testSfc	( int ngrp, GFA_SmrOlk_Grp *gfaGrp, int sfcFzl[] );

static void af_adjustFzlvl	( int nout, GFA_Elem_Format **fmt_out, int sfcFzl[] );

/*=====================================================================*/

void af_fmtFROMLines ( int ngrp, GFA_SmrOlk_Grp *gfaGrp, 
		gpc_polygon *rBnds, gpc_polygon *aBnds, 
		int *nout, GFA_Elem_Format **fmt_out, int *iret )
/************************************************************************
 * af_fmtFROMLines                                                     	*
 *                                                                      *
 * This routine clips the GFA smear/outlook in the input GFA_SmrOlk_Grp	*
 * array against FA area (open FZLVLs) or region boundaries (all other	*
 * hazards) to generates an array of GFA format strutures, which could 	*
 * be used to create information string for airmet bulletin.		*
 *                                                                      *
 * void af_fmtFROMLines ( ngrp, gfaGrp,	rBnds, aBnds, numFmt, fmt, iret)* 
 *                                                                      *
 * Input parameters:                                                    *
 *      ngrp		int		number of GFA_SmrOlk_Grp	*
 *      *gfaGrp		GFA_SmrOlk_Grp	array of GFA_SmrOlk_Grp		*
 *      *rBnds		gpc_polygon	FA flight region bounds		*
 *      *aBnds		gpc_polygon	FA flight area bounds		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout		int		number of format structures	*
 *      **fmt_out	GFA_Elem_Format array of clipped format struct	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	03/06   Created                         	*
 * M. Li/SAIC		03/06 	Added inputs to af_FROMLineRules	*  
 * E. Safford/SAIC	07/06	use af_fmtClosedFzlvl			*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * B. Yin/SAIC		12/07	Set lowest fzlvl to 0 if a surface fzlvl*
 *				intersects the FA area			*
 * J. Wu/SGT         	05/14   fix potential infinite loop in          *
 *				af_getIntlPt() when none of the points  *
 *                              in the clipped polygon is an orginal    *
 *                              point of the international bound        *
 ***********************************************************************/
{
    int 		ii, jj, nbf, ier, sfcFzl[ NUM_FA_AREAS ];    
    Boolean		adjFzlvl;
 /*---------------------------------------------------------------------*/
   
    *iret = 0;
           
    /*
     *  Check if any surface FZLVL intersects FA areas.
     */
    adjFzlvl = False;
    adjFzlvl = af_testSfc( ngrp, gfaGrp, sfcFzl );

    /*
     *  Pass GFA groups through FROM line rules to genarate format structures
     */
    *nout = 0;    
    for ( ii = 0; ii < ngrp; ii++ ) {
	
	nbf = *nout;

        if ( strcasecmp ( gfaGrp[ ii ].haz_type, "FZLVL" ) == 0 ) {
	    if ( gfaGrp[ ii ].openFzlvl ) {
	    
	        if ( gfaGrp[ ii ].smear != (VG_DBStruct *)NULL ) {
                    af_fmtOpenFzlvl ( aBnds, gfaGrp[ii].smear, nout, fmt_out, &ier );
	        }

	        if ( gfaGrp[ ii ].outlook != (VG_DBStruct *)NULL ) {
                   af_fmtOpenFzlvl ( aBnds, gfaGrp[ii].outlook, nout, fmt_out, &ier );
	        }
	    }
	    else {
	        if ( gfaGrp[ ii ].smear != (VG_DBStruct *)NULL ) {
                    af_fmtClosedFzlvl( aBnds, gfaGrp[ii].smear, nout, fmt_out, &ier );
	        }

	        if ( gfaGrp[ ii ].outlook != (VG_DBStruct *)NULL ) {
                    af_fmtClosedFzlvl( aBnds, gfaGrp[ii].outlook, nout, fmt_out, &ier );
	        }
	    }

	    /*
	     *  Set lowest FZLVL to 0 if necessary.
	     */
	    if ( adjFzlvl ) af_adjustFzlvl( *nout, fmt_out, sfcFzl );

        }
	else {
	    if ( gfaGrp[ ii ].smear != (VG_DBStruct *)NULL ) {
                af_FROMLineRules ( rBnds, gfaGrp[ii].smear, gfaGrp[ii].nsnapshot, gfaGrp[ii].snapshots, nout, fmt_out, &ier );
 	    }
	    
	    if ( gfaGrp[ ii ].outlook != (VG_DBStruct *)NULL ) {
                af_FROMLineRules ( rBnds, gfaGrp[ii].outlook, gfaGrp[ii].nsnapshot, gfaGrp[ii].snapshots, nout, fmt_out, &ier );
 	    }
	}
	
	/*
	  *  Record the original info where the new elements come from.
	  */
	for ( jj = nbf; jj < *nout; jj++ ) {
	    (*fmt_out)[ jj ].origInfo = &gfaGrp[ ii ];
	}
    }
}

/*=====================================================================*/

static void af_FROMLineRules ( gpc_polygon *bnds, 
                              VG_DBStruct *el_in, int nsnap, 
			      VG_DBStruct **snaps, int *numFmt,
			      GFA_Elem_Format **fmt, int *iret )
/************************************************************************
 * af_FROMLineRules                                                     *
 *                                                                      *
 * This routine clips the input GFA element against all FA region	*
 * boundaries and appends the results to an array of format structures.	*
 *                                                                      *
 * Note: "fmt" is allocated in this routine and must be freed by the	*
 *	 caller.                                                        *
 *                                                                      *
 * static void af_FROMLineRules ( bnds, el_in, numFmt, fmt, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *bnds		gpc_polygon     Array of region bounds polygon	*
 *      *el_in		VG_DBStruct     Pointer to GFA element 		*
 *      nsnap   	int		Number of snapshots		*
 *      **snaps		VG_DBStruct     Snapshot element array 		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *numFmt		int		number of format structures	*
 *      **fmt		GFA_Elem_Format array of clipped format struct	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05   	Created                         	*
 * J. Wu/SAIC          08/05   	Fixed the crash when GFA < 3000		*
 * H. Zeng/SAIC	       10/05	added check for _clippingFlg		*
 * J. Wu/SAIC          10/05   	check the intersection even when the	*
 *                              the cipping is turned off		*
 * J. Wu/SAIC          12/05   	Implemented the new algorithm		*
 * J. Wu/SAIC          02/06    Add new point reduction & snap      	*
 * 				intersection points with boundary      	*
 * J. Wu/SAIC          03/06   	Change "el_in" to a pointer		*
 * M. Li/SAIC	       03/06	Added number of array of snapshots	*
 * M. Li/SAIC	       04/06	Compute GPC_INT against intl bounds	*
 * E. Safford/SAIC     04/06	Fix bug when smear but no snapshots	*
 * E. Safford/SAIC     04/06	modify fmt allocation to avoid mem leak	*
 * M. Li/SAIC	       04/06	Added af_fmtIntlClip			* 
 * J. Wu/SAIC          05/06   	Fix crash from international clipping	*
 * E. Safford/SAIC     08/06	moved from afcreate.c			*
 * J. Wu/SAIC          10/06   	snap and mark the intersection points	*
 *                              with the intl bound non-reduce-able	*
 * J. Wu/SAIC          11/06   	fix a bug that generates airmets in 	*
 *				FA regions with no snapshots		*
 * J. Wu/SAIC          11/06   	Fix a bug that may introduce duplicate	*
 *				points in clipped polygon		*
 * J. Wu/SAIC          12/06   	create a fmt struct only if it intesects*
 *				a snapshot with an area >= 3K sq nm	*
 * J. Wu/SAIC          03/07   	add de-clustering capability		*
 * J. Wu/SAIC          04/07   	compress ABBA point sequence to A	*
 * B. Yin/SAIC	       11/07	initialize polygons			*
 * J. Wu/SAIC          12/07    include 6-6 airmet only if it intersects*
 *                              snapshot "6" with an area >= 3k		*
 ***********************************************************************/
{
    int 	jj, jj1, kk, nn, nfmt, ier, nip, one = 1, nintl, ns;
    float	area, snaparea;
    float	ipx[MAXPTS], ipy[MAXPTS], sipx[MAXPTS], sipy[MAXPTS];
    float	intl_ipx[MAXPTS], intl_ipy[MAXPTS]; 
    float	intl_sipx[MAXPTS], intl_sipy[MAXPTS];
    
    char	fcstHr[16];

    Boolean	addFmt = False, check6_6;
    
    gpc_polygon	gpc_tmp_poly, gpc_clip_poly, gpc_diff_poly, poly_test, 
	 	gpc_poly_0, gpc_poly_1;        
    gpc_vertex_list	contour_tmp0, contour_tmp1, contour_tmp2;
 /*--------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Initialize polygons
 */
    gpc_tmp_poly.num_contours 	= 0;
    gpc_tmp_poly.hole		= NULL;
    gpc_tmp_poly.contour	= NULL;

    gpc_clip_poly.num_contours 	= 0;
    gpc_clip_poly.hole		= NULL;
    gpc_clip_poly.contour	= NULL;

    poly_test.num_contours 	= 0;
    poly_test.hole		= NULL;
    poly_test.contour		= NULL;

    gpc_poly_0.num_contours 	= 0;
    gpc_poly_0.hole		= NULL;
    gpc_poly_0.contour		= NULL;

    gpc_poly_1.num_contours 	= 0;
    gpc_poly_1.hole		= NULL;
    gpc_poly_1.contour		= NULL;

    gpc_diff_poly.num_contours = 1;
       
/*
 *  Airmet "6-6" needs some specical checks.
 */
    check6_6 = False;
    cvg_getFld( el_in, TAG_GFA_FCSTHR, fcstHr, &ier );
    if ( strcmp ( fcstHr, "6-6" ) == 0 ) {
        check6_6 = True;
    }
    
/*
 *  Find the intersection points of GFA with the international bound
 *  and snap them to points outside of the GFA polygon.
 * 
 *  Note: international clipping is ALWAYS on.    
 */
    af_getIntlPt ( el_in, &nintl, intl_ipx, intl_ipy, intl_sipx, intl_sipy, &ier );

/*
 *  If clipping required, find the intersection points of GFA with
 *  the boundary and snap them to points outside of the GFA polygon.
 */
    nip = 0;
    if ( _clippingFlg != 0 ) {
        af_getGFAInt ( *el_in, &nip, ipx, ipy, sipx, sipy, &ier );
    }
                   
/*
 *  The algorithm has three steps:
 *  1. For each FA region, create a GPC polygon from the original GFA 
 *     element and compute its area.  If the area exceeds AREA_LIMIT 
 *     and prefs table GFA clip flag is set to TRUE, clip the polygon
 *     against the bounds of other two FA Regions. 
 *  2. For each resulting clipped part, compute its area.  If the area
 *     exceeds AREA_LIMIT, remove the clipped part from the original 
 *     GPC polygon.  The remaining parts are thus the parts located
 *     in the region in calculation AND those outside of the international
 *     bound (union of three FA region bounds). 
 *  3. For each remaining part,  clip it against international bound.
 *     For each resulting part,  if it is larger than AREA_LIMIT, add
 *     it to format structure array if it has no associated snapshots;
 *     otherwise, add it in only if it intersects with at least one of
 *     its associated snapshots.   
 *     
 *  Note that the intersection points with the boundaries are also  
 *  snapped in a way to make intact the bound between two adjacent parts 
 *  in adjacent FA regions.
 *     
 */
    for ( jj = 0; jj < NUM_FA_REGION; jj++ ) {
       
    	af_elm2poly ( *el_in, &gpc_poly_0, &ier );
    	area = af_gpcPolyArea( &gpc_poly_0.contour[0], sys_N );
               	
	if ( area >= AREA_LIMIT ) {
	                   
/*
 *   Do international clipping.
 */
            gpc_polygon_clip ( GPC_INT, &gpc_poly_0, &_intlBndPoly, &gpc_clip_poly );
	    	    	    	    	    
/*
 *  Do regional clipping as described in step 1 & 2.
 */
	    if ( _clippingFlg != 0 ) {

		for ( jj1 = 0; jj1 < NUM_FA_REGION; jj1++ ) {
               	       
		    if ( jj1 != jj ) {
 	       
			gpc_polygon_clip ( GPC_INT, &gpc_clip_poly, &bnds[jj1], &gpc_tmp_poly );
            
			for ( kk = 0; kk < gpc_tmp_poly.num_contours; kk++ )  {
	    	    
			    area = af_gpcPolyArea( &gpc_tmp_poly.contour[kk], sys_N );
			
			    if ( area > AREA_LIMIT )  {
           
				gpc_diff_poly.contour = &gpc_tmp_poly.contour[kk];

				gpc_polygon_clip ( GPC_DIFF, &gpc_clip_poly, 
					           &gpc_diff_poly, &gpc_clip_poly );
		            }
		        }
            		
			gpc_free_polygon ( &gpc_tmp_poly );		            
		    }                
		}	   
	    } /* End of "*if ( _clippingFlg != 0 )" */
                  	    
/* 
 *   Add parts into format structure arrry 
 *   as described in step 1 & 2
 */	    
	   if ( gpc_clip_poly.num_contours > 0 ) { 
		
		for ( kk = 0; kk < gpc_clip_poly.num_contours; kk++ ) {
	                
		    area = af_gpcPolyArea( &gpc_clip_poly.contour[kk], sys_N );

		    addFmt = False;
		    if ( area >= AREA_LIMIT ) {
						            
/*
 *  Check if this part is eligible to be added in
 *  For system-generated smears, it must intersect at least one
 *  snapshot with an area >= 3K.
 */
			if ( nsnap <= 0 ) {
		            addFmt = True;
			}
			else {
			    for ( nn = 0; nn < nsnap; nn++ ) {
			   	af_elm2poly ( *snaps[nn], &poly_test, &ier );   
				gpc_diff_poly.contour = &gpc_clip_poly.contour[kk];
				    
				gpc_polygon_clip ( GPC_INT, &poly_test, &gpc_diff_poly,
                                      		   &gpc_poly_1 );

				snaparea = 0.0F;
				nfmt = gpc_poly_1.num_contours;
				if ( nfmt > 0 ) {
				    for ( ns = 0; ns < nfmt; ns++ ) {
				        if ( gpc_poly_1.hole[ ns ] == G_FALSE ) {
                                            snaparea += af_gpcPolyArea( &gpc_poly_1.contour[ns], sys_N);
					}
				    }				    
				}
				
				if ( snaparea >= AREA_LIMIT )  {
				    addFmt = True;
				}
								        
				gpc_free_polygon ( &gpc_poly_1 );
                                gpc_free_polygon ( &poly_test );        
				    
			        if ( addFmt ) break;
			    }
			}

/*
 *  For airmet "6-6", it must intersect snapshot "6" with an area >= 3K
 *  to be qualified as a valid format structure.
 */
			if ( check6_6 && nsnap > 0 && addFmt ) {
			    
			    snaparea = 0.0F;
			    for ( nn = 0; nn < nsnap; nn++ ) {
			   	cvg_getFld( snaps[nn], TAG_GFA_FCSTHR, fcstHr, &ier );
                                
				if ( strcmp ( fcstHr, "6" ) == 0 ) {
				    af_elm2poly ( *snaps[nn], &poly_test, &ier );   
				    gpc_diff_poly.contour = &gpc_clip_poly.contour[kk];
				    
				    gpc_polygon_clip ( GPC_INT, &poly_test, &gpc_diff_poly,
                                      		       &gpc_poly_1 );
				    
				    nfmt = gpc_poly_1.num_contours;
				    if ( nfmt > 0 ) {
				        for ( ns = 0; ns < nfmt; ns++ ) {
				            if ( gpc_poly_1.hole[ ns ] == G_FALSE ) {
                                                snaparea += af_gpcPolyArea( &gpc_poly_1.contour[ns], sys_N);
					    }
				        }
				    }				    
				}
												        
				gpc_free_polygon ( &gpc_poly_1 );
                                gpc_free_polygon ( &poly_test );                                    			       
			    }
			    
		            if ( snaparea < AREA_LIMIT )  {
				addFmt = False;
		            }
			}
		            
/*
 *  Increase the size of the format array by 1 and add the part in.
 */
			if ( addFmt ) {
	                        
		            if ( fmt == NULL ) {
	                        G_MALLOC ( (*fmt), GFA_Elem_Format, one, "af_FROMLineRules: fmt" );
	                    }
	                    else {
		                G_REALLOC ( (*fmt), GFA_Elem_Format, (*numFmt+1), "af_FROMLineRules: fmt" );
			    }
                                							    
/*
 *  GPC clipping may occasionally results in point sequence
 *  ABBA.  Compress such sequence to a single point A.
 */			     
			    af_cleanContour ( gpc_clip_poly.contour[kk], &contour_tmp0, &ier );      			    
			    
			    if ( ier < 0 ) continue;
			    
/*
 *  Replace the intersection points with the boundaries with their
 *  matched snap points to make bounds between parts intact.
 */			     			    			    
			    af_replaceIntPt ( contour_tmp0, nintl, intl_ipx,  
				              intl_ipy, intl_sipx, intl_sipy,
					      &contour_tmp1, &ier );

			    af_replaceIntPt ( contour_tmp1, nip, ipx, ipy, 
				              sipx, sipy, &contour_tmp2, &ier ); 					      
/*
 *  Put the polygon into a new format structure - any duplicate points
 *  will be removed and counted as one single point.
 */
			    af_poly2fmt ( contour_tmp2, *el_in,
                                          _FA_Region[jj][0], &(*fmt)[ *numFmt ], &ier );

/*
 *  Mark intersection points and boundary points as non-reduceable,
 *  as well as the points immediately before/after such a
 *  non-reduceable points. 
 */
			    af_addIntFlg4Intl ( &(*fmt)[ *numFmt ], nintl, intl_ipx,  
				                intl_ipy, intl_sipx, intl_sipy, &ier );
				
			    af_addIntFlg ( &(*fmt)[ *numFmt ], nip, ipx, ipy, sipx, sipy, &ier );
                                
			    (*numFmt)++;
                                
			    free ( contour_tmp0.vertex );
			    free ( contour_tmp1.vertex );
			    free ( contour_tmp2.vertex );
		            
			}
		    } 	/* End of inner "if ( area >= AREA_LIMIT )  */					                    
		}	/* End of kk loop  */
	    }		/* End of "if ( gpc_clip_poly.num_contours > 0 ) " */            
	}		/* End of outer "if ( area >= AREA_LIMIT )  */	    	    	    	    
        
	gpc_free_polygon ( &gpc_poly_0 );
	gpc_free_polygon ( &gpc_clip_poly );	
    }				/* End of jj (region) loop */        
}

/*=====================================================================*/

static void af_addIntFlg ( GFA_Elem_Format *fmt, int nip, float *ipx,
		         float *ipy, float *sipx, float *sipy, 
			 int *iret )
/************************************************************************
 * af_addIntFlg                                                     	*
 *                                                                      *
 * This function flags the intersection points of the original GFA with	*
 * the region boundary as non-reduceable. So do the boundary points.	*
 * The points immediately before and after an intersection point or a	*
 * boundary points are also flagged as non-reduceable.  This strategy	*
 * helps to preserve the integrity of the boundary after clipping.						*
 *                                                                      *
 * static void af_addIntFlg ( fmt, nip, ipx, ipy, sipx, sipy, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      nip	int         	Total number of common intersection pt 	*
 *      *ipx	float         	X-coords of intersection pt		*
 *      *ipy	float         	Y-coords of intersection pt 		*
 *      *sipx	float         	snapped matches of ipx outside of GFA 	*
 *      *sipy	float         	snapped matches of ipy outside of GFA	*
 *                                                                      *
 * Input/Output parameters:                                          	*
 *      *fmt	GFA_Elem_Format	GFA format structure			*
 *                                                                      *
 * Output parameters:                                         		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/06   Created                   		*
 * J. Wu/SAIC		07/06   Set all original GFA points reduce-able	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06   move the initialization to af_poly2fmt  *
 * J. Wu/SAIC		03/07   Simplify the function                   *
 * D.W.Plummer/NCEP	03/07	Remove setting reduceFlg to G_FALSE for	*
 * 				adjacent points (done in cgr_reducepts)	*
 ***********************************************************************/
{
    int		np, ii, jj;
    float	elat, elon;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
/*
 *  If no regional clipping (international clipping is always on), 
 *  no need to proceed.
 */		    	
    if ( (_clippingFlg == False)  ) {
        return;
    }
    
/*
 *  If a point is a boundary point, it cannot be reduced, as well as
 *  the points immediately before and after it.  
 */		    	                    
    np = fmt->el.elem.gfa.info.npts;
    if ( _reduPtsFlg != 0 ) {
            	    	
	for ( ii = 0; ii < np; ii++ ) {            
                	    
	    elat = fmt->el.elem.gfa.latlon[ ii ];
	    elon =  fmt->el.elem.gfa.latlon[ ii+np ]  ; 
	    
	    if ( fmt->region == _FA_Region[0][0] ) {   
		for ( jj = 0; jj < _nwc; jj++ ) {
	            if ( G_DIST( elat, elon, _wcX[ jj ], _wcY[ jj ] ) < SMALLF ) {
                        fmt->reduceFlg[ ii ] = G_FALSE;
	            }
	        }
            }
	    else if ( fmt->region == _FA_Region[2][0] ) { 		    
		for ( jj = 0; jj < _nec; jj++ ) {
	            if ( G_DIST( elat, elon, _ecX[ jj ], _ecY[ jj ] ) < SMALLF ) {
                        fmt->reduceFlg[ ii ] = G_FALSE;
	            }
	        }
	    }
            else {  
		for ( jj = 0; jj < _nwc; jj++ ) {
	            if ( G_DIST( elat, elon, _wcX[ jj ], _wcY[ jj ] ) < SMALLF ) {
                        fmt->reduceFlg[ ii ] = G_FALSE;
	            }
	        }
		    
		for ( jj = 0; jj < _nec; jj++ ) {
	            if ( G_DIST( elat, elon, _ecX[ jj ], _ecY[ jj ] ) < SMALLF ) {
                        fmt->reduceFlg[ ii ] = G_FALSE;
	            }
	        }				
	    }
	} 
    }
	
/*
 *  Flag the intersection points as non-reduceable, as well as the points 
 *  immediately before and after it.
 */    
    for ( ii = 0; ii < np; ii++ ) {            
	
	elat = fmt->el.elem.gfa.latlon[ ii ];
	elon =  fmt->el.elem.gfa.latlon[ ii+np ]  ; 
	
	for ( jj = 0; jj < nip; jj++ ) {
	    
	    if ( G_DIST( elat, elon, sipx[ jj ], sipy[ jj ] ) < SMALLF ) {
				    				     
		if ( _reduPtsFlg != 0 ) {
                    fmt->reduceFlg[ ii ] = G_FALSE;
		}
	    }
	}
    } 
}

/*=====================================================================*/

static void af_getGFAInt ( VG_DBStruct el, int *nip,  float *ipx, 
		float *ipy, float *sipx, float *sipy, int *iret )
/************************************************************************
 * af_getGFAInt                                                  	*
 *                                                                      *
 * This routine finds the intersection points of a GFA with the common	*
 * border of WEST and CENTRAL region, as well as those with the common 	*
 * border of EAST and CENTRAL region.  The points are then inserted into*
 * the GFA polygon and snapped individually to points outside of the	*
 * the GFA polygon, which will not within the clustering distance of the*
 * the points immediately before and after it.  Both the intersection	*
 * points and their snapped matches are returned in map coordinate.	*
 *                                                                      *
 * static void af_getGFAInt ( el, nip, ipx, ipy, sipx, sipy, iret )	*
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
 * J. Wu/SAIC		02/06   	Created                         *
 * J. Wu/SAIC		02/06   	Make polygon close		*
 * D.W.Plummer/NCEP	4/06	Chg calling seq of clo_snapPt		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * D.W.Plummer/NCEP	09/06	Add tolerance parm to clo_snapPt	*
 * J. Wu/SAIC		03/07   Prevent adding clustering point		*
 * J. Wu/SAIC		05/07   Add more for clustering processing	*
 * J. Wu/SAIC		01/08   Correct longtitude from ipx to ipy	*
 * J. Wu/SAIC		01/08   Try best to use the snappped point	*
 ***********************************************************************/
{
    int 	ii, jj, kk, maxp, np, nout[3], ier, kk2, addOne; 
    int		one = 1, ier1, ier2, qmatch, nchk, *inout;
    int 	*bp1[3], *ap1, *bp2, *ap2, *gipB, npp; 
    int 	closed, rol, n4snap; 
    float 	*elat, *elon, *xout[3], *yout[3], slat, slon;
    float	qdist, qdist1, qdist2, *chkLat, *chkLon, tol;
#define	LLSCAL	100
/*--------------------------------------------------------------------*/
                
    *iret = 0;
    *nip = 0;
    
/*
 *  Allocate space for working arrays.
 */
    maxp = MAX_BOUND_PT; 
    for ( ii = 0; ii < 3; ii++ ) {
        G_MALLOC ( xout[ii], float, maxp, "af_getGFAInt: xout" );
        G_MALLOC ( yout[ii], float, maxp, "af_getGFAInt: yout" );
        G_MALLOC ( bp1[ii], int, maxp, "af_getGFAInt: bp1" );
    }
        
    G_MALLOC ( ap1, int, maxp, "af_getGFAInt: bp1" );
    G_MALLOC ( bp2, int, maxp, "af_getGFAInt: bp2" );
    G_MALLOC ( ap2, int, maxp, "af_getGFAInt: ap2" );
    G_MALLOC ( gipB, int, maxp, "af_getGFAInt: gipB" );
    
    G_MALLOC ( inout, int, _centP, "af_getGFAInt: inout" );
    G_MALLOC ( chkLat, float, _centP, "af_getGFAInt: chkLat" );
    G_MALLOC ( chkLon, float, _centP, "af_getGFAInt: chkLon" );
    
/*
 *  GFA polygon should be additional space for cgr_intersect.
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
 *  First find intersection points of el with central region.
 */   
    cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_centP, _centX,
                    _centY, &maxp, sys_M, &nout[1], xout[1], yout[1], 
		    bp1[1], ap1, bp2, ap2, &ier );
    
    if ( nout[1] > 0 ) {
	
        for ( ii = 0; ii < nout[1]; ii++ ) {
            xout[1][ ii ] = rint ( (double)xout[1][ ii ] * LLSCAL ) / LLSCAL; 
            yout[1][ ii ] = rint ( (double)yout[1][ ii ] * LLSCAL ) / LLSCAL;
        }
	
/*
 *  Find intersection points of el with the common part of
 *  central and west regions.
 */
	cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_westP, _westX,
                    _westY, &maxp, sys_M, &nout[0], xout[0], yout[0], 
		    bp1[0], ap1, bp2, ap2, &ier );
        	    	 	
        *nip = 0;
	if ( nout[ 0 ] > 0 ) {
            for ( ii = 0; ii < nout[0]; ii++ ) {
                xout[0][ ii ] = rint ( (double)xout[0][ ii ] * LLSCAL ) / LLSCAL; 
                yout[0][ ii ] = rint ( (double)yout[0][ ii ] * LLSCAL ) / LLSCAL;
            }
	    
	    for ( jj = 0; jj < nout[ 0 ]; jj++ ) {
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
        
/*
 *   Find intersection points of el with the common part of
 *   central and east regions
 */
        if ( (*nip) !=  nout[ 1 ] ) {
	    cgr_intersect ( sys_M, &np, elat, elon, sys_M, &_eastP, _eastX,
                    _eastY, &maxp, sys_M, &nout[2], xout[2], yout[2], 
		    bp1[2], ap1, bp2, ap2, &ier );
            
	    if ( nout[ 2 ] > 0 ) {		
                for ( ii = 0; ii < nout[2]; ii++ ) {
                    xout[2][ ii ] = rint ( (double)xout[2][ ii ] * LLSCAL ) / LLSCAL; 
                    yout[2][ ii ] = rint ( (double)yout[2][ ii ] * LLSCAL ) / LLSCAL;
                }
		
	        for ( jj = 0; jj < nout[ 2 ]; jj++ ) {
	            for ( kk = 0; kk < nout[ 1 ]; kk++ ) {
	                if ( G_DIST( xout[2][jj], yout[2][jj],
		                 xout[1][kk], yout[1][kk] ) < SMALLF ) {
		            ipx [ *nip ] = xout[2][jj];
		            ipy [ *nip ] = yout[2][jj];
			    gipB[ *nip ] = bp1[2][jj];
		        
			    (*nip)++;
		        }
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
 *  Find the Central boundary points inside the polygon to ensure
 *  the new snap points will not cluster with these points.
 */
        closed = 1;
        tol = 0.001F;
        nchk = 0;
	for ( ii = 0; ii < _centP; ii++ ) {

	    cgr_qrol ( &np, elon, elat, 
	           &closed, &(_centY[ii]), &(_centX[ii]), &tol, &rol, &ier );
	    	    
	    if ( rol > 0 ) {
	        chkLat[ nchk ] = _centX[ ii ];    
	        chkLon[ nchk ] = _centY[ ii ];    
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
 *     and match the intersection point and Pn to the new point. 
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
	    
	    
	    clo_snapPtGFA ( kk, elat[ kk ], elon[ kk ], kk2,
	                    nchk, chkLat, chkLon, n4snap, elat, elon,
			    True, True, 3.0F, &slat, &slon, &ier );
                      
	    if ( ier != 0 ) {
	        
		if ( addOne >= 0 ) {
		    slat = elat[ kk ];
	            slon = elon[ kk ];	    		
		}
		else {		
		    slat = ipx[ ii ];
	            slon = ipy[ ii ];	    
	        }
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
    
    *nip = npp;
    
/*
 *   Clean up.
 */
    for ( ii = 0; ii < 3; ii++ ) {
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


static void af_getIntlPt ( VG_DBStruct *el, int *nip,  float *xip, 
		float *yip, float *sxip, float *syip, int *iret )
/************************************************************************
 * af_getIntlPt                                                  	*
 *                                                                      *
 * This routine clips a GFA polygon against the international bound and *
 * finds the intersection points of the GFA polygon with the bound. 	*
 * Then the intersection points are snapped individually to points  	*
 * outside of the Clipped GFA polygon.  Both the intersection points and*
 * their snapped matches are returned in map coordinate.		*
 *                                                                      *
 * static void af_getIntlPt ( el, nip, xip, yip, sxip, syip, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *el	VG_DBStruct	GFA element to be clipped	      	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nip	int         	Total number of intersection points 	*
 *      *xip	float         	X-coords of intersection points		*
 *      *yip	float         	Y-coords of intersection points 	*
 *      *sxip	float         	snapped matches of xip 			*
 *      *syip	float         	snapped matches of yip			*
 *      *iret	int             Return code                    		*
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/06   	Created                         	*
 * J. Wu/SAIC          03/07   	Add de-clustering capability         	*
 * B. Yin/SAIC	       11/07	initialize polygons			*
 * J. Wu/SAIC          01/08   	snap individually if fail to snap points*
 *                              as a cluster        			*
 ***********************************************************************/
{
    int 	ii, jj, kk, np, np1, np2, ier;
    int		*ptFlag, snap_indx1, snap_indx2, *roindex; 
    int		nextPt, closed = G_FALSE, rol, two = 2, eflag; 
    float 	*elat, *elon, *xtmp, *ytmp, slat, slon, tlat, tlon;        
    float 	*elat1, *elon1, *xtmp1, *ytmp1, *tmplat, *tmplon;
    float	segX[2], segY[2], tol;
    Boolean    done;        
    
    gpc_polygon	poly_el, poly_intl;

#define	NEW_POINT	2
#define	BND_POINT	1
#define	ORG_POINT	0
/*--------------------------------------------------------------------*/
                
    *iret = 0;
    *nip = 0;
    
/*
 *  Initialize polygons
 */
    poly_el.num_contours 	= 0;
    poly_el.hole		= NULL;
    poly_el.contour		= NULL;

    poly_intl.num_contours 	= 0;
    poly_intl.hole		= NULL;
    poly_intl.contour		= NULL;
	                   
/*
 *   Do international clipping.  If no intersection, we are done.
 */
    af_elm2poly ( *el, &poly_el, &ier );
    gpc_polygon_clip ( GPC_INT, &poly_el, &_intlBndPoly, &poly_intl );

    if ( !poly_intl.contour ) {
        return;
    } 
    
/*
 *   Allocate working space.
 */
    np = poly_intl.contour[ 0 ].num_vertices;
    np2 = _intlBndPoly.contour[ 0 ].num_vertices;

    G_MALLOC ( ptFlag, int, np, "af_getIntlPt: ptFlag" );
    
    G_MALLOC ( xtmp, float, np, "af_getIntlPt: xtmp" );
    G_MALLOC ( ytmp, float, np, "af_getIntlPt: xtmp" );
    G_MALLOC ( tmplat, float, np, "af_getIntlPt: tmplat" );
    G_MALLOC ( tmplon, float, np, "af_getIntlPt: tmplon" );
    G_MALLOC ( elat, float, np + 1, "af_getIntlPt: elat" );
    G_MALLOC ( elon, float, np + 1, "af_getIntlPt: elon" );
    
    G_MALLOC ( xtmp1, float, np2, "af_getIntlPt: xtmp" );
    G_MALLOC ( ytmp1, float, np2, "af_getIntlPt: xtmp" );
    G_MALLOC ( elat1, float, np2 + 1, "af_getIntlPt: elat" );
    G_MALLOC ( elon1, float, np2 + 1, "af_getIntlPt: elon" );

    G_MALLOC ( roindex, int, np, "af_getIntlPt:  roindex" );

/*
 *   Remove any duplicate points in the resulting polygon,  convert the 
 *   polygon into map coordinates and then reorder it colckwise.
 */    
    af_getPolyVerts ( &poly_intl.contour[ 0 ], &np, xtmp, ytmp, &ier );

    gtrans ( sys_N, sys_M, &np, xtmp, ytmp, tmplat, tmplon, 
	     &ier, strlen(sys_N), strlen(sys_M) );	
                  
    cgr_reorder ( &np, tmplat, tmplon, roindex, &ier );
    for ( ii = 0; ii < np; ii++ ) {
        elat[ ii ] = tmplat[ roindex[ii] ];
	elon[ ii ] = tmplon[ roindex[ii] ];
    }

/*
 *   Retrieve the international boundary points in map coordinate. 
 */
    gpc_gvlist ( &_intlBndPoly.contour[ 0 ], &np2, xtmp1, ytmp1, &ier );
   
    gtrans ( sys_N, sys_M, &np2, xtmp1, ytmp1, elat1, elon1, 
	     &ier, strlen(sys_N), strlen(sys_M) );	        
       
/*
 *   Check out each point as either a new point, an international boundary point,
 *   or a point of the original polygon.
 */    
    np1 = el->elem.gfa.info.npts;
    int hasOrigPt = 0;
    for ( ii = 0; ii < np; ii++ ) {
	
	ptFlag[ ii ] = NEW_POINT;
	
/*
 *  Check if it is a point of the international boundary.
 */
	for ( jj = 0; jj < np2; jj++ ) {
	    if ( G_DIST( elat [ ii ], elon [ ii ], 
		         elat1[ jj ], elon1[ jj ] ) < SMALLF ) {
		ptFlag[ ii ] = BND_POINT;
		continue;                    
	    }
	}
	
/*
 *  Check if it is a point of the orginal polygon.
 */
	for ( jj = 0; jj < np1; jj++ ) {
	    if ( G_DIST( elat[ ii ], elon[ ii ], el->elem.gfa.latlon[ jj ],
		         el->elem.gfa.latlon[ jj + np1 ] ) < SMALLF ) {
		ptFlag[ ii ] = ORG_POINT;
                hasOrigPt = 1;
		continue;                    
	    }
	}
    }
    
/*
 *   Shift the array to let it start with an original point.
 */    
    if ( hasOrigPt == 1 ) {
        while (  ptFlag[ 0 ] != ORG_POINT ) {            
	    tlat = elat[ 0 ];
	    tlon = elon[ 0 ];
	    eflag = ptFlag[0];
	
	    memmove ( &(elat[0]), &(elat[1]), (np-1)*sizeof(float) );
	    memmove ( &(elon[0]), &(elon[1]), (np-1)*sizeof(float) );
	    memmove ( &(ptFlag[0]), &(ptFlag[1]), (np-1)*sizeof(int) );
	
	    elat[np-1]   = tlat;
	    elon[np-1]   = tlon;        
	    ptFlag[np-1] = eflag;        
        }	
    }
    
/*
 *   Sanp and match any new points.
 */    
    tol = 0.001F;
    for ( ii = 0; ii < np; ii++ ) {
        
	if ( ptFlag[ ii ] != NEW_POINT ) {
	    continue;
	}
	    
/*
 *  Special case 1: BNOB (B - Boundary point, N - intersection point
 *                        O - origianl point)
 *  Rule:  If NO is within the clustering distance and N is on the segment 
 *         B to B, simple match N and O with the boundary point before N.  
 *         So later all three points BNO will be compressed to one single point
 *         and results in a simple BB sequence.
 *  
 */
	nextPt = (ii + 1 + np)%np;	    
	if ( np > 3 && ptFlag[ nextPt ] == ORG_POINT && 
	     clo_isCluster( &elat[ii], &elon[ii], &elat[nextPt],
	                    &elon[nextPt], _clusterDst ) ) {	        
	    segX[0] = elon[(ii-1+np)%np];
	    segX[1] = elon[(ii+2+np)%np];
	    segY[0] = elat[(ii-1+np)%np];
	    segY[1] = elat[(ii+2+np)%np];
		
            cgr_qrol ( &two, segX, segY, &closed, 
		       &elon[ii], &elat[ii], &tol, &rol, &ier );
	    	
	    if ( rol == 0 ) {
	        xip[ *nip ]    =  elat[ ii ];
	        yip[ *nip ]    =  elon[ ii ];
	        xip[ *nip+1 ]  =  elat[ nextPt ];
	        yip[ *nip+1 ]  =  elon[ nextPt ];
	       
	        sxip[ *nip ]   = elat[(ii-1+np)%np];     
	        syip[ *nip ]   = elon[(ii-1+np)%np];     		    	    
	        sxip[ *nip+1 ] = elat[(ii-1+np)%np];     
	        syip[ *nip+1 ] = elon[(ii-1+np)%np];     		    	    

	        (*nip) += 2;
		    
	        continue;
            }
	}
	    
/*
 *  Special case 2: NBN ( B - Boundary point, N - intersection point
 *                        O - origianl point)
 *  Rule:  Do not do such clipping - match all three points with the
 *         point on the original polygon that cause such point sequence
 *         in the clipped result.
 */
	done = False;
	
        if ( ptFlag[ (ii + 1 + np)%np ] == BND_POINT &&
	     ptFlag[ (ii + 2 + np)%np ] == NEW_POINT  ) {
	    
/*
 *  Figure out the appropriate point of the orginal polygon.
 */
	    for ( kk = 0; kk < np1; kk++ ) {
	             
	        segX[0] = el->elem.gfa.latlon[ kk + np1 ];
	        segY[0] = el->elem.gfa.latlon[ kk ];
	        
		segX[1] = el->elem.gfa.latlon[ (kk + np1 + 1)%np1 + np1];
	        segY[1] = el->elem.gfa.latlon[ (kk + 1)%np1];		
		
		cgr_qrol ( &two, segX, segY, &closed, 
			   &elon[ii], &elat[ii], &tol, &rol, &ier );
	        
		if ( rol == 0 ) {

		    segX[0] = el->elem.gfa.latlon[ (kk + np1 + 1)%np1 + np1];
	            segY[0] = el->elem.gfa.latlon[ (kk + 1)%np1];		
		    	            
		    segX[1] = el->elem.gfa.latlon[ (kk + np1 + 2)%np1 + np1];
	            segY[1] = el->elem.gfa.latlon[ (kk + 2)%np1];
		
		    cgr_qrol ( &two, segX, segY, &closed, 
			       &elon[(ii + 2 + np)%np], &elat[(ii + 2 + np)%np],
			       &tol, &rol, &ier );
		    if ( rol == 0 ) {
	                
			xip[ *nip ]  =  elat[ ii ];
	                yip[ *nip ]  =  elon[ ii ];
	                xip[ *nip+1 ]  =  elat[(ii+1+np)%np ];
	                yip[ *nip+1 ]  =  elon[(ii+1+np)%np ];
	                xip[ *nip+2 ]  =  elat[(ii+2+np)%np ];
	                yip[ *nip+2 ]  =  elon[(ii+2+np)%np ];
	       
	                sxip[ *nip ] = el->elem.gfa.latlon[ (kk + 1)%np1]; 
	                syip[ *nip ] = el->elem.gfa.latlon[ (kk + np1 + 1)%np1 + np1];
	                sxip[ *nip+1 ] = el->elem.gfa.latlon[ (kk + 1)%np1];
	                syip[ *nip+1 ] = el->elem.gfa.latlon[ (kk + np1 + 1)%np1 + np1];
	                sxip[ *nip+2 ] = el->elem.gfa.latlon[ (kk + 1)%np1 ];     
	                syip[ *nip+2 ] = el->elem.gfa.latlon[ (kk + np1 + 1)%np1 + np1];

	                (*nip) += 3;
		        ii += 2;			   
		       
		        done = True;
			   
		        break;
			        
	            }		        
		}
	    }
        }
        
	
	if ( done )  {
	    continue;
        }
	
/*
 *  All other cases - snap the intersection point to a point with a 
 *  clustering distance away and outside the polygon.  For clustering 
 *  points, snap them to a single snap point.  If fail to snap a cluster
 *  of points, try to snap them individually.
 *  
 */
 	snap_indx1 = ii;
	snap_indx2 = ii;
	while ( snap_indx1 > 0 && 
	        clo_isCluster( &elat[ii], &elon[ii], &elat[snap_indx1-1],
	                       &elon[snap_indx1-1], _clusterDst ) ) {
                
            snap_indx1--;
	}
	    
	while ( snap_indx2 < (np-1) && 
	       clo_isCluster( &elat[ii], &elon[ii], &elat[snap_indx2+1],
	                      &elon[snap_indx2+1], _clusterDst ) ) {
                
	    snap_indx2++;
	}
	    	    	    	    	    
	clo_snapPtGFA( snap_indx1, elat[ ii ], elon[ ii ], snap_indx2,
	               0, NULL, NULL, np, elat, elon, True, True, 3.0F, 
		       &slat, &slon, &ier );
	            
	if ( ier != 0 && (snap_indx1 != snap_indx2) ) {
 	    
	    snap_indx1 = ii;
	    snap_indx2 = ii;
	    
	    clo_snapPtGFA( snap_indx1, elat[ ii ], elon[ ii ], snap_indx2,
	               0, NULL, NULL, np, elat, elon, True, True, 3.0F, 
		       &slat, &slon, &ier );
	    
	} 

	if ( ier != 0 ) {
	    slat = elat[ ii ];     
	    slon = elon[ ii ];     		    	    	
        }	    
	
	for ( jj = snap_indx1;  jj <= snap_indx2; jj++ ) {
		xip[ *nip ]  =  elat[ jj ];
	        yip[ *nip ]  =  elon[ jj ];
	       
	        sxip[ *nip ] = slat;     
	        syip[ *nip ] = slon;     		    	    
	       
	        (*nip)++;
	}	
    }
    
/*
 *  Clean up
 */    
    G_FREE ( ptFlag, int );
    G_FREE ( xtmp, float );
    G_FREE ( ytmp, float );
    G_FREE ( tmplat, float );
    G_FREE ( tmplon, float );
    G_FREE ( elat, float );
    G_FREE ( elon, float );
    G_FREE ( xtmp1, float );
    G_FREE ( ytmp1, float );
    G_FREE ( elat1, float );
    G_FREE ( elon1, float );
    G_FREE ( roindex, int );

    gpc_free_polygon ( &poly_el );
    gpc_free_polygon ( &poly_intl );
}

/*=====================================================================*/

static void af_addIntFlg4Intl ( GFA_Elem_Format *fmt, 
                    int intl_np, float *intl_ipx, float *intl_ipy, 
		    float *intl_sipx, float *intl_sipy, int *iret )
/************************************************************************
 * af_addIntFlg4Intl                                                    *
 *                                                                      *
 * This function flags the intersection points of the original GFA with	*
 * the international boundary and the boundary points as non-reduceable.*
 * So do the points immediately before and after such a non-reduceable 	*
 * point.  This strategy helps to preserve the integrity of the boundary*
 * along the international boundary after clipping.			*
 *                                                                      *
 * static void af_addIntFlg4Intl ( fmt, intl_np, intl_ipx, intl_ipy,	*
 *			      intl_sipx, intl_sipy, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      intl_np		int	Total number of insct points with  	*
 *      			international bound 			*
 *      *intl_ipx	float	X-coords of intl intersection pt	*
 *      *intl_ipy	float	Y-coords of intl intersection pt 	*
 *      *intl_sipx	float	snapped matches of intl_ipx 		*
 *      *intl_sipy	float	snapped matches of intl_ipy 		*
 *                                                                      *
 * Input/Output parameters:                                          	*
 *      *fmt	GFA_Elem_Format	GFA format structure			*
 *                                                                      *
 * Output parameters:                                         		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/06   	Created                         *
 * J. Wu/SAIC		03/07      	flags only, no point repalcement*
 * D.W.Plummer/NCEP	03/07	Remove setting reduceFlg to G_FALSE for	*
 * 				adjacent points (done in cgr_reducepts)	*
 ***********************************************************************/
{
    int		np, ier, ii, jj, np2;
    float	*xnormal, *ynormal, elat, elon;
    float	*xtmp1, *ytmp1, *elat1, *elon1;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
  
/*
 *  International clipping is ALWAYS on.  So if a point is an international
 *  boundary point, it cannot be reduced, as well as the points immediately
 *  before and after it.  
 */		    	                    
    if ( _reduPtsFlg != G_FALSE ) {
            
        np2 = _intlBndPoly.contour[ 0 ].num_vertices;
        
	G_MALLOC ( xtmp1, float, np2, "af_addIntFlg4Intl: xtmp1" );
        G_MALLOC ( ytmp1, float, np2, "af_addIntFlg4Intl: xtmp1" );
        G_MALLOC ( elat1, float, np2 + 1, "af_addIntFlg4Intl: elat1" );
        G_MALLOC ( elon1, float, np2 + 1, "af_addIntFlg4Intl: elon1" );
        
	gpc_gvlist ( &_intlBndPoly.contour[ 0 ], &np2, xtmp1, ytmp1, &ier );
   
        gtrans ( sys_N, sys_M, &np2, xtmp1, ytmp1, elat1, elon1, 
	         &ier, strlen(sys_N), strlen(sys_M) );	        
	
	np = fmt->el.elem.gfa.info.npts;
	for ( ii = 0; ii < np; ii++ ) {            
                	    
	    elat = fmt->el.elem.gfa.latlon[ ii ];
	    elon =  fmt->el.elem.gfa.latlon[ ii + np ]  ; 
	    
	    for ( jj = 0; jj < np2; jj++ ) {
		if ( G_DIST( elat, elon, elat1[ jj ], elon1[ jj ] ) < SMALLF ) {
                    fmt->reduceFlg[ ii ] = G_FALSE;
		}
	    }
	}
	
        G_FREE ( xtmp1, float );
        G_FREE ( ytmp1, float );
        G_FREE ( elat1, float );
        G_FREE ( elon1, float );
	 
    }
    
/*
 *  Set the reduction flag of the intersection points against the international
 *  bound to False, as well as the points immediately before and after it.
 */    
    G_MALLOC ( xnormal, float, intl_np, "af_addIntFlg4Intl: xnormal" );
    G_MALLOC ( ynormal, float, intl_np, "af_addIntFlg4Intl: ynormal" );
    
    gtrans ( sys_M, sys_N, &intl_np, intl_sipx, intl_sipy, xnormal, ynormal,
                 &ier, strlen(sys_M), strlen(sys_N) );
    
    np = fmt->el.elem.gfa.info.npts;
    for ( ii = 0; ii < np; ii++ ) {            
	
	elat = fmt->el.elem.gfa.latlon[ ii ];
	elon =  fmt->el.elem.gfa.latlon[ ii+np ]  ; 
	
	for ( jj = 0; jj < intl_np; jj++ ) {
	    
	    if ( G_DIST( elat, elon, intl_sipx[ jj ], intl_sipy[ jj ] ) < SMALLF ) {

		if ( _reduPtsFlg != G_FALSE ) {
		    fmt->reduceFlg[ ii ] = G_FALSE;
		}
	    }
	}
    }        
    
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );
}

/*=====================================================================*/

static void af_cleanContour ( gpc_vertex_list ctr_in, 
		       gpc_vertex_list *ctr_out, int *iret )
/************************************************************************
 * af_cleanContour                                                     	*
 *                                                                      *
 * This function first compresses the consecutive duplicate points to 	*
 * single point and then compress ABA point sequence to a single point A*
 *                                                                      *
 * static void af_cleanContour ( cnt_in, ctr_out, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      ctr_in		gpc_vertex_list	GPC contour			*
 *                                                                      *
 * Input/Output parameters:                                          	*
 *      *ctr_out	gpc_vertex_list	GPC contour			*
 *                                                                      *
 * Output parameters:                                         		*
 *      *iret           int             Return code                     *
 *                                        0: normal return         	*
 *                                       -1: output polygon < 3 points	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/07   Initial coding              		*
 ***********************************************************************/
{
    int		np, ier, ii, jj, done, nshift;
    float	*xn, *yn, *lat, *lon;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;       
    
/*
 *  Get point in input polygon and convert to map coordiates. 
 */        
    np = ctr_in.num_vertices;
    G_MALLOC ( xn, float, np, "af_cleanContour: xn" );
    G_MALLOC ( yn, float, np, "af_cleanContour: yn" );    
    G_MALLOC ( lat, float, np, "af_cleanContour: lat" );
    G_MALLOC ( lon, float, np, "af_cleanContour: lon" );
        
    gpc_gvlist ( &ctr_in, &np, xn, yn, &ier );
       
    gtrans ( sys_N, sys_M, &np, xn, yn, lat, lon,
             &ier, strlen(sys_N), strlen(sys_M) );
	
/*
 *  Compresses consecutive duplicate points to single point. 
 */        
    ii = 0;
    done = G_FALSE;    
    while ( done == G_FALSE )  {

	jj = ii;
	while ( (jj+1) < np &&
	        G_DIST( lat[ii], lon[ii], lat[jj+1], lon[jj+1] ) < SMALLF ) {
	    jj += 1;
	}
	nshift = jj - ii;
    
	if ( nshift != 0 )  { 
	    memmove ( &(lat[ii]), &(lat[ii+nshift]),(np-ii-nshift)*sizeof(float) );
	    memmove ( &(lon[ii]), &(lon[ii+nshift]),(np-ii-nshift)*sizeof(float) );
	    np -= nshift;
	}
	
	ii++;
	
	if ( ii >= np ) done  = G_TRUE; 
    }

/*
 *  Reduce ABA case to A (line starts at A, goes to B and then back to A).
 */        
    ii = 0;
    done = G_FALSE;    
    while ( done == G_FALSE )  {

	if ( (ii+2) < np &&
	     G_DIST( lat[ii], lon[ii], lat[ii+2], lon[ii+2] ) < SMALLF ) {
	    
	    memmove ( &(lat[ii]), &(lat[ii+2]),(np-ii-2)*sizeof(float) );
	    memmove ( &(lon[ii]), &(lon[ii+2]),(np-ii-2)*sizeof(float) );
	    np -= 2;
	}
	
	ii++;
	
	if ( ii >= np ) done  = G_TRUE; 
    }
    
/*
 *  Put points into output contour. 
 */        
    if ( np < 3 ) {
        *iret = -1;    
    }
    
    gtrans ( sys_M, sys_N, &np, lat, lon, xn, yn, 
                 &ier, strlen(sys_M), strlen(sys_N) );
                  
    ctr_out->num_vertices = np;

    G_MALLOC ( ctr_out->vertex, gpc_vertex, np, "gpc_vertex creation" );

    for ( ii = 0; ii < np; ii++ )  {
        ctr_out->vertex[ii].x = xn[ii];
        ctr_out->vertex[ii].y = yn[ii];
    }
        
    G_FREE ( xn, float );
    G_FREE ( yn, float );
    G_FREE ( lat, float );
    G_FREE ( lon, float );
}

/*=====================================================================*/

static Boolean af_testSfc( int ngrp, GFA_SmrOlk_Grp *gfaGrp, int sfcFzl[] )
/************************************************************************
 * af_testSfc	                                                  	*
 *                                                                      *
 * This routine loops through all grouped GFA elements and check if any *
 * surface freezing level crosses over an FA area. If a surfzce freezing*
 * level intersectes an FA area, the flag for the FA area in sfcFzl[]	*
 * is set to 1 and the return value is set to True.			*
 *                                                                      *
 * static Boolean af_testSfc ( ngrp, gfaGrp, sfcFzl )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	ngrp	int		number of grouped GFA structures	*
 *      *gfaGrp	GFA_SmrOlk_Grp	grouped GFA structure array	      	*
 *                                                                      *
 * Output parameters:                                                   *
 *      sfcFzl[] int            array of flags that indicate whether a	*
 *				sfc freezing level intersects an FA area*
 *                                                   			*
 * Return values: 	                                                *
 *                              True: an SFC crosses over any FA areas  *
 *                              False: no SFC cross over any FA areas   *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC	       12/07	created					*
************************************************************************/
{
    int 	iarea, igrp, jj, kk, ier, npoly;
    char 	levelStr[ 32 ];
    float	*xPoly, *yPoly;

    Boolean	intersct, rtv = False;
/*---------------------------------------------------------------------*/

    /*
     * Initialize sfcFzl[].
     */
    for ( iarea = 0; iarea < NUM_FA_AREAS; iarea++ ) {
	sfcFzl[ iarea ] = 0;
    }

    /*
     * Loop through all elements
     */
    for ( igrp = 0; igrp < ngrp; igrp++ ) {

	if ( strcasecmp( gfaGrp[ igrp ].haz_type, "FZLVL" ) == 0 &&
		gfaGrp[ igrp ].smear != NULL ) {

		cvg_getFld( gfaGrp[ igrp ].smear, "Level", levelStr, &ier );

	        /*
	         * Check if a SFC intersects any FA area
		 */
		if ( ier >= 0 && strcasecmp( levelStr, "SFC" ) == 0 ) {

                          for ( iarea = 0; iarea < NUM_FA_AREAS; iarea++ ) {

			      if ( sfcFzl[ iarea ] == 1 ) break;	

			      intersct = False;

        		      for ( jj = 0; jj < _areaBndPoly[ iarea ].num_contours; jj++ ) {

                                  npoly = _areaBndPoly[ iarea ].contour[ jj ].num_vertices + 1;

            			  G_MALLOC ( xPoly, float, npoly,
                       				"AF_ADJFZLVL xPoly" );
            			  G_MALLOC ( yPoly, float, npoly,
                       				"AF_ADJFZLVL yPoly" );
            			  /*
             			   *  Get all points in the contour and close it.
             			   */
            			  for ( kk = 0; kk < _areaBndPoly[ iarea ].contour[ jj ].num_vertices; kk++ ) {
                                                                                                
                			xPoly[ kk ] = _areaBndPoly[ iarea ].contour[ jj ].vertex[ kk ].x;
                			yPoly[ kk ] = _areaBndPoly[ iarea ].contour[ jj ].vertex[ kk ].y;
                                                                                               
            			  }
                                                                                                     
            			  xPoly[ npoly - 1 ] = xPoly[ 0 ];
            			  yPoly[ npoly - 1 ] = yPoly[ 0 ];
				  
              			  cgr_linepolyint( sys_M, gfaGrp[ igrp ].smear->elem.gfa.info.npts,
                      				&(gfaGrp[ igrp ].smear->elem.gfa.latlon[ 0 ]), 
                      				&(gfaGrp[ igrp ].smear->elem.gfa.latlon[ gfaGrp[ igrp ].smear->elem.gfa.info.npts ]), 
                      					sys_N, npoly, xPoly, yPoly, &intersct, &ier );

				  G_FREE ( xPoly, float );
				  G_FREE ( yPoly, float );

		      		  if ( intersct ) {
					
					sfcFzl[ iarea ] = 1;
					rtv = True;
					break;

				  }
			      }
			  }
		}
        }
    }

    return rtv;

}

/*=====================================================================*/

static void af_adjustFzlvl( int nfmt, GFA_Elem_Format **fmt, int sfcFzl[] )
/************************************************************************
 * af_adjustFzlvl                                                  	*
 *                                                                      *
 * This routine sets the lowest freezing level for an FA area to zero   *
 * if the input surface freezing level flag for the area is one.     	*
 *                                                                      *
 * static Boolean af_adjustFzlvl ( nfmt, fmt, sfcFzl )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	nfmt	int		number of format structures		*
 *      sfcFzl[] int            array of flags that indicate whether a	*
 *				sfc freezing level intersects an FA area*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      **fmt	GFA_Elem_Format	GFA format structure array	      	*
 *                                                   			*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC	       12/07	created					*
 * B. Yin/CWS	       12/10	changed how to put all 0s for neg values*
************************************************************************/
{
    int 	ifmt, iarea, nchar, ier;
    char 	ranges[ 256 ], *areaStr, *pt0, *pt1, *pt2;
/*---------------------------------------------------------------------*/
    
    for ( ifmt = 0; ifmt < nfmt; ifmt++ ) {

	cvg_getFld( &((*fmt)[ ifmt ].el), TAG_GFA_FZLRANGE, ranges, &ier );

	if ( ier < 0 || strlen( ranges ) <= 0 ) continue;

        for ( iarea = 0; iarea < NUM_FA_AREAS; iarea++ ) {

	    if ( sfcFzl [ iarea ] == 0 ) continue;
	 
	    areaStr = strstr( ranges, _FA_Area[ iarea ] );  

	    if ( areaStr ) {

		pt0 = strchr( areaStr, ';' );
		if ( pt0 )  pt1 = strchr( pt0 + 1, ';' );

		if ( pt1 ) {

		   pt2 = strchr( pt1 + 1, ';' );
 
		   if ( pt2 ) {

			/*
			 * Lowest level string is between pt1 and pt2
			 */
			nchar = pt2 - pt1 - 1;
			while ( nchar > 0 ) {
			    strncpy( pt1 + nchar, "0", 1 );
			    nchar--;
			}

			cvg_setFld( &((*fmt)[ ifmt ].el), TAG_GFA_FZLRANGE, 
					ranges, &ier );

		   }
		}
	    }
	}
    }
}
