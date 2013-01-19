#include "afcmn.h"


/************************************************************************
 * afarearules.c                                             		*
 *                                                                      *
 * This module contains the subroutines which apply the area rules      *
 * checks to GFA elements in order to determine to which area the GFA   *
 * blongs.								*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *      af_areaRules		- check size of GFA elements          	*
 ***********************************************************************/



void af_areaRules ( gpc_polygon	*bnds, int *nin, 	
		          GFA_Elem_Format **fmt_in, int *iret )
/************************************************************************
 * af_areaRules                                                     	*
 *                                                                      *
 * This routine checks the size of the input GFA elements within FA area*
 * boundaries to determine the primary area and the possible adjacent	*
 * area they belong to. Additional GFA format structure are created if	*
 * needed.	                					*
 *                                                                      *
 * Note: "fmt_in" is reallocated in this routine and must be freed by	*
 *	 the caller.                                               	*
 *                                                                      *
 * void af_areaRules ( bnds, nin, fmt_in, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *bnds		gpc_polygon     Array of area bounds polygon	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *numFmt		int     	number of GFA format structures	*
 *      *fmt_in		GFA_Elem_Format Array of GFA format structures	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05   	Created                         	*
 * J. Wu/SAIC          10/05   	Freed gpc_poly_tmp properly		*
 * J. Wu/SAIC          10/05    skip the deleted structures   		*
 * J. Wu/SAIC          10/05   	add safety check			*
 * J. Wu/SAIC          12/05   	change para in af_poly2fmt		*
 * B. Yin/SAIC	       01/06	skip if the GFA is an open line		*
 * J. Wu/SAIC          02/06    Add new point reduction & snap      	*
 * 				intersection points with boundary      	*
 * J. Wu/SAIC          03/06   	Copy "origInfo" for new elements	*
 * M. Li/SAIC	       06/06	Add check for intersection w/ snapshots *
 * M. Li/SAIC	       07/06    Logic change for FA area intersection	*	
 * J. Wu/SAIC          07/06   	fix hand-drawn smears w/o snapshots	*
 * E. Safford/SAIC     08/06	don't process any fzlvl contours	*
 * E. Safford/SAIC     08/06	moved from afcreate.c              	*
 * B. Yin/SAIC	       10/06	remove canceled snapshots.		*
 * J. Wu/SAIC          10/06   	record the position of new structure	*
 * E. Safford/SAIC     12/06	param change to af_useSnapshots()	*
 * J. Wu/SAIC          12/06   	add a fmt struct only if it intesects	*
 *				a snapshot with an area >= 3K sq nm	*
 ***********************************************************************/
{
    int 	ii, jj, kk, ier, areaIdx, total, np, nn, ncon, ns;
    int		nNoCancel, nCancel, smearFlag;
    char	subtype[ 32 ];
    float	tmpArea, clipArea[2], maxArea[2];
    Boolean	intersct[2];

    gpc_polygon	gpc_poly_tmp[2], poly_test, gpc_poly_out;
    VG_DBStruct **elNoCancel, **elCancel;
/*--------------------------------------------------------------------*/

    *iret = 0;
                    
    /*
     * Apply the area rules: create AIRMET for the primary area - the one 
     * either with the larger size or the one with size > 3000.  When both 
     * areas > 3000, both areas are considered as primary areas and one 
     * additional AIRMET should be created.. 
     */
    total = *nin;
    for ( ii = 0; ii < *nin; ii++ ) {

        /*
	 *  Initialize.
	 */
	(*fmt_in)[ii].twin = -1;


	/*
	 *  Toss out all freezing level contours.  The area rules are
	 *  applied to them within af_fzlvl2fmt().
	 */
	if( (*fmt_in)[ii].fzlvlContour ) {
	    continue;
	}
	
	
	/*
          * Clip the element's polygon against the two areas withinn 
          * the given FA region. 
          */
	if ( (*fmt_in)[ ii ].region == 'W' ) {
	    areaIdx = 0;
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 0 ], &gpc_poly_tmp[0] );
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 1 ], &gpc_poly_tmp[1] );		
	}
	else if ( (*fmt_in)[ ii ].region == 'C' ) {
	    areaIdx = 1;
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 2 ], &gpc_poly_tmp[0] );
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 3 ], &gpc_poly_tmp[1] );		
	}
	else {
	    areaIdx = 2;
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 4 ], &gpc_poly_tmp[0] );
	    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ ii ].el_poly, 
	                    &bnds[ 5 ], &gpc_poly_tmp[1] );			
	}
		
       	    
        /*
          *  Compute the size of clipped areas.
	  */
	for ( jj = 0; jj < 2; jj++ ) {
            	    
	    clipArea[jj] = 0.0;
	    maxArea [jj] = 0.0; 
	    
	    for ( kk = 0; kk < gpc_poly_tmp[ jj ].num_contours; kk++ ) {
	        tmpArea = af_gpcPolyArea( &gpc_poly_tmp[ jj ].contour[ kk ], 
			                          sys_N );
                maxArea[ jj ] = G_MAX ( tmpArea, maxArea[ jj ] );
	        clipArea[ jj ] += tmpArea;
	    }
	        
	   /*
	     *  Check for intersection.
	     *  
	     *  Note: for user-drawn smears without associated snapshots, 
	     *        the intersection flag should be True.
	     *  
	     */
	    if ( (*fmt_in)[ii].origInfo->nsnapshot <= 0 ) {
	        intersct[jj] = True;
	    }
	    else {

		/* 
		 *  Remove canceled snapshots.
		 */
		cvg_getFld( &(*fmt_in)[ii].el, TAG_GFA_SUBTYPE, subtype, &ier );
		smearFlag = atoi( subtype ) - atoi( subtype ) / 10 * 10;

		af_useSnapshots( (*fmt_in)[ii].origInfo->snapshots,
				 (*fmt_in)[ii].origInfo->nsnapshot,
				 smearFlag, &elNoCancel, &nNoCancel, 
				 &elCancel, &nCancel, &ier );

	        intersct[jj] = False;
	        for ( nn = 0; nn < nNoCancel; nn++ ) {
	    	    af_elm2poly ( *elNoCancel[nn], &poly_test, &ier );
		    gpc_polygon_clip ( GPC_INT, &poly_test, &gpc_poly_tmp[ jj ], &gpc_poly_out );  

		    ncon = gpc_poly_out.num_contours;
		    if ( ncon > 0 ) {
		        for ( ns = 0; ns < ncon; ns++ ) {
			    if ( af_gpcPolyArea( &gpc_poly_out.contour[ns], sys_N ) >= AREA_LIMIT) {
		                intersct[jj] = True;
				break;
			    }
			}
		    }
				    		    
		    gpc_free_polygon ( &gpc_poly_out );
                    gpc_free_polygon ( &poly_test );
		    
		    if ( intersct[jj] ) break;
		}

		G_FREE ( elNoCancel, VG_DBStruct* );
		G_FREE ( elCancel, VG_DBStruct* );

	    }
	        
	    gpc_free_polygon ( &gpc_poly_tmp[ jj ] );
	
	}
        
	
	/*
	  *  Safety check - make sure the clip intercepts with at least one
	  *  area.  This should be true if the af_FROMLineRules() works
	  *  correctly.
	  */
	if ( !(clipArea[0] > 0.0F || clipArea[1] > 0.0F) ) {
	    continue;    
	}
	
	    
	/*
	  *  Set up the area information in the format structure
	  */
	if ( intersct[0] && (maxArea[ 0 ] >= AREA_LIMIT || clipArea[ 0 ] >= AREA_LIMIT) ) {
	    
	    strcpy ( (*fmt_in) [ ii ].area, _FA_Area[ areaIdx*2 ] );
			
	    if ( intersct[1] && ( maxArea[ 1 ] >= AREA_LIMIT || clipArea[ 1 ] >= AREA_LIMIT ) ) {
		
		strcpy ( (*fmt_in)[ ii ].adjarea, _FA_Area[ areaIdx*2 + 1 ] );
		        
		/*
		  *  Create an additional AIRMET.
		  */
		G_REALLOC ( (*fmt_in), GFA_Elem_Format, (total+1), "af_areaRules: fmt_in" );
		    			    
		af_poly2fmt ( *((*fmt_in)[ii].el_poly->contour), (*fmt_in)[ii].el,  
                                (*fmt_in)[ii].region, &(*fmt_in)[total], &ier );
                             
	        strcpy ( (*fmt_in)[total].area, (*fmt_in)[ii].adjarea );
		strcpy ( (*fmt_in)[total].adjarea, (*fmt_in)[ii].area );

		if ( (*fmt_in)[ii].reduceFlg != (int *)NULL ) {
		    np = (*fmt_in)[ii].el_poly->contour[0].num_vertices;
		    G_MALLOC ( (*fmt_in)[total].reduceFlg, int, np, 
		                              "af_areaRules: reduceFgg" );		     
		    for ( kk = 0; kk < np; kk++ ) {
		        (*fmt_in)[total].reduceFlg[kk] =  (*fmt_in)[ii].reduceFlg[kk];
		    }
		}
		
		/*
		   *  Keep the info where the new element comes from
		   */
		if ( (*fmt_in)[ii].origInfo != (GFA_SmrOlk_Grp *)NULL ) {
		    (*fmt_in)[total].origInfo =  (*fmt_in)[ii].origInfo;	    
		}
				
		/*
		   *  Record the indexes of the twins.
		   */
		(*fmt_in)[total].twin = ii;
		(*fmt_in)[ii].twin = total;		 
		
		total++;			    

	    }
	}
	else {
	    if ( intersct[1] && (maxArea[ 1 ] >= AREA_LIMIT || clipArea[ 1 ] >= AREA_LIMIT) ) {
		
		strcpy ( (*fmt_in)[ ii ].area, _FA_Area[ areaIdx*2 + 1 ] );
	    }
	    else {
		if ( clipArea[ 0 ] > clipArea[ 1 ] ) {
		    strcpy ( (*fmt_in)[ ii ].area, _FA_Area[ areaIdx*2 ] );
		}
		else {
		    strcpy ( (*fmt_in)[ ii ].area, _FA_Area[ areaIdx*2 + 1 ] );
		}
	    }
	}	    	    		
    }
    
    *nin = total;        	
}

/*=====================================================================*/
