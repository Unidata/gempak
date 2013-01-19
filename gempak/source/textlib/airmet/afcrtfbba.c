#include "afcmn.h"

/*
 *  Private functions
 */
static void af_addFbbaAttr (	int			numFmt, 
				GFA_Elem_Format 	*fmtAry, 
				int 			*iret );


/*
 *  Public functions
 */
void af_crtFBBA ( char *day, char *cycle, int issuance,
		int nin, VG_DBStruct *elIn, 
		int *nout, VG_DBStruct **elOut, int *iret )
/************************************************************************
 * af_crtFBBA                                                     	*
 *                                                                      *
 * This routine generates the F/BB/A elements,	which will be used to	*
 * create text bulletin in the airmet formattrr.  The routine does the	*
 * following in sequence: 						*
 *	1. group smears and snapshots with the hazard type and tag	*
 *	2. clip smears against the international & FA region boundaries *
 *	3. reduce points and clip against area boundaries if needed	*
 *      4. produce conditional wording using snapshots information	*
 *	5. apply the worst attribute rules using snapshots information	*
 *	6. set F/BB/A attributes (state list/area/region/wording)	*
 *	7. copy all valid elements in the format structures to output	*
 *                                                                      *
 * Note: 								*
 *	1. Originally, the clipping was performed in the airmet 	*
 *         formatter via af_create() in the versions before 5.11.1.  	*
 *         Starting from v5.11.1, the geographic processing is moved to *
 *	   the FROM tool to make the FORMATTED FROM line as user 	*
 *         editable VG elements (F/BB/As).				*
 *                                                                      *
 *	2. The subtype will be changed to GFA_FBBA_AIRMET or		*
 *	   GFA_FBBA_OUTLOOK in pgfrom_smearAll() after the call to	*
 *         af_crfFBBA() since such an change requires calls to a few	*
 *         PGEN functions and we do not want to do so in an AF function,*
 *         which is a lower-level library function.			*
 *                                                                      *
 *	3. Memory is located for output VG elements.  It is the caller's*
 *         responsibility to free it. 					*
 *		  							*
 * void af_crtFBBA (day, cycle, issuance, nin, elIn, nout, elOut, iret)	* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*day		char		2 digit day of the month	*
 *      *cycle		char		cycle string			*
 *      issuance	int		issuance type from AIRMET GUI	*
 *      nin		int		number of input elements	*
 *      *elIn		VG_DBStruct     array of GFA elements		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout		int		number of output elements	*
 *      **elOut		VG_DBStruct     array of GFA FBBA elements	*
 *      *iret           int             error code                  	*
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	10/07   Created	                        	*
 * J. Wu/SAIC          	01/08   Generate only one FBBA for formattble   *
 *                              GFAs crossing two FA areas in one region*
***********************************************************************/
{
    int 		ii, jj, ier, ngfagrp, numFmt, one = 1;
    char		tag[ 128 ];
    GFA_Elem_Format	*fmt;
    GFA_SmrOlk_Grp	*gfaGrp;
    double		epsilon;   
/*---------------------------------------------------------------------*/

    *iret = 0;
    *nout = 0;
    
    /*
     * Get GPC_EPSILON setting from prefs.tbl (defaults to 1e10-6).
     */
    ctb_pfstr ( "GFA_AF_EPSILON", tag, &ier );
    if ( ier == 0 ) {
        sscanf ( tag, "%lf", &epsilon );
    }
    else {
        epsilon = (double)GFA_EPSILON;
    }
    
    gpc_set_epsilon ( epsilon );


    /*
     *  Initialize bounds.
     */
    af_initBnds( &ier );

    
    /*
     *  Pre-process the input VG elements into an array of "GFA_SmrOlk_Grp".
     */
    gfaGrp = (GFA_SmrOlk_Grp *)NULL;
    af_elms2grps ( nin, elIn, &ngfagrp, &gfaGrp, &ier );


    /*
     *  Pass grouped GFAs through FROM line rules to generate format structures
     */
    numFmt = 0;
    fmt = (GFA_Elem_Format *)NULL;
    af_fmtFROMLines ( ngfagrp, gfaGrp, _regionBndPoly, _areaBndPoly,
		     &numFmt, &fmt, &ier );

    
    /*
     *  Combine IFR CIG and VIS in the GFA format structures. 
     */
    af_IFRCombineCIGVIS	( &numFmt, &fmt, &ier );


    /*
     *  Pass GFA format structures through area rules to set up area/adjarea info. 
     */
    af_areaRules ( _areaBndPoly, &numFmt, &fmt, &ier );

    
    /*
     * Reduce points to desired threshold set in prefs.tbl.
     * Check the flag before doing the above.
     */
    if ( _reduPtsFlg != 0 )  af_reducePts ( &numFmt, &fmt, &ier );

    
    /*
     * Remove one of the twin format structures that have the "area" as another's
     * "adjarea", and vice versa.  SO only one FBBA will be generated for a
     * formattable GFA that crosses over 2 FA areas in the same FA region.
     */
    for ( ii = 0; ii < numFmt; ii++ ) { 

 	if ( fmt[ ii ].delete == G_FALSE && 
	     fmt[ ii ].twin >= 0 ) {
	    
	    fmt[ fmt[ ii ].twin ].delete = G_TRUE;
        }
    }
            
    
    /*
     *  Pass GFA format structures through condition wording rules. 
     */
    if ( _condWording != 0 ) {        
	af_condsWording ( cycle, day, issuance, numFmt, fmt, &ier );
    }


    /*
     *  Apply selectd snapshots' attributes to clipped airmet/outlook. 
     */
    af_applyAttr ( cycle, numFmt, fmt, &ier );
    
    
    /*
     *  Set wording, area, region, and state list for each format structure.
     */
    af_addFbbaAttr ( numFmt, fmt, &ier );

    
    /*
     *  Generate F/BB/As for the valid GFA format structs. 
     */
    for ( ii = 0; ii < numFmt; ii++ ) { 

 	if ( fmt[ ii ].delete == G_FALSE ) {
	    if ( elOut == NULL ) {
	        G_MALLOC ( (*elOut), VG_DBStruct, one, "af_crtFBBA: elOut" );
	    }
	    else {
		G_REALLOC ( (*elOut), VG_DBStruct, (*nout + 1 ), "af_crtFBBA: elOut" );
            }
		
	    af_copyGFA( &fmt[ ii ].el, &(*elOut)[ *nout ], &ier );
	        
	    if ( ier == 0 ) {
		(*nout)++;
	    }
        }
    }
   
    
    /*
     *  Release bounds. 
     */
    af_freeBnds( &ier );

    
    /*
     *  Free GFA format structures.  The freezing level contours do not have 
     *  the same structures, so they are not included in the free.
     */
    for ( ii = 0; ii < numFmt; ii++ ) {
        if( !fmt[ii].fzlvlContour ) {		
            gpc_free_polygon( fmt[ii].el_poly );	
	    G_FREE ( fmt[ii].el_poly, gpc_polygon );
	    G_FREE ( fmt[ii].reduceFlg, int );
	    G_FREE ( fmt[ii].wording, char );
	}
	for( jj=0; jj<fmt[ii].el.elem.gfa.info.nblocks; jj++ ) {
	    G_FREE ( fmt[ii].el.elem.gfa.info.blockPtr[ jj ], gfaBlock_t );
	}
    }					    
    
    G_FREE ( fmt, GFA_Elem_Format );


    /*
     *  Free GFA group structures.
     */
    for ( ii = 0; ii < ngfagrp; ii++ ) {
	G_FREE ( gfaGrp[ii].snapshots, VG_DBStruct * );
    }					    
    G_FREE ( gfaGrp, GFA_SmrOlk_Grp );        


    /*
     *  Reset GPC_EPSILON to 1e10-2 for other applications.
     */
    gpc_set_epsilon ( (double)OUT_EPSILON );

}

/***********************************************************************/

static void af_addFbbaAttr ( int numFmt, GFA_Elem_Format *fmtAry, 
			    int *iret )
/************************************************************************
 * af_addFBBAAttr                                                     	*
 *                                                                      *
 * This routine sets the beginning/ending wording, area, region, and  	*
 * state list for all VALID GFA format structures.  Invalid structures	*
 * are marked "delete", including those polygons having less than 3 	*
 * points (closed) or 2 points (open), and those having no state list.	*
 * Outlooks for LLWS, M_FZLVLS, and FZLVL are deleted as well.		*
 *                                                                      *
 * static void af_addFbbaAttr ( nin, fmtAry, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *      numFmt		int     	Number of GFA format structures	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *fmtAry		GFA_Elem_Format Array of GFA format structures	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	10/07   Created	                        	*
 * J. Wu/SAIC          	01/08   allow two FA areas for one FBBA       	*
***********************************************************************/
{
    int		ii, npts, ier, gfaSubType, hazTypeI;
    char	beginWording[ 256 ], endWording[ 256 ], value[256];
    char	stateList[ STD_STRLEN ] = "";
    char	hazardType[ STD_STRLEN ] = "", areas[16];
    Boolean	isAirmet, badElm;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Retrieve and save begin/end wordings.
     */
    for ( ii = 0; ii < numFmt; ii++ ) { 

 	if ( fmtAry[ ii ].delete == G_FALSE ) {

            /*
              *  Do some quality checks: Closed GFA should have at least 
	      *  3 points while open GFAs should have at least 2 points.
	      */	    
	    badElm = False;
	    npts = fmtAry[ii].el.elem.gfa.info.npts;
	    if ( npts < 3 ) {
		badElm = True;	    
	        if ( fmtAry[ii].el.hdr.closed == G_FALSE ) {
		    badElm = False;	    		
		}
	    }
	    
	    if ( badElm ) {
	        fmtAry[ ii ].delete = G_TRUE;
		continue;
	    }
            
	    
	    /*
	      *  No outlooks for FZLVL, M_FZLVL, and LLWS.
	      */
	    isAirmet = True;
            cvg_getFld ( &(fmtAry[ ii ].el), TAG_GFA_SUBTYPE, value, &ier );
            gfaSubType = atoi( value ) - atoi( value ) / 10 * 10;
 	    hazTypeI = atoi( value ) / 10;
                                                                                         
            if ( ( gfaSubType == GFA_USER_OUTLOOK ) ||
                 ( gfaSubType == GFA_SYSTEM_OUTLOOK ) ) {
                        
	        isAirmet = False;
                    
		if ( hazTypeI == GFA_HAZARD_FZLVL_SFC || 
		     hazTypeI == GFA_HAZARD_FZLVL     ||
		     hazTypeI == GFA_HAZARD_M_FZLVL   ||			      
		     hazTypeI == GFA_HAZARD_LLWS )  {
			     
		    fmtAry[ ii ].delete = G_TRUE;    
		    continue;
		}			
            }
                                                                                          
	    /*
              *  Get the formatted State list.  If the state list is
              *  empty then skip this hazard altogether.
              */
 	    cvg_getFld( &fmtAry[ii].el, TAG_GFA_AREATYPE, hazardType, &ier );
	    af_TURBRenameHILO ( hazardType );
            af_fmtStateList( hazardType, npts,
                             fmtAry[ii].el.elem.gfa.latlon,
                             &fmtAry[ii].el.elem.gfa.latlon[ npts ],
                             fmtAry[ii].area, fmtAry[ii].adjarea,
                             stateList, &ier );
		
	    if( ier < 0 || strlen( stateList ) <= 0 ) {                		
		fmtAry[ ii ].delete = G_TRUE;    
		continue;
            }
	    
	    cvg_setFld( &fmtAry[ii].el, TAG_GFA_STATESLIST, stateList, &ier );	
	    
	    
	    /*
	      *  Parse and set the beginning/ending wording.
	      */
	    af_parseWording( isAirmet, fmtAry[ii].wording, beginWording,
			     endWording, &ier );

            if ( strlen( beginWording ) != 0 ) {
		cvg_setFld( &fmtAry[ii].el, TAG_GFA_CONDSBEGIN, beginWording, &ier );
            }

            if ( strlen( endWording ) != 0 ) {
		cvg_setFld( &fmtAry[ii].el, TAG_GFA_CONDSEND, endWording, &ier );
	    }
	    
	    
	    /*
	      *   Set area and region.  One FBBA may have two FA areas
	      *   separating by "-" or "."	  
	      */
	    strcpy( areas, fmtAry[ ii ].area );
	    if ( fmtAry[ ii ].twin >= 0 ) {
	        strcat( areas, "-" );
	        strcat( areas, fmtAry[ ii ].adjarea );
	    }
	    
	    cvg_setFld( &fmtAry[ii].el, TAG_GFA_AREAS, areas, &ier );
	    
	    if ( fmtAry[ ii ].region == 'W' || fmtAry[ ii ].region == 'w' ) {
		cvg_setFld( &fmtAry[ii].el, TAG_GFA_REGION, "W", &ier );
            }
	    else if ( fmtAry[ ii ].region == 'C' || fmtAry[ ii ].region == 'c' ) {
		cvg_setFld( &fmtAry[ii].el, TAG_GFA_REGION, "C", &ier );
	    }
	    else {
		cvg_setFld( &fmtAry[ii].el, TAG_GFA_REGION, "E", &ier );
	    }	    	    
	}
    }        
}

/***********************************************************************/
