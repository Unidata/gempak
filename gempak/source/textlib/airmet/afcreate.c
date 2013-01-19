#include "afcmn.h"


void af_create ( int nareas, char areas[][ 8 ],
		int ntypes, char *types[3], char *day,
		char *cycle, int nin, VG_DBStruct *el_in, int issuance,
		char *string[NUM_FA_AREAS][NUM_TYPES], int *iret )
/************************************************************************
 * af_create                                                     	*
 *                                                                      *
 * This routine clips the input GFA elements against FA region and area *
 * boundaries and generates an information string (xml format), which 	*
 * can be used to create bulletin.					*
 *                                                                      *
 * void af_create ( nareas, area, ntypes, type, day, cycle, nin, el_in,	* 
 *		    issuance, string, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	nAreas		int		number of FA areas		*
 *	areas[][8]	char		array of area strings		*
 *	nTypes		int		number of hazard types 		*
 *	*type[3]	char		array of hazard type strings	*
 *	*day		char		2 digit day of the month	*
 *      *cycle		char		cycle string			*
 *      *nin		int		number of input elements	*
 *      *el_in		VG_DBStruct     array of GFA elements		*
 *      issuance	int		issuance type from AIRMET GUI	*
 *                                                                      *
 * Output parameters:                                                   *
 *      **string	char		array of tagged string		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	07/05   Created                         	*
 * J. Wu/SAIC          	08/05   Reduced points to threshold in prefs.tbl*
 * L. Hinson/AWC       	09/05   Propogate day thru af_create            *
 *                                thru af_fmt2xml                       *
 * J. Wu/SAIC          	10/05   Added IFR CIG and VIS combination       *
 * H. Zeng/SAIC	       	10/05	added flags for pt reduction&clipping	*
 * B. Yin/SAIC         	11/05   change GFA category to subtype          *
 * E. Safford/SAIC	12/05	add outlooks to xml stream              *
 * B. Yin/SAIC         	 1/06   handle open line FZLVL      		*
 * J. Wu/SAIC		02/06	Add new point reduction & snap      	*
 * 				intersection points with boundary      	*
 * J. Wu/SAIC          	03/06   introduce snapshot info into fmt struct	*
 * B. Yin/SAIC          03/06   handle no FZLVL in FA area condition    *
 * J. Wu/SAIC          	03/06   add condition wording based on snapshots*
 * M. Li/SAIC           04/06   Added af_reg2intl                       *
 * E. Safford/SAIC	04/06	fix memory leak on blockptr		*
 * E. Safford/SAIC	08/06	avoid free for fzlvl contours		*
 * J. Wu/SAIC          	09/06   apply snapshot attr to airmet/outlook	*
 * J. Wu/SAIC          	09/06   default _condWording to FALSE		*
 * J. Wu/SAIC          	10/06   change point reduction algrithm & load	*
 * 				all preferences from a lib function	*
 * J. Wu/SAIC          	10/06   set GPC_EPSILON	for clipping		*
 * J. Wu/SAIC          	11/06   put epsilon into prefs.tbl		*
 * J. Wu/SAIC          	01/07   load extended FA area bounds		*
 * B. Yin/SAIC		03/07	Add issuance as an input parameter	*
 * B. Yin/SAIC		03/07	Add issuance and day for af_condsWording* 
 * J. Wu/SAIC		05/07   Load common points for extended FA	*
 *				area bounds      			*
 * E. Safford/SAIC	08/07	use af_initBnds and af_freeBnds		*
 * B. Yin/SAIC		09/07	retrieve and save begin/end wordings	* 
***********************************************************************/
{
    int 		ii, jj, ier, ngfagrp, numFmt, gfaSubType;    
    char		tag[ 128 ], tmpStr[ 128 ];
    char		beginWording[ 256 ], endWording[ 256 ];
    GFA_Elem_Format	*fmt;
    GFA_SmrOlk_Grp	*gfaGrp;
    static float        areaLatCentroid[NUM_FA_AREAS];
    static float        areaLonCentroid[NUM_FA_AREAS];
    static Boolean      loadCentroid = False; 
    double		epsilon;   
    Boolean		isSmear;
/*---------------------------------------------------------------------*/

    *iret = 0;

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


    af_initBnds( &ier );

/*
 *  Pre-process the input VG elements into an array of "GFA_SmrOlk_Grp".
 */
    gfaGrp = (GFA_SmrOlk_Grp *)NULL;
    af_elms2grps ( nin, el_in, &ngfagrp, &gfaGrp, &ier );


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
 *  Load the area centroid lat/lon array.
 */
    if ( ! loadCentroid ) {
       af_loadCentroids ( areaLatCentroid, areaLonCentroid, &ier );
       loadCentroid = True;
    }
/*
 *  Pass GFA format structures through condition wording rules. 
 */
    if ( _condWording != 0 ) {
        af_condsWording ( cycle, day, issuance, numFmt, fmt, &ier );

	/*
	 * Retrieve and save begin/end wordings.
	 */
        for ( ii = 0; ii < numFmt; ii++ ) { 

 	    if ( fmt[ ii ].delete == 0 ) {

                    isSmear = True;
                    cvg_getFld ( &(fmt[ ii ].el), TAG_GFA_SUBTYPE, tmpStr, &ier );
                    gfaSubType = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;
                                                                                          
                    if ( ( gfaSubType == GFA_USER_OUTLOOK ) ||
                         ( gfaSubType == GFA_SYSTEM_OUTLOOK ) ) {
                        isSmear = False;
                    }
                                                                                          
	            af_parseWording( isSmear, fmt[ii].wording, beginWording,
			endWording, &ier );

		    if ( strlen( beginWording ) != 0 ) {
		       cvg_setFld( &fmt[ii].el, TAG_GFA_CONDSBEGIN, beginWording, &ier );
		    }

		    if ( strlen( endWording ) != 0 ) {
		       cvg_setFld( &fmt[ii].el, TAG_GFA_CONDSEND, endWording, &ier );
		    }

	    }
        }
    }
/*
 *  Apply selectd snapshots' attributes to clipped airmet/outlook. 
 */
    af_applyAttr ( cycle, numFmt, fmt, &ier );
/*
 *  Generate the XML format string from the GFA format struct. 
 */
    for ( ii = 0; ii < nareas; ii++ ) {
        for ( jj = 0; jj < ntypes; jj++ ) {
            string[ii][jj] = (char *)NULL;
        }
    }
    af_fmt2xml ( nareas, areas, ntypes, types, day, cycle, 
                 numFmt, fmt, areaLatCentroid, areaLonCentroid,
                 string, &ier );

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

