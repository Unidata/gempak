
#include "afcmn.h"

/************************************************************************
 * afutils.c                                             		*
 *                                                                      *
 * This module contains some utility routines frequently used in the 	*
 * airmet library.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *      af_elm2poly		- creates a gpc polygon from an GFA elem*
 *      af_gpcPolyArea		- get the size of a gpc polygon		*
 *      af_poly2fmt		- construct a fmt struct from polygon	*
 *      af_fmtStateList		- format the state list			*
 *	af_IFRCombineCIGVIS 	- combine overlapped IFR CIG & VIS 	*
 *	af_queryPrefs		- load pref. settings used in formatter	*
 * 	af_useSnapshots		- remove cancelled snapshots		*
 * 	af_QualityControl	- check for bad GFA elements		*
 * 	af_getSSAttr		- get snapshot attr for each fcst hour	*
 *      af_TURBRenameHILO       - rename TURB-HI/LO to TURB		*
 *      af_copyGFA       	- copys one GFA elem to another GFA elem*
 *									*
 *   private functions:                                                 *
 *	af_checkMtObscStates	- verify MT OBSC state list		*
 *      								*
 ***********************************************************************/

static void af_checkMtObscStates ( const char *inStateList,
                                char *outStateList, int *iret );



/*
 *  Global variables
 */
static 	char	*_mtObscStates = NULL;


/*=====================================================================*/

void af_elm2poly ( VG_DBStruct el, gpc_polygon *gpc_poly, int *iret )
/************************************************************************
 * af_elm2poly                                                     	*
 *                                                                      *
 * Creates a GPC polygon in normalized coordinate from the GFA's points.*
 *                                                                      *
 * void af_elm2poly ( el, gpc_poly, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      el		VG_DBStruct     GFA element			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *gpc_poly	gpc_polyon	GPC polygon structure   	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05   	Created                         	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int			hole = 0, ier = 0, np;
    float		*xnormal, *ynormal;    
    gpc_vertex_list	verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
           

    /*
     * Convert to normalized coordinate first.
     */
    np = el.elem.gfa.info.npts;     
    G_MALLOC ( xnormal, float, np, "af_elm2poly: xnormal" );
    G_MALLOC ( ynormal, float, np, "af_elm2poly: ynormal" );

    gtrans ( sys_M, sys_N, &np, el.elem.gfa.latlon, 
             &el.elem.gfa.latlon[np], xnormal, ynormal,
             &ier, strlen(sys_M), strlen(sys_N) );
    
    /*
     * Fill GPC polygon structure with points in the incoming GFA element.
     */
    gpc_poly->num_contours = 0;
    gpc_poly->hole         = (int*)NULL;
    gpc_poly->contour      = (gpc_vertex_list*)NULL;

    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;

    np = el.elem.gfa.info.npts;     
    gpc_cvlist ( np, xnormal, ynormal, &verts, &ier );
    gpc_poly->num_contours = 0;
    gpc_add_contour ( gpc_poly, &verts, hole );
    
    free ( verts.vertex );
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

}

/*=====================================================================*/

float af_gpcPolyArea ( gpc_vertex_list *contour, char *sysp )
/************************************************************************
 * af_gpcPolyArea                                                    	*
 *                                                                      *
 * Calculates the area within a gpc polygon contour.			*
 *                                                                      *
 * void af_gpcPolyArea ( contour, sysp )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      poly		gpc_polygon	GPC polygon contour   		*
 *      *sysp		char		Coordinate system of polygon   	*
 *                                                                      *
 * Output parameters:                                                   *
 *      af_gpcPolyArea  float		Area in square nautical miles   *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          07/05   	Created                         	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int		np, ier;    
    float	*xlat, *ylon, *xnormal, *ynormal, area; 

    float	_radius = RADIUS*M2NM;	/* The Earth's radius in nautical miles*/
/*---------------------------------------------------------------------*/
        
    area = 0.0F;
    
    if ( contour ) {
        np = contour->num_vertices;
     
        G_MALLOC ( xlat, float, np, "af_gpcPolyArea: xlat\n" );
        G_MALLOC ( ylon, float, np, "af_gpcPolyArea: ylon\n" );
        G_MALLOC ( xnormal, float, np, "af_gpcPolyArea: xnormal\n" );
        G_MALLOC ( ynormal, float, np, "af_gpcPolyArea: ynormal\n" );
    
        gpc_gvlist ( contour, &np, xnormal, ynormal, &ier );

        gtrans ( sysp, sys_M, &np, xnormal, ynormal, xlat, ylon,
                        &ier, strlen(sysp), strlen(sys_M) );
     
        cgr_sphpolyarea ( &np, xlat, ylon, &_radius, &area, &ier );
    
        G_FREE ( xlat, float );
        G_FREE ( ylon, float );
        G_FREE ( xnormal, float );
        G_FREE ( ynormal, float );
    }
    
    return ( area );
    
}

/*=====================================================================*/

void af_poly2fmt ( gpc_vertex_list contour, VG_DBStruct el,  
                     char region, GFA_Elem_Format *fmt, int *iret )
/************************************************************************
 * af_poly2fmt                                                     	*
 *                                                                      *
 * Constructs a new GFA format structure element using the given GPC 	*
 * polygon and the VG element. The new VG element in the format struct	*
 * will have the same header information as the input VG element's but 	*
 * using lat/lons from the polygon. The input polygon are assumed in 	*
 * normalized coordinate.						*
 *                                                                      *
 * void af_poly2fmt ( contour, el, region, fmt, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      contour		gpc_vertex_list	GPC polygon contour	   	*
 *	el		VG_DBStruct	VG element structure		*
 *      region		char		region?				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *fmt		GFA_Elem_Format	GFA format structure		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/05   	Created                         *
 * J. Wu/SAIC		08/05   	Freed memory                    *
 * T. Piper/SAIC	09/05	replaced '1' with np, where np=1	*
 * J. Wu/SAIC		08/05   Initialize "delete" to G_FALSE		*
 * J. Wu/SAIC		12/05   change input to "gpc_vertex_list" type	*
 * B. Yin/SAIC		01/06	set the openLine to false		*
 * J. Wu/SAIC		02/06   remove duplicates from polygon		*
 * J. Wu/SAIC		03/06   initialize fmt->origInfo		*
 * J. Wu/SAIC		03/06   initialize fmt->wording			*
 * E. Safford/SAIC	08/06	initialize fmt->fzlvl			*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06   initialize fmt->reduceFLg to TRUE	*
 ***********************************************************************/
{
    int		nblock, blockLen, np, ier, one, ii;
    float	*xnormal, *ynormal;

    gpc_vertex_list	verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
                             				                
    /*
     * Fill the information.
     */	
    fmt->delete = G_FALSE;
    fmt->region = region;
    fmt->area[0] = '\0';
    fmt->adjarea[0] = '\0';
    fmt->reduceFlg = (int *)NULL;
    fmt->origInfo = (GFA_SmrOlk_Grp *)NULL;
    fmt->wording = (char *)NULL;

        
    /*
     * Copy the element, need to convert the polygon vertices
     * into map coordinate.
     */
    nblock = el.elem.gfa.info.nblocks;
    blockLen = nblock * STD_STRLEN * sizeof ( char );
		    
    fmt->el.elem.gfa.info.nblocks = nblock;

    G_MALLOC ( fmt->el.elem.gfa.info.blockPtr[ 0 ],
	       gfaBlock_t, nblock, "af_poly2fmt: fmt.el.blockPtr" );

    memcpy ( &(fmt->el.hdr), &(el.hdr), sizeof( VG_HdrStruct ) );

    memcpy ( fmt->el.elem.gfa.info.blockPtr[ 0 ], 
	     el.elem.gfa.info.blockPtr[ 0 ], blockLen );
		    	
    np = contour.num_vertices;
        
    G_MALLOC ( xnormal, float, np, "af_poly2fmt: xnormal" );
    G_MALLOC ( ynormal, float, np, "af_poly2fmt: ynormal" );
                   
    af_getPolyVerts ( &contour, &np, xnormal, ynormal, &ier );

    fmt->el.elem.gfa.info.npts = np;
    gtrans ( sys_N, sys_M, &np, xnormal, ynormal, 
	         fmt->el.elem.gfa.latlon,
	         &(fmt->el.elem.gfa.latlon[ np ]),
                 &ier, strlen(sys_N), strlen(sys_M) );	
    
    fmt->el.hdr.recsz = (int) ( sizeof( VG_HdrStruct ) 
	      	+ 2 * sizeof( int ) + blockLen
                + ( sizeof( float ) * (size_t)( 2 * np )));	

    /*
     *  Add the contour to the new GFA format structure's polygon
     */        
    one = 1;
    G_MALLOC ( fmt->el_poly, gpc_polygon, one, "af_poly2fmt: fmt->el_poly" );	
    fmt->el_poly->num_contours = 0;
    fmt->el_poly->hole         = (int*)NULL;
    fmt->el_poly->contour      = (gpc_vertex_list*)NULL;
    fmt->openLine 	       = False;
    fmt->fzlvlContour	       = False;

    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;

    gpc_cvlist ( np, xnormal, ynormal, &verts, &ier );
    
    gpc_add_contour ( fmt->el_poly, &verts, G_FALSE );

    free ( verts.vertex );
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );


    /*
     *  Initialize flags to TRUE for point reduction.
     */		    	
    G_MALLOC ( fmt->reduceFlg, int, np, "af_poly2fmt: reduceFlg" );	     

    for ( ii = 0; ii < np; ii++ ) {   
	fmt->reduceFlg[ ii ] = G_TRUE;	            
    }

}

/*=====================================================================*/

void af_fmtStateList ( const char *hazardType, int npts, 
			      const float *lat, const float *lon,
                              const char *area, const char *adjArea,
                              char *stateList, int *iret )
/************************************************************************
 * af_fmtStateList                                                      *
 *                                                                      *
 * This routine formats the state list.                                 *
 *                                                                      *
 * Note that it is assumed that adequate space (STD_STRLEN is safe) has *
 * been allocated for *stateList by the calling routine.                *
 *                                                                      *
 * static void af_fmtStateList ( npts, *lat, *lon, *area, *adjArea,     *
 *                               *stateList, *iret )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *hazardType     const char	element's hazard type		*
 *      npts            int             number of coordinate points     *
 *      *lat            const float     array of lat coordinates        *
 *      *lon            const float     array of lon coordinates        *
 *      *area           const char      FA area of smear                *
 *      *adjArea        const char      adjacent FA area in same region *
 *                                                                      *
 * Output parameters:                                                   *
 *      *stateList      char    list of states separated by " "         *
 *      *iret           int     Return code                             *
 *                                0: normal return                      *
 *                               -1: clo error on BNDS_FILE             *
 *                               -2: clo error getting flat/flon        *
 *                               -3: error processing States            *
 *                               -4: error processing Great Lakes       *
 *                               -5: error processing adjacent states   *
 *                               -6: error processing coastal waters    *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      12/05   initial coding                          *
 * E. Safford/SAIC	01/06	add hazardType param, eliminate CSTL    *
 *				  WTRS and Great Lakes for MT_OBSC      *
 * E. Safford/SAIC	02/06	fix CSTL WTRS over only waters (no land)*
 * E. Safford/SAIC	03/06	chng CSTL WTRS per new AWC specification*
 * E. Safford/SAIC	04/06	add call to af_checkMtObscStates	*
 * E. Safford/SAIC	05/06	mv call to af_checkMtObscStates		*
 * J. Wu/SAIC		07/06	initialize "finalStList" 		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    char        stList[ STD_STRLEN ], finalStList[ STD_STRLEN ], state[ 3 ];
    char        adjStList[ STD_STRLEN ], lake[ 3 ];
    char        tmpStr[ STD_STRLEN ], mtObscStatesList[ STD_STRLEN ];
	        
    char        localArea[ STD_STRLEN ], localAdjArea[ STD_STRLEN ];

    char        ptype[] = "AIRMET";

    float       flat[ MAXPTS ], flon[ MAXPTS ];
    float       localLat[ MAXPTS ], localLon[ MAXPTS ];
				    
    int         ier = 0, nState = 0, ii, jj, numWaterPts = 0;
    int		numLakePts = 0;
/*--------------------------------------------------------------------*/

    *iret = 0;
    stList[ 0 ] = '\0';
    finalStList[ 0 ] = '\0';
    
    
    /*
     *  The local versions of the parameters gets arround compiler warnings --
     *  the clo_* routines don't generally declare params as constants
     *  when they should.
     */
    for ( ii=0; ii < npts; ii++ ) {
         localLat[ii] = lat[ii];
	 localLon[ii] = lon[ii];
    }

    strcpy( localArea, area );
    strcpy( localAdjArea, adjArea );


    /*
     *  Check if smear area covers any states.
     */
    clo_binpoly( BNDS_FILE, npts, localLat, localLon, &ier );
    if( ier < 0 ) {
        *iret = -1;
	return;
    }

    clo_tgltln( BNDS_FILE, MAXST, &nState, flat, flon, &ier );
    if( ier < 0 ) {
        *iret = -2;
        return;
    }

    for ( jj = 0; jj < nState; jj++ )  {

        tmpStr[0] = '\0';
        clo_bginfo ( BNDS_FILE, jj, tmpStr, &ier );
        if( ier < 0 ) {
           break;
        }

        cst_gtag ( "STATE", tmpStr, "XX", state, &ier );
        if( ier < 0 ) {
           break;
        }

        sprintf ( stList, "%s%s ", stList, state );
    }

    if( ier < 0 ) {
        *iret = -3;
        return;
    }


    /*
     *  Check if smear area covers any Great Lakes, but
     *  do not check if hazard type is MT_OBSC.
     */
    ier = 0;
    if( strcmp( hazardType, MT_OBSC_HAZARD ) != 0 ) { 
        clo_binpoly( GREAT_LAKES, npts, localLat, localLon, &ier );
	numLakePts = clo_qnhot();
    }
    else {
        numLakePts = 0;
    }

    if( ier >= 0 && numLakePts > 0 ) {

        for( jj=0; jj < numLakePts; jj++ ) {

	    tmpStr[0] = '\0';
            clo_bginfo( GREAT_LAKES, jj, tmpStr, &ier );
            if( ier < 0 ) {
                break;
	    }

	    cst_gtag( "ID", tmpStr, "?", lake, &ier );
	    if( ier < 0 ) {
	        break;
	    }

	    sprintf( stList, "%s%s ", stList, lake );
	}
    }

    if( ier < 0 ) {
        *iret = -4;
        return;
    }


    /*
     *  Check for the airmet over coastal waters.  
     *
     *  If the state to which those waters belong is not already 
     *  in the stList, then add it.
     *
     *  MT_OBSC hazards can never include any coastal waters.
     */

     if( strcmp( hazardType, MT_OBSC_HAZARD ) != 0 ) { 

         clo_binpoly( AIRMET_CSTL_BNDS, npts, localLat, localLon, &ier );
         if( ier < 0 ) {
             *iret = -6;
	     return;
         }
	 numWaterPts = clo_qnhot();
     }
     else {
	 numWaterPts = 0;
     }


     if( numWaterPts > 0 ) {

        ier = 0;
	for ( jj = 0; jj < clo_qnhot(); jj++ )  {

            clo_bginfo ( AIRMET_CSTL_BNDS, jj, tmpStr, &ier );
	    if( ier < 0 ) {
	        break;
	    }

            cst_gtag ( "ID", tmpStr, "XX", state, &ier );
            if( ier < 0 ) {
                break;
            }

	    if ( !strstr( stList, state ) ) {
                sprintf( stList, "%s%s ", stList, state );
            } 

        }

	if( ier < 0 ) {
	    *iret = -6;
	    return;
	}

	strcat ( stList, " " );
    }


    /*
     *  List states in the FA_Area bounds first and then followed
     *  by states in the adjacent FA_Area bounds.
     */
    clo_fastates ( localArea, stList, ' ', ptype, finalStList, &ier );

    if( ier < 0 ) {
        *iret = -5;
	return;
    }

    strcpy( stateList, finalStList );

    if ( strlen ( adjArea ) > (size_t)0 ) {
        clo_fastates ( localAdjArea, stList, ' ', ptype, adjStList, &ier );

        if( ier >= 0 ) {
            strcat ( stateList, " " );
            strcat ( stateList, adjStList );
        }
        else {
            *iret = -5;
            return;
        }
    }

    /*
     *  If this is a MT_OBSC, purge all non-MT_OBSC states from the
     *  final state list.
     */
    if( strcmp( hazardType, MT_OBSC_HAZARD ) == 0 ) {
        af_checkMtObscStates( stateList, mtObscStatesList, &ier );
        if( ier >= 0 ) {
            strcpy( stateList, mtObscStatesList);
        }
    }


    /*
     *  If the airmet covers land and water then append "AND CSTL WTRS" 
     *  to the state list.  If only water then use just "CSTL WTRS" (no AND).
     */
    if( numWaterPts > 0 ) {

        if( nState > 0 ) {
	    strcat( stateList, " AND");
        }

	strcat( stateList, " CSTL WTRS"); 
    }
}

/*=====================================================================*/

void af_IFRCombineCIGVIS ( int *nin, 
			GFA_Elem_Format **fmt_in, int *iret )
/************************************************************************
 * af_IFRCombineCIGVIS                                                  *
 *                                                                      *
 * This routine combines an IFR CIG with IFR VISs if the IFR CIG 	*
 * overlaps an IFR VIS by an amount greater or equal to 3000 sq nm AND	*
 * the overlap amounts to greater than or equal to 50% of the smaller	*
 * of the two areas.  The total combination becomes a single IFRCIGVIS	*
 * FROM	line and the original IFR CIG and IFR VISs areas goes away.	*
 *                                                                      *
 * Note: 								*
 *    (1) "fmt_in" is reallocated in this routine and must be freed by	*
 *	  the caller.                                               	*
 *    (2) The new format structure array may contain "DELETED" format	*
 *	  structures.  It is up to the caller to check the flag.	*
 *                                                                      *
 * static void af_IFRCombineCIGVIS ( nin, fmt_in, iret )		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *nin		int     	number of GFA format structures	*
 *      *fmt_in		GFA_Elem_Format Array of GFA format structures	*
 *                                                    			*
 * Output parameters:                                             	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          10/05   	Created                         	*
 * J. Wu/SAIC          12/05   	Change para in af_poly2fmt()		*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int 	ii, jj, kk, *indx, ier, total, nComb;
    float	CIG_area, VIS_area, INT_area;
    char	value[32], hazard[32], type1[32], type[1024];
    char	freq0[32], freq1[32], worstFreq[32];
       
    gpc_polygon	gpc_poly_tmp, gpc_poly_int, gpc_poly_union;
/*--------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     *  Initialize the working array.
     */
    G_MALLOC ( indx, int, *nin, "af_IFRCombineCIGVIS: indx" );
    for ( ii = 0; ii < *nin; ii++ ) {
        indx[ ii ] = -1;
    }
    
    
    /*
     *  Combine IFR CIG with all IFR VIS if CIG and VIS overlap >= 3000 sq nm 
     *  and >= 50% of the smaller of the two (CIG and VIS).  Under this rule, 
     *  one VIS may be combined multiple times with different CIGs. 
     *
     *  After combination, the original CIG and VIS go away as marked "deleted"
     *  and a new hazard structure is generated as an "IFR".  The new hazard
     *  inherits all attributes from CIG except: (1) the "Frequency" will be the
     *  worst of the CIG and all combined VISs; (2) the "Type" will the combination
     *  of CIG and VIS' "Type" seperated by "/'.
     */
    total = *nin;
    for ( ii = 0; ii < *nin; ii++ ) {
		
	cvg_getFld ( &((*fmt_in)[ ii ].el), TAG_GFA_AREATYPE, hazard, &ier );	    
       	    	
        if ( strcmp ( hazard, "IFR_CIG" ) == 0 ) {

	    CIG_area = af_gpcPolyArea( &((*fmt_in)[ii].el_poly->contour[ 0 ]), sys_N );
            cvg_getFld ( &((*fmt_in)[ ii ].el), "<Frequency>", worstFreq, &ier );      
            cvg_getFld ( &((*fmt_in)[ ii ].el), "<Type>", type, &ier );      

	    nComb = 0;
	    
	    for ( jj = 0; jj < *nin; jj++ ) {
		
		cvg_getFld ( &((*fmt_in)[ jj ].el), TAG_GFA_AREATYPE, value, &ier );	    
	        
		if ( (*fmt_in)[jj].region == (*fmt_in)[ii].region &&
		      strcmp ( value, "IFR_VIS" ) == 0 ) {
		    		    	            
                    gpc_polygon_clip ( GPC_INT, (*fmt_in)[ii].el_poly, 
		                                (*fmt_in)[jj].el_poly, &gpc_poly_tmp );
	            /*
	               * Remove possible holes in union.
                       */
                    gpc_poly_int.num_contours = 0;
                    gpc_poly_int.hole         = (int*)NULL;
                    gpc_poly_int.contour      = (gpc_vertex_list*)NULL;

                    for ( kk = 0; kk < gpc_poly_tmp.num_contours; kk++ ) {
                        if ( gpc_poly_tmp.hole[ kk ] == G_FALSE ) {
	                    gpc_add_contour ( &gpc_poly_int, 
			                      &gpc_poly_tmp.contour[ kk ], G_FALSE );
	                }
                    }
	                
	            gpc_free_polygon ( &gpc_poly_tmp );
			
	            INT_area = af_gpcPolyArea( &gpc_poly_int.contour[ 0 ], sys_N );
	            VIS_area = af_gpcPolyArea( &((*fmt_in)[jj].el_poly->contour[ 0 ]), sys_N );
                        
	            
		    /*
	               *  Check if the CIG and VIS need to be combined.
                       */
 		    if ( INT_area >= AREA_LIMIT && 
		         INT_area >= ( G_MIN( VIS_area, CIG_area) * 0.5F ) ) {
						    
	                /*
		           *  Combine this VIS with CIG
		           */ 			    
			cvg_getFld ( &((*fmt_in)[ jj ].el), "<Type>", type1, &ier );
			
			if ( nComb == 0 ) {
			    gpc_polygon_clip ( GPC_UNION, (*fmt_in)[ii].el_poly, 
		                                          (*fmt_in)[jj].el_poly, &gpc_poly_tmp );
			}
			else {
			    gpc_polygon_clip ( GPC_UNION, &gpc_poly_union,
			                         (*fmt_in)[jj].el_poly, &gpc_poly_tmp );

			}
			
			if ( strstr ( type, type1 ) == NULL ) {
			    strcat ( type, "/" );
			    strcat ( type, type1 );
		        }
									
			indx[ nComb ] = jj;
			nComb++;
			
                        
			/*
	                   * Remove possible holes in union.
                           */
                        gpc_poly_union.num_contours = 0;
                        gpc_poly_union.hole         = (int*)NULL;
                        gpc_poly_union.contour      = (gpc_vertex_list*)NULL;

                        for ( kk = 0; kk < gpc_poly_tmp.num_contours; kk++ ) {
                            if ( gpc_poly_tmp.hole[ kk ] == G_FALSE ) {
	                        gpc_add_contour ( &gpc_poly_union, 
				                  &gpc_poly_tmp.contour[ kk ], G_FALSE );
	                    }
	                }            
		    
		        gpc_free_polygon ( &gpc_poly_tmp );
		
	                strcpy( freq0, worstFreq );
                        cvg_getFld ( &((*fmt_in)[ jj ].el), "<Frequency>", freq1, &ier );
                        ctb_gfaCmpSeverity( hazard, "Frequency", 
	     				    freq0, freq1, worstFreq, &ier ); 
		    
		    }  /* end of if ( INT_area >= AREA_LIMIT ... ) */		    
		}   /* end of if ( strcmp ( value, "IFR_VIS" ) == 0 */ 
	    }   /* end of "jj" loop */
	    
	    
	    /*
	      *  Generate a new hazard IFRCIGVIS and delete original CIG and VIS
	      */
	    if ( nComb > 0 ) {
	        
		G_REALLOC ( (*fmt_in), GFA_Elem_Format, 
		            (total+1), "af_IFRCombineCIGVIS: fmt_in" );
		    			    
		af_poly2fmt ( gpc_poly_union.contour[0], (*fmt_in)[ii].el,
                              (*fmt_in)[ii].region, &(*fmt_in)[total], &ier );
	                                
		cvg_setFld ( &((*fmt_in)[ total ].el), "<Frequency>", worstFreq, &ier );
                cvg_setFld ( &((*fmt_in)[ total ].el), "<Type>", type, &ier );
	        cvg_setFld ( &((*fmt_in)[ total ].el), TAG_GFA_AREATYPE, "IFR", &ier );
			
		total++;
			
		gpc_free_polygon ( &gpc_poly_union );
		    
		(*fmt_in)[ii].delete = G_TRUE;
		for ( kk = 0; kk < nComb; kk++ ) {
		    (*fmt_in)[ indx[kk] ].delete = G_TRUE;
	        }		
	    }
		
	} /* end of if ( strcmp ( hazard, "IFR_CIG" ) == 0 ) */
    }   

    
    G_FREE ( indx, int );	
    *nin = total;

    
    /*
     * Switch "IFR_CIG" and "IFR_VIS" to "IFR" for correct wording in the formatter.
     */
    for ( ii = 0; ii < *nin; ii++ ) {	                
	cvg_getFld ( &((*fmt_in)[ ii ].el), TAG_GFA_AREATYPE, hazard, &ier );
	
	if ( strcmp( hazard, "IFR_CIG" ) == 0 ||
	     strcmp( hazard, "IFR_VIS" ) == 0 ) {    
	    cvg_setFld ( &((*fmt_in)[ ii ].el ), TAG_GFA_AREATYPE, "IFR", &ier );
	}
    }
        
}

/*=====================================================================*/

void af_getPolyVerts ( gpc_vertex_list *contour, 
		int *nout, float *xout, float *yout, int *iret)
/************************************************************************
 * af_getPolyVerts                                                     	*
 *                                                                      *
 * This function gets the contour's vertices.  Duplicate points in the 	*
 * will be count as one.  Normalized coordinate is assumed.		*
 *                                                                      *
 * void af_getPolyVerts ( contour, nout, xout, yout, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour	gpc_vertex_list	Pointer to GPC polygon contour	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout		int		Number of unique points		*
 *      *xout		float		X-coord of points		*
 *      *yout		float		Y-coord of points		*
 *      *iret		int		Return code			*
 *                                      0 - normal 			*
 *                                       			 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/06   	Created                         *
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	09/06	make a library function			*
 ***********************************************************************/
{
    int		np, ii, jj, ier;
    float	*xtmp, *ytmp;
    Boolean	repeat;
/*---------------------------------------------------------------------*/
		    	
    *iret = G_NORMAL;
    
    np = contour->num_vertices;
        
    gpc_gvlist ( contour, &np, xout, yout, &ier );

    G_MALLOC ( xtmp, float, np, "af_getPolyVerts: xtmp" );
    G_MALLOC ( ytmp, float, np, "af_getPolyVerts: ytmp" );    

    /*
     *  Remove repeated points
     */
    *nout = 0;
    for ( ii = 0; ii < np; ii++ ) {

	repeat = False;

	for ( jj = ii + 1; jj < np; jj++ ) {

	    if ( ( fabs ( xout[ ii ] - xout[ jj ] ) < TIE_DISTANCE ) &&

	         ( fabs ( yout[ ii ] - yout[ jj ] ) < TIE_DISTANCE ) ) {

	       repeat = True;
	       break;

	    }
	}

	if ( !repeat ) {

	   xtmp[ *nout ] = xout[ ii ];
	   ytmp[ *nout ] = yout[ ii ];
	   (*nout)++;

	}
    }
    
    if ( *nout > 0 ) {
        for ( ii = 0; ii < *nout; ii++ ) {
           xout[ ii ] = xtmp[ ii ];
           yout[ ii ] = ytmp[ ii ];
        }      
    }
    
    G_FREE ( xtmp, float );
    G_FREE ( ytmp, float );
    
}

/*=====================================================================*/

static void af_checkMtObscStates ( const char *inStateList,
                                char *outStateList, int *iret )

/************************************************************************
 * af_checkMtObscStates                                                 *
 *                                                                      *
 *                                                                      *
 * static void af_checkMtObscStates ( *inStateList,                     *
 *                                    *outStateList, *iret)             *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 *                                      -1: error reading table data    *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      04/06   Initial coding                          *
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    char        **output, *tmpStr, *ptr, *srchPtr;
    int         ier = 0, nchar=2;
/*----------------------------------------------------------------------*/

    *iret = 0;
    strcpy( outStateList, "" );

    /*
     *  If _mtObscStates is NULL, then read the table and load
     *  the _mtObscStates string.
     */
    if( _mtObscStates == NULL ) {

        xml_readTable( "mt_obsc_states.xml", "pgen", "/MT_OBSC/area",
	                1, NULL, &output, &ier, "state", NULL );

	if( ier >= 0 ) {
	    G_MALLOC( _mtObscStates, char, (strlen( output[0] ) + 1 ),
	              "af_checkMtObscStates" );
	    strcpy( _mtObscStates, output[0] );

	    G_FREE( output[0], char );
	    G_FREE( output, char* );
	}
	else {
            G_MALLOC( _mtObscStates, char, nchar, "af_createMtObscStates" );
            strcpy( _mtObscStates, " " );
            *iret = -1;
        }
    }

    if( *iret < 0 ) {
        return;
    }


    /*
     *  Check the inStateList, state by state, against the _mtObscStates
     *  list.  Only write those states that are found in the _mtObscStates
     *  list to outStateList.
     */
    G_MALLOC( tmpStr, char, strlen( inStateList ) + 1,
                                        "af_createMtObscStates: tmpStr" ) ;
    strcpy( tmpStr, inStateList );

    ptr = strtok( tmpStr, " " );

    while( ptr != ( char )NULL ) {
        srchPtr = NULL;
        srchPtr = strstr( _mtObscStates, ptr );

        if( srchPtr ) {
            if( strlen( outStateList ) == 0 ) {
	        strcpy( outStateList, ptr );
	    } else {
	        strcat( outStateList, " " );
	        strcat( outStateList, ptr );
	    }
        }
        ptr = strtok( NULL, " " );
    }

    G_FREE( tmpStr, char );
}


/*=====================================================================*/

void af_queryPrefs ( int *iret )
/************************************************************************
 * af_queryPrefs                                                    	*
 *                                                                      *
 * Queries prefs.tbl for the preference settings used in the formatter.	*
 *                                                                      *
 * void af_queryPrefs ( int *iret )					*
 *                                                                      *
 * Input/Output parameters:                                    		*
 *	None                                                      	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int	Return code   				*
 *                      	0 - normal                    		*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          09/06   	Created                         	*
 * J. Wu/SAIC          03/07   	Added more preference settings		*
 ***********************************************************************/
{
    int 		ier;    
    char		tag_val[4096];
    float		tval;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
        
          
    /*
     * Query the value of _clippingFlg - whether to do regional clipping. 
     * Default to 1 (TRUE)
     */
    _clippingFlg = 1;
    ctb_pfstr ( "GFA_AF_CLIPPING", tag_val, &ier );
    if ( ier == 0 && strcasecmp(tag_val,"FALSE") == 0 ) {
        _clippingFlg = 0;
    }


    /*
     * Query the value of _condWording - whether to do conditional wording.
     * Default to 1 (TRUE).
     */
    _condWording = 1;
    ctb_pfstr ( "GFA_AF_CONDWORDING", tag_val, &ier );
    if ( ier == 0 && strcasecmp ( tag_val, "FALSE" ) == 0 ) {
        _condWording = 0;
    }

    
    /*
     * Query the value of _reduPtsFlg - whether to do point reduction. 
     * Default to 1 (TRUE). 
     */
    _reduPtsFlg = 1;
    ctb_pfstr ( "GFA_AF_REDUCEPTS", tag_val, &ier );
    if ( ier == 0 && strcasecmp(tag_val,"FALSE") == 0 ) {
        _reduPtsFlg = 0;
    }


    /*
     * Query the value of _reducePct - maximum areal percentage 
     * increase allowed when a single single point is removed
     * from a polygon. Default to 3.0 (3.0%). 
     */
    _reducePct = REDUCE_INCR_PCT;
    ctb_pfstr ( "SMEAR_INCR_PCT", tag_val, &ier );
    if ( ier >= 0 ) {
        cst_crnm ( tag_val, &tval, &ier );
        if ( ier == 0 ) {
            _reducePct = tval;
        }    
    }

    _reducePctOrig = REDUCE_INCR_PCT_ORIG;
    ctb_pfstr ( "REDUCEPTS_INCR_PCT_ORIG", tag_val, &ier );
    if ( ier >= 0 ) {
        cst_crnm ( tag_val, &tval, &ier );
        if ( ier == 0 ) {
            _reducePctOrig = tval;
        }    
    }

    /*
     * Query the value of _reduceDist - the maximum distance from
     * the original point allowed when a single is removed from
     * a polygon.  Default to 100.0 (Nautical miles). 
     */
    _reduceDst = REDUCE_INCR_DST;
    ctb_pfstr ( "SMEAR_INCR_DST", tag_val, &ier );
    if ( ier >= 0 ) {
        cst_crnm ( tag_val, &tval, &ier );
        if ( ier == 0 ) {
            _reduceDst = tval;
        }    
    }


    /*
     * Query the value of _clusterDist - the distance to define
     * consecutive points as a clustering.  Default to 30nm. 
     */
    _clusterDst = CLUSTER_DIST;        	
    ctb_pfstr ( "GFA_SNAP_CLUSTER_DIST", tag_val, &ier );
    if ( ier >= 0 ) {
        cst_crnm ( tag_val, &tval, &ier );
        if ( ier == 0 ) {
            _clusterDst = tval;
        }    
    }
    
}

/*=====================================================================*/

void af_useSnapshots ( VG_DBStruct **elIn, int nElIn, int smearFlag,
                       VG_DBStruct ***elOut, int *nElOut, 
		       VG_DBStruct ***cancelElOut, int *nCancelOut, 
		       int *iret )
/************************************************************************
 * af_useSnapshots                                                    	*
 *                                                                      *
 * This routine removes canceled snapshots if there are non-canceled   	*
 * snapshots in the input elements array. If all input snapshots are    *
 * canceled, they are kept in the result array.                         *
 *                                                                      *
 * Note: The calling routine should free elOut and cancelElOut.         *
 *                                                                      *
 * void af_useSnapshots ( elIn, nElIn, smearFlag, elOut, nElOut, iret ) *
 *                                                                      *
 * Input parameters:                                    		*
 *	*elIn		VG_DBStruct	input snapshots pointer array  	*
 *	nElIn		int		number of input snapshots     	*
 *	smearFlag	int		smear or outlook	      	*
 *                                                                      *
 * Output parameters:                                                   *
 *      ***elOut	VG_DBStruct	result snapshots 		*
 *      *nElOut		int		number of result snapshots	*
 *	***cancelElOut	VG_DBStruct	array of canceled snapshots	*
 *	*nCancelOut	int		number of canceled snapshots 	*
 *      *iret		int		Return code   			*
 *                      		0 - normal                    	*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC         10/06   	Created                         	*
 * E. Safford/SAIC	12/06	add cancelElOut, nCancelOut params	*
 * B. Yin/SAIC         	03/07   set cancelElOut to NULL if allCancel	*
 ***********************************************************************/
{
    int         ii, jj, kk, startHr, endHr, *flags, ier;
    char        fcstHr[ 32 ], status[ 32 ];

    Boolean     allCancel;
/*---------------------------------------------------------------------*/
                                                                                     
    *iret = 0;
    *nCancelOut = 0;

    /* 
     *  Initialize the start and end hours.
     */
    if ( smearFlag == GFA_USER_SMEAR || smearFlag == GFA_SYSTEM_SMEAR ) {
                                                                                
       startHr = 0;
       endHr   = 6;
                                                                                    
    }
    else if ( smearFlag == GFA_USER_OUTLOOK || smearFlag == GFA_SYSTEM_OUTLOOK ) {
                                                                                    
       startHr =  6;
       endHr   = 12;
                                                                                 
    }
    else {
                                                                                    
       startHr = endHr = 0;
                                                                               
    }
                                                                          
    /* 
     *  Set flags for all input snapshots.
     *       Cancelled       --  0
     *       Not Cancelled   --  1
     *       Not in period   -- -1
     */
    G_MALLOC ( flags, int, nElIn, "af_useSnapshots: flags" );
                                                                                
    allCancel = True;
                                                                               
    for ( ii = 0; ii < nElIn; ii++ ){
                                                                         
        fcstHr[ 0 ] = '\0';
        cvg_getFld ( elIn[ ii ], TAG_GFA_FCSTHR, fcstHr, &ier );
                                                                                
        if ( atoi( fcstHr ) >= startHr && atoi( fcstHr ) <= endHr ) {
                                                                           
           status[ 0 ] = '\0';
           cvg_getFld( elIn[ ii ], TAG_GFA_STATUS, status, &ier );
                                                                          
           if ( strcasecmp( status, "CAN" ) == 0 ) {
                                                                           
              flags[ ii ] = 0;
              (*nCancelOut)++;
                                                                          
           }
           else {
                                                                               
              flags[ ii ] = 1;
              allCancel   = False;
                                                                              
           }
                                                                             
        }
        else {
                                                                                 
           flags[ ii ] = -1;
                                                                               
        }
                                                                               
    }
                                                                              
    /* 
     *  If all snapshots in the smear/outlook period 
     *  are cancelled, keep them as acceptable snapshots.
     */
    *nElOut = 0;
                                                                              
    for ( ii = 0; ii < nElIn; ii ++ ) {
                                                                             
        if ( allCancel ) {
                                                                                 
           if ( flags[ ii ] == 0 ) {
                                                                          
              (*nElOut)++;
              flags[ ii ] = 1;
                                                                              
           }

        }                                                                        
        else {
                                                                                 
          if ( flags[ ii ] == 1 ) {
                                                                                   
             (*nElOut)++;
                                                                                  
          }
       }
    }
                                                                                
    /* 
     *  Copy the acceptable snapshots pointers to the output array.
     */
    G_MALLOC( *elOut, VG_DBStruct*, *nElOut, "af_useSnapshots: elOut" );

    if ( !allCancel ) { 

       G_MALLOC( *cancelElOut, VG_DBStruct*, *nCancelOut, 
    					"af_useSnapshots: cancelElOut" );
    }
    else {

       *cancelElOut = NULL;

    }                                                           

    for ( ii = 0, jj = 0, kk = 0; ii < nElIn; ii++ ) {
                                                                                  
        if ( flags[ ii ] == 1 ) {
                                                                                    
           (*elOut)[ jj ] = elIn[ ii ];
           jj++;
                                                                                
        }
	else if( flags[ ii ] == 0 ) {
           (*cancelElOut)[ kk ] = elIn[ii];
	   kk++;
	}
    }

    G_FREE ( flags, int );

}

/*=====================================================================*/
void af_qualityControl ( int nin, VG_DBStruct *el_in, int maxinfo, 
			 char *info, int *iret )
/************************************************************************
 * af_qualityControl                                                    *
 *                                                                      *
 * Checks the array of GFA elements for bad attributes. The first 	*
 * implementation only checks for number of points less than 3.		*
 * The output info string is NULL unless a bad element is found.	*
 *                                                                      *
 * void af_qualityControl ( nin, el_in, maxinfo, info, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      nin		int	        Input number of GFA elements	*
 *      *el_in		VG_DBStruct     Array of GFA elements		*
 *      el		VG_DBStruct     GFA element			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *info		char		Information string		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 *                                       -1: bad GFA found		*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	03/07	Created					*
 ***********************************************************************/
{
int	ii, subType, bad_found, hazTypeI, ier;
char	*buffer;
char	*fmt = {"%s for hazard %s / tag %s contains too few points."};
char	tmpStr[ STD_STRLEN ], hazType[ STD_STRLEN ], tag[ STD_STRLEN ];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    info[0] = '\0';
           
    for ( ii = 0; ii < nin; ii++ ) {

        /*  
	 *  Examine GFA elements
	 */
	if (  el_in[ ii ].hdr.vg_type == GFA_ELM ) {

	    /*
	     * Only pull out non-freezing level GFAs w/ npts less than 3,
	     * and freezing level GFAs with less than 2.
	     *
	     * Pull area type (hazard) so we can do the hazard-specific
	     * checking.
	     */
	    cvg_getFld ( &el_in[ ii ], TAG_GFA_SUBTYPE, tmpStr, &ier );
	    hazTypeI = atoi( tmpStr ) / 10;

	    bad_found = G_FALSE;

	    if ( hazTypeI == GFA_HAZARD_FZLVL_SFC || 
		 hazTypeI == GFA_HAZARD_FZLVL )  {
		if ( el_in[ ii ].elem.gfa.info.npts < 2 )  {
		    bad_found = G_TRUE;
		}
	    }
	    else if ( el_in[ ii ].elem.gfa.info.npts < 3 )  {
		bad_found = G_TRUE;
	    }

	    if (  bad_found == G_TRUE )  {

		/*
		 * Get fields for info return
		 */
	        cvg_getFld ( &el_in[ ii ], TAG_GFA_SUBTYPE, tmpStr, &ier );
	        subType = atoi( tmpStr ) - ( atoi( tmpStr ) / 10 * 10 );
	        cvg_getFld ( &el_in[ ii ], TAG_GFA_AREATYPE, hazType, &ier );
		cvg_getFld ( &el_in[ ii ], TAG_GFA_TAG, tag, &ier );

		/*
		 * Allocate just enough space
		 */
		G_MALLOC ( buffer, char, 
			strlen(fmt)+strlen(tmpStr)+strlen(hazType)+strlen(tag),
			"Error allocating buffer in af_qualityControl" );

		/*
		 * format the information in buffer
		 */
		sprintf ( buffer, fmt, 
			( subType == 0 ? "Snapshot" : 
			  ( subType == 1 || subType == 2 ? "Smear" : 
			    "Outlook" ) ), hazType, tag );
		/*
		 * copy allowable characters into info string
		 */
		strncpy ( info, buffer, maxinfo );

		/*
		 * free buffer memory & set error before returning
		 */
		G_FREE ( buffer, char );

		*iret = -1;

		return;
	    }
	}
    }
}

/*=====================================================================*/

void af_replaceIntPt ( gpc_vertex_list ctr_in, int nip, float *ipx,
		         float *ipy, float *sipx, float *sipy, 
			 gpc_vertex_list *ctr_out, int *iret )
/************************************************************************
 * af_replaceIntPt                                                     	*
 *                                                                      *
 * This function replaces the given points in the original polygon with	*
 * with specified new points.						*
 *                                                                      *
 * void af_replaceIntPt ( fmt, nip, ipx, ipy, sipx, sipy, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      ctr_in	gpc_vertex_list	GPC contour				*
 *      nip	int         	Total number of common intersection pt 	*
 *      *ipx	float         	X-coords of intersection pt		*
 *      *ipy	float         	Y-coords of intersection pt 		*
 *      *sipx	float         	snapped matches of ipx outside of GFA 	*
 *      *sipy	float         	snapped matches of ipy outside of GFA	*
 *                                                                      *
 * Input/Output parameters:                                          	*
 *      *ctr_outgpc_vertex_list	GPC contour				*
 *                                                                      *
 * Output parameters:                                         		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		03/07   Initial coding              		*
 ***********************************************************************/
{
    int		np, ier, ii, jj;
    float	*xn, *yn, *lat, *lon;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;       
    
    /*
     *  Replace the given points with their specified matches. 
     */        
    np = ctr_in.num_vertices;
    G_MALLOC ( xn, float, np, "af_replaceIntPt: xtmp" );
    G_MALLOC ( yn, float, np, "af_replaceIntPt: ytmp" );    
    G_MALLOC ( lat, float, np, "af_replaceIntPt: xnormal" );
    G_MALLOC ( lon, float, np, "af_replaceIntPt: ynormal" );
        
    gpc_gvlist ( &ctr_in, &np, xn, yn, &ier );
       
    gtrans ( sys_N, sys_M, &np, xn, yn, lat, lon,
             &ier, strlen(sys_N), strlen(sys_M) );
	
    for ( ii = 0; ii < np; ii++ ) {            		
	for ( jj = 0; jj < nip; jj++ ) {	    
	    if ( G_DIST( lat[ii], lon[ii], ipx[ jj ], ipy[ jj ] ) < SMALLF ) {
		lat[ ii ] = sipx[ jj ];
	        lon[ ii ] = sipy[ jj ]; 
	    }
	}
    } 
    

    /*
     *  Put points into output contour. 
     */        
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

void dumpGFA_Elem_Format ( char *str, int *nin,
                        GFA_Elem_Format **fmt_in )
/************************************************************************
 * dumpGFA_Elem_Format                                                  *
 *                                                                      *
 * Dumps the contents of a GFA formatting element structure.            *
 * The identifying string is for debugging location purposes.           *
 *                                                                      *
 * void dumpGFA_Elem_Format ( str, nin, fmt_in )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *str            char            Identifying string              *
 *      *nin            int             Input # of GFA fmt elements     *
 *      **fmt_in        GFA_Elem_Format Array of GFA format elements    *
 *                                                                      *
 * Output parameters:                                                   *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     03/07   Created                                 *
 ***********************************************************************/
{
    int ii, jj, npts;
/*---------------------------------------------------------------------*/

    printf("%s\n", str );

    for ( ii = 0; ii < *nin; ii++ )  {

        printf ( "GFA_Elem_Format #%2d: %s\n", ii,
                (*fmt_in)[ii].delete == G_TRUE ? "DELETED" : "ACTIVE" );

        npts = (*fmt_in)[ii].el.elem.gfa.info.npts;

        printf ( "\tRegion = %c, Area = %s, adjArea = %s, npts = %d\n",
                (*fmt_in)[ii].region, (*fmt_in)[ii].area, (*fmt_in)[ii].adjarea, npts );

        printf("\treduceFlg=");
        for ( jj = 0; jj < (*fmt_in)[ii].el.elem.gfa.info.npts; jj++ )  {
            printf("%c", (*fmt_in)[ii].reduceFlg[jj] == G_TRUE?'T':'F' );
        }
        printf("\n");

        for ( jj = 0; jj < npts; jj++ )  {
            printf("\t%2d - %6.2f, %6.2f\n", jj, (*fmt_in)[ii].el.elem.gfa.latlon[jj],
                    (*fmt_in)[ii].el.elem.gfa.latlon[jj+npts] );
        }
    }
}

/*=====================================================================*/

void af_getSSAttr ( int nin, VG_DBStruct **el_in, gpc_polygon *poly,
		   int *nout, GFA_SS_Attr *ss_attr, int *iret )
/************************************************************************
 * af_getSSAttr		                                       		*
 *                                                                      *
 * Groups snapshots at each unique forecast hour and determine each	*
 * group's attributes from the snapshots within the group.		*
 *                                                                      *
 * void af_getSSAttr ( nin, el_in, poly, nout, ss_attr, iret )		*
 *                                                                      *
 * Input parameters:	  	                                        *
 *      nin		int		number of snapshots		*
 *      **el_in 	VG_DBStruct 	array of snapshots		*
 *      *poly		gpc_polygon 	pointer to FROM line polygon	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *nout		int 		number of forecast hours found	*
 *      *ss_attr	GFA_SS_Attr	attribute of ss at each hour	*
 *      *iret		int 		return code			*
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	04/07 	Created					*
 ***********************************************************************/
{ 
    int			ii, jj, ier, cur_hour, nsnap, tmin;
    int			*ss_type, *whour, *wmin;
    int			*dvlpg_hr, *endg_hr;   
    float		farea, fhour;
            
    char		*pstr, fcst_hr[9];

    gpc_polygon		ss_poly, tmp_poly;  
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    G_MALLOC ( ss_type, int, nin, "af_getSSAttr: ss_type" );	    
    G_MALLOC ( whour, int, nin, "af_getSSAttr: whour" );	    
    G_MALLOC ( wmin, int, nin, "af_getSSAttr: wmin" );	    
    G_MALLOC ( dvlpg_hr, int, nin, "af_getSSAttr: dvlpg_hr" );	    
    G_MALLOC ( endg_hr, int, nin, "af_getSSAttr: endg_hr" );	    
		
	
    /*
     *  Find the type of each snapshot
     *  X_SNAPSHOTS - SS intersects with the clipped polygon with an area > 3K.
     *  O_SNAPSHOTS - SS do not intersect with the clipped polygon
     */
    for ( ii = 0; ii < nin; ii++ ) {
            
	af_elm2poly ( *(el_in[ii]), &ss_poly,  &ier );
	    
	gpc_polygon_clip ( GPC_INT, poly, &ss_poly, &tmp_poly );
	    
	farea = 0.0F;
	ss_type [ ii ] = O_SNAPSHOTS;	    
	if ( tmp_poly.num_contours > 0  )  {
            for ( jj = 0; jj < tmp_poly.num_contours; jj++ ) {
                if ( tmp_poly.hole[ jj ] == G_FALSE ) {
	            farea += af_gpcPolyArea( &tmp_poly.contour[ jj ], sys_N );
		}
            }	        
	        
            if ( farea > AREA_LIMIT ) {
	        ss_type [ ii ] = X_SNAPSHOTS;		
	    }
	}
	    
	gpc_free_polygon ( &tmp_poly );		            
	gpc_free_polygon ( &ss_poly );		            
    }
    

    /*
     *  Retrieve each snapshot's forecast time. 
     *
     *  Round UP   - X_SNAPSHOTS for development wording or
     *               O_SNAPSHOTS for ending wording 
     *  Round Down - X_SNAPSHOTS for ending wording or
     *               O_SNAPSHOTS for development wording 
     */
    for ( ii = 0; ii < nin; ii++ ) {
	cvg_getFld ( el_in[ ii ], TAG_GFA_FCSTHR,  fcst_hr, &ier );            
	    	    
	whour[ ii ] = atoi ( fcst_hr );
	wmin[ ii ] = 0;	    
	pstr = strchr ( fcst_hr, ':' );
	if ( pstr && (pstr++) ) {
            wmin [ ii ]= atoi ( pstr );	    
	}
	
	fhour = whour [ ii ] + wmin [ ii ] / 60.0F;
	if ( ss_type[ ii ] == X_SNAPSHOTS ) {
	    dvlpg_hr[ ii ] = ceil ( fhour );
	    endg_hr[ ii ]  = floor ( fhour );
	}
	else {
	    dvlpg_hr[ ii ] = floor ( fhour );
	    endg_hr[ ii ]  = ceil ( fhour );
	}
    }
        
    /*
     *  Group snapshots with their forecast hours and determine each group's
     *  attributes - forecast hour,  number of snopshots in group , start index, 
     *  each snapshot's type (X or O),  type of the group, hour for
     *  development wording, and hour for ending wording.  
     *   
     *  The type of the group is determined as:
     *      O_SNAPSHOTS - if all snapshots in the group are type O;  
     *      X_SNAPSHOTS - if at least one of the snapshots in the group is type X.
     *   
     *  The hours of the development/ending wording are computed as:  
     *      Round UP   - X_SNAPSHOTS for development wording or
     *                   O_SNAPSHOTS for ending wording 
     *      Round Down - X_SNAPSHOTS for ending wording or
     *                   O_SNAPSHOTS for development wording 
     */
    *nout = 1;
    cur_hour = whour[ 0 ] * 60 + wmin[ 0 ];
    nsnap = 0;
    for ( ii = 0;  ii < nin; ii++ ) {		
	tmin = whour[ ii ] * 60 + wmin[ ii ];
	if ( tmin !=  cur_hour ) {
	    cur_hour = tmin;	    
	    (*nout)++;
	    nsnap = 0;	
	}
	
	if ( nsnap == 0 ) {	    
	    ss_attr[ *nout - 1].hour = whour[ ii ];
	    ss_attr[ *nout - 1].minute = wmin[ ii ];
	    ss_attr[ *nout - 1].nsnap = 1;
	    ss_attr[ *nout - 1].start = ii;
	    ss_attr[ *nout - 1].type = ss_type[ ii ];	    
	    ss_attr[ *nout - 1].dvlpg_hr = dvlpg_hr[ ii ];
	    ss_attr[ *nout - 1].endg_hr = endg_hr[ ii ];
	}
	else {
	    ( ss_attr[ *nout - 1].nsnap )++;
	    	    
	    if ( ss_type[ ii ] == X_SNAPSHOTS ) {
	        ss_attr[ *nout - 1].type = ss_type[ ii ];
	        ss_attr[ *nout - 1].dvlpg_hr = dvlpg_hr[ ii ];
	        ss_attr[ *nout - 1].endg_hr = endg_hr[ ii ];
	    }
	}
	
	ss_attr[ *nout - 1].ssw[nsnap].type = ss_type[ ii ];
	ss_attr[ *nout - 1].ssw[nsnap].hour = whour[ ii ];
	ss_attr[ *nout - 1].ssw[nsnap].minute = wmin[ ii ];
	ss_attr[ *nout - 1].ssw[nsnap].dvlpg_hr = dvlpg_hr[ ii ];
	ss_attr[ *nout - 1].ssw[nsnap].endg_hr = endg_hr[ ii ];
	
	nsnap++;
    }


    /*
     *  Clean up
     */
    G_FREE ( ss_type, int );	    
    G_FREE ( whour, int );	    
    G_FREE ( wmin, int );	    
    G_FREE ( dvlpg_hr, int );	    
    G_FREE ( endg_hr, int );	    
}

/*=====================================================================*/
				
void af_TURBRenameHILO ( char *hazardType )  
/************************************************************************
 * af_TURBRenameHILO                                                    *
 *                                                                      *
 * This routine renames the hazard type to 'TURB' if the hazard is      *
 * 'TURB-HI' or 'TURB-LO'.                                    	        *
 *                                                                      *
 * static void af_TURBRenameHILO ( hazardType )				*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *hazardType	char 	Hazard type				*
 *                                                                      *
 * Return parameters:                   	                        *
 *      None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		 2/06	Created					*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		09/07	make it a public function		*
 ***********************************************************************/
{
    if ( strcasecmp ( hazardType, "TURB-HI" ) == 0 ||
    	 strcasecmp ( hazardType, "TURB-LO" ) == 0 ) {

	strcpy ( hazardType, "TURB" );

    }
} 

/*=====================================================================*/
                               
void af_copyGFA ( const VG_DBStruct *elIn, VG_DBStruct *elOut, 
			int *iret )
/************************************************************************
 * af_copyGFA	                                                	*
 *                                                                      *
 * Copys one GFA element to another GFA element.   			*
 *                                                                      *
 * void af_copyGFA ( elIn, elOut, iret )			 	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*elIn 		VG_DBStruct	input GFA element		*	
 *									*
 * Output parameters:                                                   *
 *	*elOut		VG_DBStruct	output GFA element		*
 *	*iret		int		error code			*
 *					-1 - Incorrect input, no copying*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/07	initial coding				*
 * J. Wu/SAIC		12/07	check the input element			*
*************************************************************************/
{
   int		nblocks;
/*---------------------------------------------------------------------*/
   
   *iret = 0;
   
   if ( !elIn || elIn->elem.gfa.info.nblocks == 0 ||
        elIn->hdr.vg_type != GFA_ELM ) {
       *iret = -1;
       return;
   }
   
   nblocks = elIn->elem.gfa.info.nblocks;
   elOut->elem.gfa.info.nblocks = nblocks;

   G_MALLOC ( elOut->elem.gfa.info.blockPtr[ 0 ],
	      gfaBlock_t, nblocks, "pgfrom_smear: eOut->blockPtr" );

   memcpy ( &(elOut->hdr), &(elIn->hdr), sizeof( VG_HdrStruct ) );

   memcpy ( elOut->elem.gfa.info.blockPtr[ 0 ], 
	    elIn->elem.gfa.info.blockPtr[ 0 ], 
	    elIn->elem.gfa.info.nblocks * STD_STRLEN * sizeof ( char ) );

   elOut->elem.gfa.info.npts= elIn->elem.gfa.info.npts;
   memcpy ( &(elOut->elem.gfa.latlon), &(elIn->elem.gfa.latlon), 
            elIn->elem.gfa.info.npts * 2 * sizeof( float ) );

}

/*=====================================================================*/
