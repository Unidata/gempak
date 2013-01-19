#define AF_GLOBAL
#include "afcmn.h"

/*
 * Preference settings in prefs.tbl
 */
int             _clippingFlg, _condWording, _reduPtsFlg;
float           _reducePct, _reducePctOrig, _reduceDst, _clusterDst;

/*
 * Global declarations
 */
gpc_polygon     _intlBndPoly;
gpc_polygon     _regionBndPoly[NUM_FA_REGION];
gpc_polygon     _areaBndPoly[NUM_FA_AREAS];
gpc_polygon     _areaXBndPoly[NUM_FA_AREAS];


/*
 *  Global variables
 */
char    *_FA_Region[ NUM_FA_REGION ] = {"WEST", "CENTRAL", "EAST"};
char    *_FA_Area[NUM_FA_AREAS] = {"SLC", "SFO", "CHI", "DFW", "BOS", "MIA"};

/*
 *  FA region bounds in map coordinates as closed lines.
 */
Boolean         _boundSet = False;
int             _westP, _centP, _eastP;
float           _westX[MAX_BOUND_PT], _westY[MAX_BOUND_PT],
                _centX[MAX_BOUND_PT], _centY[MAX_BOUND_PT],
                _eastX[MAX_BOUND_PT], _eastY[MAX_BOUND_PT];
/*
 *  Common borders of w/c regions, and c/e regions (map coord)
 */
int             _nwc, _nec;
float           _wcX[MAX_BOUND_PT], _wcY[MAX_BOUND_PT],
                _ecX[MAX_BOUND_PT], _ecY[MAX_BOUND_PT];

/*
 *  Common borders of SLC/SFO extended areas, and CHI_BOS/DFW_MIA.
 */
int             _nextdw, _nextdc;
float           _extdwX[MAX_BOUND_PT], _extdwY[MAX_BOUND_PT],
                _extdcX[MAX_BOUND_PT], _extdcY[MAX_BOUND_PT];
/*
 *  Extended FA area bounds in map coordinates as closed lines.
 */
int             _sfoP, _slcP, _chibosP, _dfwmiaP;
float           _sfoX[MAX_BOUND_PT], _sfoY[MAX_BOUND_PT],
                _slcX[MAX_BOUND_PT], _slcY[MAX_BOUND_PT],
                _chibosX[MAX_BOUND_PT], _chibosY[MAX_BOUND_PT],
                _dfwmiaX[MAX_BOUND_PT], _dfwmiaY[MAX_BOUND_PT];


/************************************************************************
 * afbounds.c                                             		*
 *                                                                      *
 * This module contains the subroutines that prepare international,	*
 * region, and area boundaries.                				*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 * 	af_initBnds		- initialize area/region bound structs  *
 * 	af_freeBnds		- free area/region bound structs 	*
 *      af_bnds2poly		- generate gpc polygons from bounds file*
 *	af_reg2intl		- compute international bound		* 
 *	af_setBoundInt		- find & save common boundary points	*
 *      af_loadCentroids	- get centroids of the input polygons	*
 ***********************************************************************/

void af_initBnds ( int *iret )
/************************************************************************
 * af_initBnds		                                                *
 *                                                                      *
 * Initialize all common bounds.                                       	*
 *                                                                      *
 * void af_intiBnts ( void )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	none								*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC  	08/07	initial conding				*
 ***********************************************************************/
{
    int		ii, ier;
    char	tag[ STD_STRLEN ];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if( !_boundSet ) {
/*
 * Query the preference settings in prefs.tbl.
 */
    af_queryPrefs ( &ier );

/*
 *  Create GPC polygons for all FA region and area bounds
 */
    for ( ii = 0; ii < NUM_FA_REGION; ii++ ) {
        sprintf ( tag, "<REGION>%s", _FA_Region[ii] );
	af_bnds2poly ( "FA_REGION_BNDS", tag, &_regionBndPoly[ii], &ier );
    }
    for ( ii = 0; ii < NUM_FA_AREAS; ii++ ) {
        sprintf ( tag, "<AREA>%s", _FA_Area[ii] );
        af_bnds2poly ( "FA_AREA_BNDS", tag, &_areaBndPoly[ii], &ier );
    }
    for ( ii = 0; ii < NUM_FA_AREAS; ii++ ) {
        sprintf ( tag, "<AREA>%s", _FA_Area[ii] );
        af_bnds2poly ( "FA_AREAX_BNDS", tag, &_areaXBndPoly[ii], &ier );
    }
    af_reg2intl ( &ier );

/*
 *  Find and save common borders between W & C, and those between 
 *  C & E regions.
 */
    af_setBoundInt ( &ier );
    _boundSet = G_TRUE;

    }

}


void af_freeBnds ( int *iret )
/************************************************************************
 * af_freeBnds		                                                *
 *                                                                      *
 * Free up all common bounds.                                       	*
 *                                                                      *
 * void af_freeBnts ( void )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	none								*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC  	08/07	initial conding				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Free GPC polygons for all FA bounds.
 */
    for ( ii = 0; ii < NUM_FA_REGION; ii++ ) {
        gpc_free_polygon ( &_regionBndPoly[ii] );
    }					    
    for ( ii = 0; ii < NUM_FA_AREAS; ii++ ) {
        gpc_free_polygon ( &_areaBndPoly[ii] );
    } 
    gpc_free_polygon ( &_intlBndPoly );

    _boundSet = G_FALSE;
}


/*=====================================================================*/

void af_bnds2poly ( char *bounds, char *name, 
			gpc_polygon *polygon, int *iret )
/************************************************************************
 * af_bnds2poly		                                                *
 *                                                                      *
 * Puts all bound parts into a polygon in normalized coordinate.	*
 *                                                                      *
 * void af_bnds2poly ( bounds, name, polygon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*bounds		char		Bounds name			*
 *	*name		char		Clipping area tag		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*polygon	gpc_polygon	Resulting polygon		*
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           7/05   	Copied from cloclip.c			*
 * J. Wu/SAIC           2/06   	Save bounds in map coord for future ref	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC           01/07   load extended FA area bounds		*
 ***********************************************************************/
{
    int			minp, maxp, next, npts, ier, ii;
    float		*xlat, *ylon, *xnormal, *ynormal;
    float		filter;
    gpc_vertex_list 	verts;
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    /*
     * Set bounds type and tag
     */
    clo_bstype ( bounds, &ier );
    clo_bstag  ( name, &ier );

    /*
     * Initialization
     */
    minp   = 0;
    maxp   = LLMXPT;
    filter = 0.0F;
    next   = 0;

    polygon->num_contours = 0;
    polygon->hole         = (int*)NULL;
    polygon->contour      = (gpc_vertex_list*)NULL;

    /*
     * Read each bounds part and put it in a gpc polygon
     */  
    G_MALLOC ( xlat, float, maxp, "af_bnds2poly: xlat" );
    G_MALLOC ( ylon, float, maxp, "af_bnds2poly: ylon" );

    clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &next );
        
    /*
     *  Save off boundary points  - keep only TWO digits for later use
     *  in cgr_intersect().
     */
    if ( !_boundSet ) {
        if ( strstr ( name, _FA_Region[0] ) ) {
            _westP = npts;
	    for ( ii = 0; ii < npts; ii++ ) {
                _westX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                _westY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	    }	    
        }
        else if ( strstr ( name, _FA_Region[1] ) ) {
	    _centP = npts;
	    for ( ii = 0; ii < npts; ii++ ) {
                _centX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                _centY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	    }		    
        }
	else if ( strstr ( name, _FA_Region[2] ) ) {
            _eastP = npts;
	    for ( ii = 0; ii < npts; ii++ ) {
                _eastX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                _eastY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	    }
	}		    
        else if ( strcasecmp ( bounds, "FA_AREAX_BNDS" ) == 0 ) {
            if ( strstr ( name, "SLC" ) ) {
                _slcP = npts;
	        for ( ii = 0; ii < npts; ii++ ) {
                    _slcX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                    _slcY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	        }	    	    
            }        
            else if ( strstr ( name, "SFO" ) ) {
                _sfoP = npts;
	        for ( ii = 0; ii < npts; ii++ ) {
                    _sfoX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                    _sfoY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	        }		    
            }        
            else if ( strstr ( name, "CHI" ) ) {
                _chibosP = npts;
	        for ( ii = 0; ii < npts; ii++ ) {
                    _chibosX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                    _chibosY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	        }		    
            }        
            else if ( strstr ( name,"DFW" ) ) {
                _dfwmiaP = npts;
	        for ( ii = 0; ii < npts; ii++ ) {
                    _dfwmiaX [ ii ] = rint ( (double)xlat[ ii ] * 100 ) / 100;
                    _dfwmiaY [ ii ] = rint ( (double)ylon[ ii ] * 100 ) / 100;
	        }		    
            }
	}        
    }
     
    while ( next == 0 ) {

        /*
         * Convert bounds coordinate system to sys_N
         */
        G_MALLOC ( xnormal, float, npts, "af_bnds2poly: xnormal" );
        G_MALLOC ( ynormal, float, npts, "af_bnds2poly: ynormal" );

        gtrans ( sys_M, sys_N, &npts, xlat, ylon, xnormal, ynormal,
                 &ier, strlen(sys_M), strlen(sys_N) );


        /*
         * Create GPC polygon structures for bounds.
         */
        verts.vertex          = (gpc_vertex*)NULL;
        verts.num_vertices    = 0;

        gpc_cvlist ( npts, xnormal, ynormal, &verts, &ier );
	
	
	/*
	 * Holes are not considered 
	 */
        gpc_add_contour ( polygon, &verts, G_FALSE );

	  
	/*
	 * Free memory
	 */
        free ( verts.vertex );
        G_FREE ( xnormal, float );
        G_FREE ( ynormal, float );

        clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &next );
    }
  
    G_FREE ( xlat, float );
    G_FREE ( ylon, float );

}

/*=====================================================================*/

void af_reg2intl( int *iret )
/************************************************************************
 * af_reg2intl                                                          *
 *                                                                      *
 * This function computes the GPC_UNION of three regional bounds        *
 * into one international bound.                                        *
 *                                                                      *
 * void af_reg2intl ( iret )                      		        *
 *                                                                      *
 * Input parameters:                                                    *
 * Input/Output parameters:                                             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		04/06	Created                            	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06	remove holes from the union         	*
 ***********************************************************************/
{
    int			ii;
    
    gpc_polygon		poly0;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    gpc_polygon_clip ( GPC_UNION, &_regionBndPoly[2], &_regionBndPoly[1], &poly0);
    gpc_polygon_clip ( GPC_UNION, &_regionBndPoly[0], &poly0, &poly0 );
    
    _intlBndPoly.num_contours = 0;
    _intlBndPoly.hole         = (int*)NULL;
    _intlBndPoly.contour      = (gpc_vertex_list*)NULL;

    for ( ii = 0; ii < poly0.num_contours; ii++ ) {
        if ( poly0.hole[ ii ] == G_FALSE ) {
	    gpc_add_contour( &_intlBndPoly, &poly0.contour[ ii ], G_FALSE );
	}
    }
   
    gpc_free_polygon ( &poly0 );
}

/*=====================================================================*/

void af_setBoundInt ( int *iret )
/************************************************************************
 * af_setBoundInt                                                       *
 *                                                                      *
 * This routine finds and saves the common boundary points between WEST	*
 * and CENTRAL region, and those between EAST and CENTRAL region, as 	*
 * as those between extended FA area bounds.				*
 *                                                                      *
 * void af_setBoundInt ( iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      None								*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret	int             Return code                     	*
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/06   Created                         	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		05/07   find common points btwn extended areas	*
 ***********************************************************************/
{
    int 	ii, jj;    
/*--------------------------------------------------------------------*/
            
    *iret = 0;
    
    /*
     *  Find common points between W & C, E & C FA region boundaries.
     */
    _nwc = 0;
    _nec = 0;
    for ( ii = 0; ii < (_centP-1); ii++ ) {       	
	for ( jj = 0; jj < (_westP-1); jj++ ) {	    
	    if ( G_DIST ( _centX[ii], _centY[ii],
	                  _westX[jj], _westY[jj] ) < SMALLF ) {
	        _wcX[ _nwc ] = _centX[ ii ];    
	        _wcY[ _nwc ] = _centY[ ii ]; 	
		(_nwc)++;
	    }           
	}

	for ( jj = 0; jj < (_eastP-1); jj++ ) {	    
	    if ( G_DIST ( _centX[ii], _centY[ii], 
	                 _eastX[jj], _eastY[jj] ) < SMALLF ) {
	        _ecX[ _nec  ] = _centX[ ii ];    
	        _ecY[ _nec ] = _centY[ ii ];
	        (_nec)++;
	    }           
	}
    }

    /*
     *  Find common points between extended SFO & SFC area bounds,
     *  as well as CHI_BOS & DFW_MIA.
     */
    _nextdw= 0;
    for ( ii = 0; ii < (_slcP-1); ii++ ) {       	
	for ( jj = 0; jj < (_sfoP-1); jj++ ) {	    
	    if ( G_DIST ( _slcX[ii], _slcY[ii],
	                  _sfoX[jj], _sfoY[jj] ) < SMALLF ) {
	        _extdwX[ _nextdw ] = _slcX[ ii ];    
	        _extdwY[ _nextdw ] = _slcY[ ii ]; 	
		(_nextdw)++;
	    }           
	}
    }
    
    
    _nextdc = 0;
    for ( ii = 0; ii < (_chibosP-1); ii++ ) {       	
	for ( jj = 0; jj < (_dfwmiaP-1); jj++ ) {	    
	    if ( G_DIST ( _chibosX[ii], _chibosY[ii], 
	                 _dfwmiaX[jj], _dfwmiaY[jj] ) < SMALLF ) {
	        _extdcX[ _nextdc  ] = _chibosX[ ii ];    
	        _extdcY[ _nextdc ] = _chibosY[ ii ];
	        (_nextdc)++;
	    } 
	}
    }
}

/*=====================================================================*/

void af_loadCentroids ( float xCentroid[ NUM_FA_AREAS ],
                               float yCentroid[ NUM_FA_AREAS ], int *iret )
/************************************************************************
 * af_loadCentroids                                                     *
 *                                                                      *
 * This routine gets the centroids of the input polygons. Assume each   *
 * polygon has only one contour. The input polygons are in normalized	*
 * coordinate systems. The output (x,y) are also normalized coords.	*
 *                                                                      *
 * void af_loadCentroids ( xCentroid, yCentroid, iret ) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *xCentroid      float           x array of the centroid       	*
 *      *yCentroid      float           y array of the centroid       	*
 *	*iret		int		return code 0: normal		*
 *                                                                      *
 * Return parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           3/06   Created                                 *
 * M. Li/SAIC		 4/06	Removed input				*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * E. Safford/SAIC	08/07	rm gtrans to Map coords			*
 ***********************************************************************/
{
    int         ii, kk, np, ier, one;
    float	dummy, *xPoly, *yPoly;
/*---------------------------------------------------------------------*/
     
    *iret = 0;
    one   = 1;
    xPoly = NULL;
    yPoly = NULL;

    /*
     *  Loop over all FA areas and find the centroid of each area.
     */
    for ( ii = 0; ii < NUM_FA_AREAS; ii++ ) {
                                                                                 
        if ( _areaBndPoly[ ii ].num_contours <= 0 ) {
                                                                                  
           xCentroid[ ii ] = RMISSD;
           yCentroid[ ii ] = RMISSD;
                                                                               
        }
                                                                                
        else {
                                                                                   
            G_MALLOC ( xPoly, float, _areaBndPoly[ ii ].contour[ 0 ].num_vertices,
                       "AF_LOADCENTROIDS xPloy" );
            G_MALLOC ( yPoly, float, _areaBndPoly[ ii ].contour[ 0 ].num_vertices,
                       "AF_LOADCENTROIDS yPoly" );
                                                                                 
            /*
             *  Pull all points from the contour and get the centroid.
             */
            for ( kk = 0; kk < _areaBndPoly[ ii ].contour[ 0 ].num_vertices; kk++ ) {
                                                                                   
                xPoly[ kk ] = _areaBndPoly[ ii ].contour[ 0 ].vertex[ kk ].x;
                yPoly[ kk ] = _areaBndPoly[ ii ].contour[ 0 ].vertex[ kk ].y;
                                                                                 
            }
                                                                               
            np = _areaBndPoly[ ii ].contour[ 0 ].num_vertices;
                                                                                 
            cgr_centroid ( xPoly, yPoly, &np, &xCentroid[ ii ], &yCentroid[ ii ],
                           &dummy, &ier );

            G_FREE ( xPoly, float );
            G_FREE ( yPoly, float );

        }
                                                                                
    }

}
                                                                                      
/*=====================================================================*/
