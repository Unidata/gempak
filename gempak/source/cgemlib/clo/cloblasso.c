#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

void clo_blasso ( char *bndtyp, char *key, int *npts, char *btags, 
		gpc_polygon *union_poly, int *iret )
/************************************************************************
 * clo_blasso                                                           *
 *                                                                      *
 * This function takes a string of bound tags, semi-colon separated, and*
 * returns a polygon structure from the union of the polygons designated*
 * by the bound tags.  A return code of 1 due to a bad bounds key tag	*
 * could mean the tag name (eg., <FIPS>) is invalid and/or the tag value*
 * (eg., 51059 -- a FIPS code) is invalid.  Processing continues, so the*
 * invoking routine may	also want to check the number of polygons for 0,*
 * in which case it would most likely be caused by an invalid tag name.	*
 *									*
 * Note that the tag name may or may not be enclosed with '<' and '>'.  *
 * If it is not, they will be added to the tag name.			*
 *                                                                      *
 * clo_blasso ( bndtyp, key, npts, btags, union_poly, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*bndtyp		char	Bounds file alias name (eg., WBCMZ_BNDS)*
 * 	*key		char	Search tag name in bounds file		*
 *				 (eg., <FIPS> or FIPS, <STATE> or STATE)*
 * 	*npts		int	No. of bounds tag values in string btags*
 * 	*btags		char	Bounds tag values, delimited by ";"	*
 *					(eg., 51059;51057;51053)	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*union_poly	gpc_polygon	GPC polygon structure of union	*
 *	iret		int	Return code				*
 *				 =  0	Normal				*
 *				 =  1	No bound area found for a tag  	*
 *					  due to bad tag name or value  *
 *				 = -1	Bounds file open error return	*
 *					  or illegal bounds name	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP        1/05                                           *
 * F. J. Yen/NCEP	 2/05	Increased size of btagkey; Enclosed tag *
 *				name with '<' and '>' if missing.	*
 * J. Wu/SAIC            6/05   remove reference to LLMXPT              *
 * F.Yen&D.Plummer/NCEP  7/05   Redesigned for performance.		*  
 * D.W.Plummer/NCEP	 8/05	Add final GPC_UNION to rm spurious pts	*
 * S. Guan              9/16    Set GPC_EPSILON to 0.001 instead of     * 
 *                              using  default 0.01 in gpc.h to fix WOU *
 *                              issue (erroneously depicts the outline) *
 ***********************************************************************/

{
    char	**arrptr, btagkey[80];
    char	*tag_start = { "<" };
    char	*tag_end   = { ">" };
    int		ii,  maxexp, minp, ier, ierro, mxpts, end;
    int		narr, numTmp, len, hole, initl;
    float       *xTmp, *yTmp;
    float	rlatmn, rlonmn, dlatmx, dlonmx, filter;
    gpc_polygon	polygon;
    gpc_vertex_list	verts;
/*---------------------------------------------------------------------*/

    *iret  = 0;
    ier    = 0;
    minp   = 0;
    hole   = 0;
    filter = 0.0;
    maxexp = 400;
    /* 
     * Initalize the gpc polygon structure variables 
     */
    union_poly->num_contours	= 0;
    union_poly->hole		= (int *)NULL;
    union_poly->contour	= (gpc_vertex_list *)NULL;
    clo_bstype ( bndtyp , &ier );  /* Set the bounds file */
    if ( ier != 0 ) {
	*iret = -1;
	return;
    }
    /*
     * Set the bounds area
     */
    rlatmn = -90.0;
    dlatmx = +90.0;
    rlonmn = -180.0;
    dlonmx = +180.0;
    clo_bsarea ( &rlatmn, &rlonmn, &dlatmx, &dlonmx, &ier ); 
    /*
     * Initialize the clo library
     */
    clo_init ( &ier);
    /*
     * Allocate memory
     */
    clo_qmxpts ( "BOUNDS", &mxpts, &ier );
    G_MALLOC ( xTmp, float, mxpts, "CLO_BLASSO" );
    G_MALLOC ( yTmp, float, mxpts, "CLO_BLASSO" );
    /*  
     * Break apart the bounds tag values 
     */
    arrptr = (char **)malloc(*npts * sizeof(char *));
    for (ii = 0; ii < *npts; ii++) {
	arrptr[ii] = (char *) malloc(maxexp);
    }
    cst_clst ( btags, ';', " ", *npts, maxexp, arrptr, &narr, &ier );
    /* 
     * initialize single bounds polygon 
     */
    polygon.num_contours	= 0;
    polygon.hole		= (int *)NULL;
    polygon.contour		= (gpc_vertex_list *)NULL;
    /* 
     * Loop over bounds tag values 
     */
    for (ii=0;ii < narr; ii++) {
	cst_rmbl ( arrptr[ii], arrptr[ii],  &len, &ier );
	if ( len > 0 ) {
	    ierro  = 0;
	    /* 
	     * Create the key tag to search on ( eg. <FIPS>51097 ).  First,
	     * check for '<' at beginning of tag.  Add it if it isn't there.
	     */
	    btagkey[0] = '\0';
	    if( key[0] != '<' ) {
        	strcpy( btagkey, tag_start );
	    }
	    strcat ( btagkey, key );
	    /*
	     *  Check for '>' at end of key tag.  Add it if it isn't there.
	     */
	    end = strlen ( key );
	    if ( key [ end - 1 ] != '>' ) {
		strcat ( btagkey, tag_end );
	    }
	    strcat ( btagkey, arrptr[ii]);
    	    clo_bstype ( bndtyp , &ier );  /* Set the bounds file */
	    clo_bstag ( btagkey, &ier );   /* Set the bounds key tag
					      (eg <FIPS>51097 ) */
	    /*
	     * Find the bound key tag polygon
	     */
	    initl = 1;
	    while ( ierro == 0 ) {
		/*
		 * Get next bounds
		 */
		clo_bgnext (&minp, &mxpts, &filter, &numTmp, xTmp, yTmp,
			    &ierro ); 
		if ( initl == 1 ) {
		    initl = 0;
		    if ( ierro != 0 ) {
			/*
			 * No bound area (polygon) found for current bounds
			 * key tag in btagkey due to either an invalid tag
			 * name (eg, <FIPS> or <STATE>) and/or invalid tag
			 * value (ie, if the tag name is <FIPS>, the tag
			 * value would be a FIPS code such as 13009).
			 */
			*iret = 1;
		    }
		}
	        if ( ierro == 0 ) {
		  /* 
		   * initialize vertex list
		   */
		  verts.vertex	    = (gpc_vertex*)NULL;
		  verts.num_vertices	=0;
		  gpc_cvlist (numTmp, xTmp, yTmp, &verts, &ier );
		  gpc_add_contour ( &polygon, &verts, hole);
		  free ( verts.vertex );
	        }
	    }  /* end for while loop */
	}   /* end for length check */
    }   /* end for loop */
    /*
     * union the polygon with a NULL polygon (union) to get the union
     */
    gpc_set_epsilon(0.001);
    gpc_polygon_clip (GPC_UNION, &polygon, union_poly, union_poly );
    gpc_free_polygon ( &polygon);
    gpc_polygon_clip (GPC_UNION, &polygon, union_poly, union_poly );
    /*
     * Free memory space.
     */
    G_FREE ( xTmp, float );
    G_FREE ( yTmp, float );
    for ( ii = 0; ii < *npts; ii++ ) {
        free ( arrptr[ii] );
    }
    free ( arrptr);
}
