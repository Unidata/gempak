#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

#define		MAX_NUM_BNDS	( 16 )
#define		PART_INCR	( 1000 )
#define		PTS_INCR	( 10000 )

/*
 *  Initialization flag for this module
 */
static int	_init		= G_FALSE;


/*
 *  Private structure to hold bound information
 */
typedef struct bndsprt_t  {
    long        strec;          /* Starting record of boundary part	*/
    float       minlat;         /* Minimum latitude of boundary part 	*/
    float       minlon;         /* Minimum longitude of boundary part 	*/
    float       maxlat;         /* Maximum latitude of boundary part 	*/
    float       maxlon;         /* Maximum longitude of boundary part 	*/
    int         npts;           /* Number of points in the part		*/

} Bndsprt_t;

typedef struct binfo_t  {
    char        name[LLMXLN];   /* Boundary name                   	*/
    int		isVGF;		/* Boundary is/is not a VGF file	*/
    float       minlat;         /* Minimum latitude of boundary 	*/
    float       minlon;         /* Minimum longitude of boundary 	*/
    float       maxlat;         /* Maximum latitude of boundary 	*/
    float	maxlon;         /* Maximum longitude of boundary	*/
    int		nparts;		/* Number of boundary parts		*/
    Bndsprt_t	*bndspt;        /* Boundary information                 */

} BInfo_t;

typedef struct bnd_t  {
    int		nbnd;	 	/* Number of boundaries overall 	*/
    BInfo_t	*bound;		/* Boundary information structure	*/
    
} Bnd_t;


static Bnd_t	_curBnds;		/* Last-saved bounds information	*/


/*
 *  Glabal arrays to hold all bound coordinates
 */
static int	_totalPts	= 0;	
static float	*_bndLat	= (float *)NULL;	
static float	*_bndLon	= (float *)NULL;	


/*
 *  Pointers used to read bounds (closed lines) from a VG file
 */
static FILE	*_ifptr		= (FILE *)NULL;
static int	_curPos		= 0;


/*
 *  Private functions
 */
void oa_bndchk ( float *x1, float *y1, float *x2, float *y2,
                int *intrsct, int *iret );
static void oa_bndfree ( void );
void oa_bndinb ( char *sysin, int *npts, float *xin, float *yin,
                 int *inout, int *iret );
void oa_bndset ( char *bnds_name, char *proj, float *ang,
                float *garea, int *iret );
static void oa_bndsgt ( float *x1, float *y1, float *x2, float *y2, 
		 int *intrsct, int *iret );
static void oa_bndvgf ( char *vgfn, int *np, float *lat, float *lon,
                       float *minlat, float *minlon,
                       float *maxlat, float *maxlon, int *iret );


/************************************************************************
 * oabnd.c                                                         	*
 *                                                                      *
 * This module contains functions used in OA programs to check the 	*
 * blocking bounds. The bounds could be either pre-defined bound area	*
 * or VG files containing closed lines. 				*
 * 									*
 * The bound blocking results in:					*
 *   (1) all grid points inside of the blocking bounds will be marked 	*
 *       as MISSED without objective analysis performed on them.	*
 *   (2) all stations inside of the bounds will not be used in analysis.*
 *   (3) if the line segment between a grid point and a station 	*
 *       intersects with the boundaries, that station will not be used	*
 *       in the analysis for that associated grid point.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *   oa_bndset()	Reads in blocking bounds' inforamtion		*
 *   oa_bndchk()	Checks if a segment intersects with boundaries	*
 *   oa_bndinb()	Checks if point(s) is inside of boundaries	*
 *                                                                      *
 *   oa_bndfree()	Frees memory					*
 *   oa_bndvgf()	Reads in bounds' info from a VG file		*
 *   oa_bndsgt()	Checks if two segments intersect		*
 ***********************************************************************/

void oa_bndset ( char *bnds_name, char *proj, float *ang, 
                float *garea, int *iret )
/************************************************************************
 * oa_bndset								*
 *									*
 * Reads in the specified blocking bounds' information and stores in	*
 * dynamic memory structure.  All non-valid bounds are automatically	*
 * filtered out and only valid bounds are loaded.  Multiple bound areas	*
 * can be specified via '+' sign between bounds name.  Default is NULL, *
 * means no bounds are specified.  Bound coordinates are transformed	*
 * and stored as sys_N for future access.  If bound names differ from 	*
 * last-saved, bounds information will be reset.  If bnds_name is NULL, *
 * all current bound information will be unset.  The bounds could be 	*
 * either pre-defined bound areas or VG files containing closed lines.	*
 *									*
 * void oa_bndset ( bnds_name, proj, ang, garea, iret )			*
 *									*
 * Input parameters:							*
 *	*bnds_name	char		Bound names seperated by '+'	*
 *	*proj		char		Projection type in grid file	*
 *	*ang		float[]		Projection angles in grid file	*
 *	*garea		float[]		Projection area in grid file	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	                                0 - Normal			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/05	initial coding				*
 * B. Yin/SAIC		05/05	set initial value for isVGF 		*
 ***********************************************************************/
{
    int		ii, jj, ier, minp, mxpts, lens, ierr, nn, matched;
    int		numTmp, prtCap, ptsCap, more, nprt, mxbnd;
    char	*ptr = (char *)NULL, newfil[256];
    char	*tptr = (char *)NULL, bound[32], tag[32];
    float	filter, xTmp[LLMXPT], yTmp[LLMXPT];
    float	minlat, minlon, maxlat, maxlon;
    long	maxbytes;
    
    Bnd_t	newBnd;            
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;


    /* 
     *  Initialize the bound structure.
     */
    if ( !_init ) {
        _curBnds.nbnd = 0;
	_init = G_TRUE;
    }
    
    
    /* 
     *  Initialize CLO library.
     */
    clo_init ( &ier );
        
    
    /* 
     *  Unset memory and return, if no bound input found. 
     */            
    if ( bnds_name == (char *)NULL || strlen( bnds_name ) == (size_t)0 ) { 
        oa_bndfree ();
	return;
    }
    

    /*
     *  Filter out the valid bound names from the input.
     */
    cst_rmbl ( bnds_name, bnds_name, &lens, &ier );
    
    newBnd.nbnd = 0;
    mxbnd = MAX_NUM_BNDS;
    G_MALLOC ( newBnd.bound, BInfo_t, (int)(mxbnd * sizeof(BInfo_t)),  "oa_bndset" );
    
    ptr = strtok ( bnds_name, "+" );            
    while ( ptr != (char *)NULL && newBnd.nbnd < MAX_NUM_BNDS ) {	
	newBnd.bound[newBnd.nbnd].isVGF = G_FALSE;
	if ( strstr ( ptr, ".vgf" ) ) {
	    cfl_inqr ( ptr, NULL, &maxbytes, newfil, &ier );
	    if ( ier == 0 )  {    	        	        
		strcpy ( newBnd.bound[newBnd.nbnd].name, ptr );
		newBnd.bound[newBnd.nbnd].isVGF = G_TRUE;
		newBnd.nbnd++;
	    }
	}
	else {
	    cst_lcuc ( ptr, ptr, &ier );
	    /*
	     * Strip actual bound name away from a possible tag.
	     */
	    tptr = cst_split ( ptr, '|', sizeof(bound), bound, &ier );
	    clo_bstype ( bound, &ier );
	    if ( ier == 0 ) {
	        /*
	         * Save entire bound input, including tag, for later use.
	         */
		strcpy ( newBnd.bound[newBnd.nbnd].name, ptr );
		newBnd.nbnd++;
	    }
	}

	ptr = strtok ( NULL, "+" );        
    }        
        	        
    
    /* 
     *  Keep the previous boundaries and return if no changes found
     *  in bounds' input. 
     */            
    matched = G_FALSE;
    for ( ii = 0; ii < newBnd.nbnd; ii++ ) {    
        for ( jj = 0; jj < _curBnds.nbnd; jj++ ) {
	    if ( strcmp ( newBnd.bound[ii].name, _curBnds.bound[jj].name ) == 0 ) {
		matched = G_TRUE;
		break;
	    }
	}	
	
	if ( !matched ) break;    
    }
    
    if ( matched ) {
        G_FREE ( newBnd.bound, BInfo_t );      
	return;       
    }
    
    oa_bndfree ( );        
    
    /* 
     *  If there are some changes in bounds, reset memory to load 
     *  new bound information.
     */            
    oa_bndfree ();
    
    prtCap = PART_INCR;
    ptsCap = PTS_INCR;
       
    bnds_name[0] = '\0';
    _curBnds.nbnd = newBnd.nbnd;
    G_MALLOC ( _curBnds.bound, BInfo_t, (int)(newBnd.nbnd * 
	                     sizeof(BInfo_t)),  "oa_bndset" );
    for ( ii = 0; ii < newBnd.nbnd; ii++ ) {    
	strcpy ( _curBnds.bound[ii].name, newBnd.bound[ii].name );
	_curBnds.bound[ii].isVGF = newBnd.bound[ii].isVGF;
        G_MALLOC ( _curBnds.bound[ii].bndspt, Bndsprt_t, 
	               (int)(prtCap * sizeof(Bndsprt_t)), "oa_bndset" );
        
        /* 
          *  Return a valid bound string.
         */            
	strcat ( bnds_name, newBnd.bound[ii].name );
	if ( ii != ( newBnd.nbnd - 1 ) ) {
	    strcat ( bnds_name, "+" );
	}
    }	
            
    G_MALLOC ( _bndLat, float, (int)(ptsCap * sizeof(float)), "oa_bndset" );
    G_MALLOC ( _bndLon, float, (int)(ptsCap * sizeof(float)), "oa_bndset" );
        
    G_FREE ( newBnd.bound, BInfo_t );
        
    
    /* 
     *  Set up PROJ and GAREA identical with those in grid files.  
     */
    gsmprj ( proj, &ang[0], &ang[1], &ang[2], &garea[0], 
             &garea[1], &garea[2], &garea[3], &ier, strlen(proj) );

    clo_bsarea ( &garea[0], &garea[1], &garea[2], &garea[3], &ier );

    /* 
     *  Loop over all boundaries to load information. 
     */
    minp   = 0;
    mxpts  = LLMXPT;
    filter = 0.0F;
    _totalPts = 0;
    		                   
    for ( ii = 0; ii < _curBnds.nbnd; ii++ ) {
		
	more = G_TRUE;	    
        if ( !_curBnds.bound[ii].isVGF ) {
	    tptr = cst_split ( _curBnds.bound[ii].name, '|', 
		    sizeof(bound), bound, &ier );
	    clo_bstype ( bound, &ier );
            if ( ier != 0 )  {
		more = G_FALSE; 
	    }
	    else  {
		if ( tptr != (char *)NULL )  {
	    	    tptr = cst_split ( tptr, '|', sizeof(tag), tag, &ier );
		    if ( strlen(tag) > (size_t)0 )  {
			clo_bstag ( tag, &ier );
            		if ( ier != 0 )  {
			    /*
			     * Process true/false info here...
			     */
			    more = G_FALSE; 
	    		}
		    }
		}
	    }
	}
	        		
	nprt = 0;
	while ( more ) {
	        
	    if ( _curBnds.bound[ii].isVGF ) {
                oa_bndvgf ( _curBnds.bound[ii].name, &numTmp, xTmp, yTmp, 
		            &minlat, &minlon, &maxlat, &maxlon, &ierr );	
	    }
	    else {		
		clo_bgnext ( &minp, &mxpts, &filter, &numTmp, xTmp, yTmp, &ierr );
	        if ( ierr == 0 ) {
		    clo_bgrange ( &minlat, &minlon, &maxlat, &maxlon, &ierr );
		}
	    }
		
	    if ( ierr != 0 ) {	        		    
	        more = G_FALSE;
	    }
	    else {        
		
		nn = _totalPts + numTmp;		    
		    
	        /*
                  *  Increase the memory block if necessary.
		  */
		if ( nn > ptsCap ) {
		    ptsCap = ( nn/PTS_INCR + 1 ) * PTS_INCR;
		    G_REALLOC ( _bndLat, float, 
			        (int)(ptsCap * sizeof(float)), "oa_bndset" );
	            G_REALLOC ( _bndLon, float, 
			        (int)(ptsCap * sizeof(float)), "oa_bndset" );
		}
                    
		if ( nprt > prtCap ) {
		    prtCap = ( nprt/PART_INCR + 1 ) * PART_INCR;
		    G_REALLOC ( _curBnds.bound[ii].bndspt, Bndsprt_t, 
			       (int)(prtCap * sizeof(Bndsprt_t)), "oa_bndset" );
		}
		    		    		
                /* 
                  *  Transform bound points to normalized coordinates. 
                  */
		gtrans ( sys_M, sys_N, &numTmp, xTmp, yTmp,
                         xTmp, yTmp, &ier, strlen(sys_M), strlen(sys_N) );


		/*
		  *  Save the coordinates and range record for each part.
		  */
                memmove ( &_bndLat[_totalPts], &xTmp, (size_t)numTmp*sizeof(float) );
                memmove ( &_bndLon[_totalPts], &yTmp, (size_t)numTmp*sizeof(float) );
               	    	                      	    	       
		_curBnds.bound[ii].bndspt[nprt].strec = _totalPts;		
		_curBnds.bound[ii].bndspt[nprt].npts = numTmp;		
	        
		_curBnds.bound[ii].bndspt[nprt].minlat = minlat;
	        _curBnds.bound[ii].bndspt[nprt].minlon = minlon;
	        _curBnds.bound[ii].bndspt[nprt].maxlat = maxlat;
	        _curBnds.bound[ii].bndspt[nprt].maxlon = maxlon;		
		
		if ( nprt == 0 ) {
	            _curBnds.bound[ii].minlat = minlat;
	            _curBnds.bound[ii].minlon = minlon;
	            _curBnds.bound[ii].maxlat = maxlat;
	            _curBnds.bound[ii].maxlon = maxlon;				    
		}
		else { 
		    _curBnds.bound[ii].minlat = G_MIN ( _curBnds.bound[ii].minlat, minlat );
		    _curBnds.bound[ii].minlon = G_MIN ( _curBnds.bound[ii].minlon, minlon );
		    _curBnds.bound[ii].maxlat = G_MAX ( _curBnds.bound[ii].maxlat, maxlat );
		    _curBnds.bound[ii].maxlon = G_MAX ( _curBnds.bound[ii].maxlon, maxlon );
		}
		
		_totalPts += numTmp;		    
		    		    		
		nprt++;		    		    	    
	    }
	    
	    _curBnds.bound[ii].nparts = nprt;		
	    	     	    	    
	}          
    }                
}

/*======================================================================*/

void oa_bndchk ( float *x1, float *y1, float *x2, float *y2, 
		int *intrsct, int *iret )
/************************************************************************
 * oa_bndchk								*
 *									*
 * Check if the giving segment intersects the "blocking" boundaries.	*
 *									*
 * Note: the caller must use normalized coordinates as input.		*
 *									*
 * void oa_bndchk ( x1, y1, x2, y2, intrsct, iret )			*
 *									*
 * Input parameters:							*
 *	*sysin		char		Input coordinate system		*
 *	*x1		float		x coord. of the seg. start point*
 *	*y1		float		y coord. of the seg. start point*
 *	*x2		float		x coord. of the seg. end point	*
 *	*y2		float		y coord. of the seg. end point	*
 *									*
 * Output parameters:							*
 *	*intrsct	int		1/0 - yes/no for intersection	*
 *	*iret		int		Return code			*
 *					0 = normal			*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/05	initial coding				*
 ***********************************************************************/
{
    int		ii, jj, kk, ier, start, end;
    float	llx, lly, urx, ury, xs[2], ys[2], xb[2], yb[2];
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
                    
    /*
     *  Build a bounding box for the segment.
     */
    llx = G_MIN ( *x1, *x2 );
    lly = G_MIN ( *y1, *y2 );
    urx = G_MAX ( *x1, *x2 );
    ury = G_MAX ( *y1, *y2 );
        
    xs[0] = *x1;
    ys[0] = *y1;
    xs[1] = *x2;
    ys[1] = *y2;
    

    /*
     *  Check if the segment intersects the boundaries - try to reduce
     *  the number of checks, especially the one deep in the loop.
     */
    for ( ii = 0; ii < _curBnds.nbnd; ii++ ) {	

        /*
         *  First check if the segment's bounding box intersects the 
         *  range box of this bound.
	 */
        if ( cgr_ntrsct ( llx, lly, urx, ury, 
	     _curBnds.bound[ii].minlat, _curBnds.bound[ii].minlon,
             _curBnds.bound[ii].maxlat, _curBnds.bound[ii].maxlon, &ier ) ) {
        
	    for ( jj = 0; jj < _curBnds.bound[ii].nparts; jj++ ) {
                
                /*  
		  *  Second,  check if the bounding box intersects the
                  *  range box of this part.
                  */
	        if ( cgr_ntrsct ( llx, lly, urx, ury, 
	                _curBnds.bound[ii].bndspt[jj].minlat, 
		        _curBnds.bound[ii].bndspt[jj].minlon,
                        _curBnds.bound[ii].bndspt[jj].maxlat, 
			_curBnds.bound[ii].bndspt[jj].maxlon, &ier ) ) {
                
		    start = _curBnds.bound[ii].bndspt[jj].strec;
	            end   = start + _curBnds.bound[ii].bndspt[jj].npts - 1;
	     
	            
                    /*  
		       *  Finally, check if the segment actaully intersects 
		       *  this part - this is the most expensive check.
                       */
		    for ( kk = start; kk < end; kk++ ) {
	                xb[0] = _bndLat[kk];
                        yb[0] = _bndLon[kk];
                        xb[1] = _bndLat[kk+1];
                        yb[1] = _bndLon[kk+1];
            
	                oa_bndsgt ( xs, ys, xb, yb, intrsct, &ier );
          	    	    
	                if ( *intrsct ) return;
		    }
		}
	    }
	}	           
    }    

    *intrsct = G_FALSE;
}

/*======================================================================*/

void oa_bndinb ( char *sysin, int *npts, float *xin, float *yin, 
                 int *inout, int *iret )
/************************************************************************
 * oa_bndinb								*
 *									*
 * Check if the giving points are inside or outside of boundaries.	*
 *									*
 * void oa_bndinb ( sysin, npts, xin, yin, inout, iret )		*
 *									*
 * Input parameters:							*
 *	*sysin		char		Input points coordinate system	*
 *	*npts		int		number of points to test	*
 *	*xin		float[]		x coord. of test points		*
 *	*yin		float[]		y coord. of test points		*
 *	*inout		int[]		Array of in or out results	*
 *  					0  =  outside of boundaries	*
 *  					1  =  inside of boundaries	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 = normal			*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/05	initial coding				*
 * B. Yin/SAIC		05/05	fixed a bug(sysyin is a char in Fortran)*
 ***********************************************************************/
{
    int		ii, jj, kk, ier, npls, *ninout, nst;
    char	sysinStr[ 2 ];
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    sysinStr[ 0 ] = sysin[ 0 ];
    sysinStr[ 1 ] = '\0';
    /*
     *  Default as outside of bounds.
     */
    for ( ii = 0; ii < *npts; ii++ ) {
        inout[ii] = G_FALSE;
    }
                    
    G_MALLOC ( ninout, int, (int)(*npts * sizeof(int)), "oa_bndinb" );
    for ( ii = 0; ii < *npts; ii++ ) {
        ninout[ii] = G_FALSE;
    }
       
    /*
     *  Check if the points are inside or outside of bounds.
     */
    for ( ii = 0; ii < _curBnds.nbnd; ii++ ) {	            
        for ( jj = 0; jj < _curBnds.bound[ii].nparts; jj++ ) {	            
            npls = _curBnds.bound[ii].bndspt[jj].npts;
	    nst = _curBnds.bound[ii].bndspt[jj].strec;
	    
	    cgr_inpoly ( sysinStr, npts, xin, yin, sys_N, &npls,
		         &_bndLat[nst], &_bndLon[nst], 
		         ninout, &ier );
        
	    for ( kk = 0; kk < *npts; kk++ ) {
                if ( ninout[kk] == G_TRUE )  inout[kk] = G_TRUE;
	    }
	}	      
    }

    G_FREE ( ninout, int );	    

}

/***********************************************************************/

static void oa_bndfree ( void )
/************************************************************************
 * oa_bndfree								*
 *									*
 * This function frees memory used to hold the bound information.	*
 *									*
 * static void oa_bndfree ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/05	initial coding				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/
    
    /*
     *  Free the bound information structure.
     */
    for ( ii = 0; ii < _curBnds.nbnd; ii++ ) {
	G_FREE ( _curBnds.bound[ii].bndspt, Bndsprt_t );	
	_curBnds.bound[ii].nparts = 0;	
    }
    
    G_FREE ( _curBnds.bound, BInfo_t );
    
    _curBnds.nbnd = 0;		

    /*
     *  Free the bound coordinates arrays.
     */
    _totalPts = 0;
    G_FREE ( _bndLat, float );
    G_FREE ( _bndLon, float );	    
    
}

/***********************************************************************/

static void oa_bndvgf ( char *vgfn, int *np, float *lat, float *lon,
                       float *minlat, float *minlon, float *maxlat,
		       float *maxlon, int *iret )
/************************************************************************
 * oa_bndvgf								*
 *									*
 * This function reads bound coordinates from a VGF file.		*
 *									*
 * static void oa_bndvgf ( vgfn, np, lat, lon, minlat, minlon, maxlat,	*
 *                         maxlat, maxlon, iret )			*
 *									*
 * Input parameters:							*
 *	*vgfn		char		VG file to read			*
 *									*
 * Output parameters:							*
 *	*np		int		number of points on line	*
 *	*lat		float[]		line latitudes 			*
 *	*lon		float[]		line longitudes			*
 *	*minlat		float		Minimum latitude		*
 *	*minlon		float		Minimum longitude		*
 *	*maxlat		float		Maximum latitude		*
 *	*maxlon		float		Maximum longitude		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/05	initial coding				*
 ***********************************************************************/
{
    int		ii, more, ier;
    char	ifname[LLMXLN];
    long	ifilesize;
    VG_DBStruct	el;
    FILE	*fp;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    
    /*
     *   Check if the input file is an valid VGF file.
     */
    if ( _ifptr == (FILE *) NULL ) {
        cvg_open ( vgfn, G_FALSE, &(fp), &ier );
        if ( ier != 0 )  {
            *iret = -1;
	    return;
        }
	
	cfl_clos ( fp, &ier );    
    }
        

    /*
     *   Open the file for reading.
     */
    cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );
    if ( _ifptr == (FILE *) NULL ) {
        _ifptr = (FILE *) cfl_ropn ( ifname, "", &ier);
    }
    

    /*
     *   Read the next available line element.
     */
    more = G_TRUE;        
    while ( more == G_TRUE )  {

	cvg_rdrecnoc ( ifname, _ifptr, _curPos, &el, &ier );

        if ( ier < 0 )  {
            cfl_clos ( _ifptr, &ier );	    
            
	    more = G_FALSE;
            _curPos = 0;	    
	    _ifptr = (FILE *)NULL;
	    *iret = -1;
	    return;
	}
        else if ( el.hdr.recsz > 0 )  {
	    _curPos += el.hdr.recsz;
            
	    if ( el.hdr.vg_type == LINE_ELM || el.hdr.vg_type == SPLN_ELM ) {    
                
		if ( el.hdr.vg_type == LINE_ELM ) {
		    *np = el.elem.lin.info.numpts;
                    for ( ii = 0; ii < *np; ii++ )  {
                        lat [ ii ] = el.elem.lin.latlon [ ii ];
                        lon [ ii ] = el.elem.lin.latlon [ ii + (*np) ];
                    }	                    
                }
		else {
		    *np = el.elem.spl.info.numpts;
                    for ( ii = 0; ii < *np; ii++ )  {
                        lat [ ii ] = el.elem.spl.latlon [ ii ];
                        lon [ ii ] = el.elem.spl.latlon [ ii + (*np) ];
                    }	                    
		}
		
		
		/*
		  *  Close the line as a bound.
		  */
		lat [ *np ] = lat [ 0 ];
                lon [ *np ] = lon [ 0 ];
		
		(*np)++;		    
	        
		*minlat = el.hdr.range_min_lat;
		*minlon = el.hdr.range_min_lon;		
		*maxlat = el.hdr.range_max_lat;
		*maxlon = el.hdr.range_max_lon;

		return;
	    }
	}
    }
}

/************************************************************************/

static void oa_bndsgt ( float *x1, float *y1, float *x2, float *y2, 
		 int *intrsct, int *iret )
/************************************************************************
 * oa_bndsgt								*
 *									*
 * This function accepts two line segments and determines if they	*
 * intersect one another.  The line segments are assumed to be in 	*
 * normalized coordinate.  						*
 *									*
 * static void oa_bndsg ( x1, y1, x2, y2, intrsct, iret )		*
 *									*
 * Input parameters:							*
 *	*x1	float	X-coordinate of endpoints for segment #1	*
 *	*y1	float	Y-coordinate of endpoints for segment #1	*
 *	*x2	float	X-coordinate of endpoints for segment #2	*
 *	*y2	float	Y-coordinate of endpoints for segment #2	*
 *									*
 * Output parameters:							*
 *	*intrsct int	Result: 					*
 *			0-FALSE (the segments do not intersect),	*
 *			1-TRUE (the segments intersect)			*
 *	*iret	 int	Return code					*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	04/05	Adapted from cgr_segint()		*
 ***********************************************************************/
{
    float	x, y, m1, b1, m2, b2;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *intrsct = 0;

    x = RMISSD;
    y = RMISSD;


    /*
     *  Check for vertical first segment and compute (x,y) intersect.
     */
    if ( G_DIFF(x1[0], x1[1]) ) {

	x = x1[0];
	if ( G_DIFF(x2[0], x2[1]) )  return;
	m2 = (y2[1]-y2[0]) / (x2[1]-x2[0]);
	b2 = y2[0] - m2 * x2[0];
	y = m2 * x + b2;

    }

    
    /*
     *  Check for vertical second segment and compute (x,y) intersect.
     */
    else if ( G_DIFF(x2[0], x2[1]) ) {

	x = x2[0];
	if ( G_DIFF(x1[0], x1[1]) )  return;
	m1 = (y1[1]-y1[0]) / (x1[1]-x1[0]);
	b1 = y1[0] - m1 * x1[0];
	y = m1 * x + b1;

    }

    
    /*
     *  Finally compute (x,y) intersect for all other cases.
     */
    else {

    	m1 = (y1[1]-y1[0]) / (x1[1]-x1[0]);
    	b1 = y1[0] - m1 * x1[0];

    	m2 = (y2[1]-y2[0]) / (x2[1]-x2[0]);
    	b2 = y2[0] - m2 * x2[0];

	if ( G_DIFF(m1, m2) )  {
	    x = RMISSD;
	    y = RMISSD;
	}
	else  {
	    if ( G_DIFFT(m1, 0.0F, 0.01) )  {
	        x = ( b2 - y1[0] ) / ( - m2 );
	        y = y1[0];
	    }
	    else if ( G_DIFFT(m2, 0.0F, 0.01) )  {
	        x = ( y2[0] - b1 ) / ( m1 );
	        y = y2[0];
	    }
	    else  {
	        x = ( b2 - b1 ) / ( m1 - m2 );
	        y = m1 * x + b1;
	    }
	}

    }

    /*
     *  Check if intersecting point is within each segment's bounds.
     */
    if ( ERMISS(x) || ERMISS(y) )  return;

    if ( x < G_MIN(x1[0],x1[1]) || x > G_MAX(x1[0],x1[1]) )  return;
    if ( x < G_MIN(x2[0],x2[1]) || x > G_MAX(x2[0],x2[1]) )  return;
    if ( y < G_MIN(y1[0],y1[1]) || y > G_MAX(y1[0],y1[1]) )  return;
    if ( y < G_MIN(y2[0],y2[1]) || y > G_MAX(y2[0],y2[1]) )  return;

    *intrsct = 1;

}

/***********************************************************************/
