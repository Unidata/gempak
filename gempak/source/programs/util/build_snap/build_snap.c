#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

#define	MAX_PTS			( 3000 )
#define	BUBBLE_RADIUS		( 250.0F )
#define BUBBLE_RADIUS_INT	( 250 )
#define	BUBBLE_DENSITY		( 1.0F )
#define	CLOSEST_DIST		( 300.0F ) 

#define	VOR_TABLE		"vors.tbl"
#define	COORDPTS_TABLE		"coordPts.tbl"
#define	VOR_INFO		"VOR"
#define	VOR_EXCLUDE		"AK"

#define ANGLE_STEP		( 22.5 )
#define SNAP_POINT_STEP		( 10 )
#define DEFAULT_VOR_PT_RADIUS	( 20 )
#define DEFAULT_COORD_PT_RADIUS ( 10 )


/*
 *  VORS station table structure
 */
typedef struct vinfo_t
{
    char	*id;		/* Identifier  			*/
    int		nm;		/* Block/station number		*/
    char	*desc;		/* Description			*/
    char	*state;		/* State            		*/
    char	*cntry;		/* Country            		*/
    float	lat;		/* Latitude 			*/
    float 	lon;		/* Longitude			*/
    int 	elv;		/* Elevation			*/
    int 	pri;		/* Priority			*/
    char 	*col10;		/* Column 10			*/
    
} VInfo_t;

typedef struct vor_t
{
    int		nstn;		/* Number of vor stations 	*/
    VInfo_t	*station;	/* Vors station info structure	*/
    
} Vor_t;


/*  
 *  These two radii are the distances that are to be void of 
 *  snap points.  These initial values may be overriden by user 
 *  input values, if supplied.  See snap_usage() for more info.
 */
static int	_VOR_ptRadius	= DEFAULT_VOR_PT_RADIUS;		
static int	_COORD_ptRadius = DEFAULT_COORD_PT_RADIUS;


/*
 *  Private Functions
 */
static void snap_buildSnapTable ( char *outfile, int *iret );

static void snap_locateSnapPts ( float vLat, float vLon, 
			char *vorName, int npts, float *bLat,
			float *bLon, int *nSnap, float *snapLat, 
			float *snapLon, char **snapDesc, Vor_t *coordPts,
			int *iret );

static void snap_makeBubble ( float vorLat, float vorLon,
			int *numPts, float *bubbleLat, 
			float *bubbleLon, int *iret );

static void snap_makeVorRectangle ( float v0Lat, float v0Lon, 
			float v1Lat, float v1Lon, 
			float *rLat, float *rLon, int *iret );

static void snap_loadStnPts ( Vor_t *stn, char *fileName, int *iret );

static void snap_writeSnapTbl ( FILE *fp, int np, float *sLat, 
				float *sLon, char **snapDesc, 
				Boolean coord, int *iret );

static void snap_writeTblHeader ( FILE *fp, int *iret );

static Boolean snap_okToUsePt ( float lat, float lon, Vor_t *coordPts );
static void snap_usage ( void );

/************************************************************************
 * build_snap.c								*
 *									*
 * CONTENTS:								*
 * build_snap								*
 * snap_buildSnapTable		Build the snap table			*
 * snap_loadStnPts		Read in all VOR stations from vors.tbl	*
 * snap_locateSnapPts		Locate snap points within clipped bubble*
 * snap_makeBubble		Make a 100nm circle around a VOR point	*
 * snap_makeVorRetangle		Make a retangle from two VOR points	*
 * snap_writeSnapPts		Write snap points to snap table		*
 * snap_writeTblHeader		Write header information to snap table	*
 * snap_okToUsePt		Check if a pt is in 10nm to a coord pt	*
 * snap_usage			Print usage message			*
 ***********************************************************************/
 

int main ( int argc, char **argv )
/************************************************************************
 * build_snap								*
 *                                                                      *
 * This program creates a snap table - a station table of all "snapable"*
 * points relative to all VORs nationwide to be used for GFA snapping.	*
 *                                                                      *
 * int main ( argc, argv )                                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *	argc   		int      number of parameters of command line   *
 * 	argv   		char**   parameter array of command line	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	NONE                                            		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC	 	01/05	initial coding				*
 * E. Safford/SAIC	07/05	add command line parsing		*
 ***********************************************************************/
{
    int    	ier, argctr = 0, ch;
    char    	outfile[128];
/*---------------------------------------------------------------------*/


    /* 
     *  If no output file name provided, print the usuage message.
     *  Otherwise, build the snap table and writes to the output file.
     */
    if ( argc < 2 )  {
	snap_usage();
    }
    else { 

        ch = getopt( argc, argv, "HhV:v:C:c:" );

	while ( ch != EOF && ch != 255 ) {

	    switch( ch ) {
	        case 'V':
	        case 'v':
		    if( strlen( optarg ) > (size_t)0 ) {
          	        _VOR_ptRadius = atoi( optarg );
	                argctr++;
		    }
		    break;

		case 'C':
		case 'c':
		    if( strlen( optarg ) > (size_t)0 ) {
          	        _COORD_ptRadius = atoi( optarg );
	                argctr++;
		    }
		    break;

		case 'H':
		case 'h':
		    snap_usage();
		    return( 0 );

		default:
		    break;
            }
	    argctr++;
            
	    ch = getopt( argc, argv, "HhV:v:C:c:" );
        }

	argctr++;

  	if( argctr >= argc ) {
	    snap_usage();
	}
	else { 
            strcpy ( outfile, argv[optind] ); 

	    printf(" \n");
	    printf("  Building %s using \n", outfile);
	    printf("     VOR point radius          = %d\n", _VOR_ptRadius );
	    printf("     Coordination point radius = %d\n", _COORD_ptRadius );

            snap_buildSnapTable ( outfile, &ier );
  	} 
    } 
    
    return ( 0 );
}


/*=====================================================================*/

static void snap_buildSnapTable ( char *outfile, int *iret )
/************************************************************************
 * snap_buildSnapTable							*
 *									*
 * Build the snap table from "vors.tbl" and then output to file.	*
 *                                                                      *
 * static void snap_buildSnapTable ( outfile, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *     *outfile   	char	Output file name for the snap table	*
 *                                                                      *
 * Output parameters:                                                   *
 *     *iret		int	Return code				*
 *                                   0 - normal                 	*
 *                                  -1 - Unable to open output file	*
 *                                  -2 - Fail to load vors.tbl		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/05	initial coding				*
 * E. Safford/SAIC	06/05	increase allocation for sdesc array	*
 * B. Yin/SAIC		06/05	add coordination points in the snap tbl *
 * E. Safford/SAIC	10/05	fix hpux compile wrng on strlen() > 3	*
 ***********************************************************************/
{
    int		ii, jj, kk, ier, vorPts, nvor, bbPts;
    int		maxPts = MAX_PTS, nout, nclst;
    int		nrec = 4, ntmp, nfnl, nsnp;
    int		mode, istat, iunit, itype;
    
    float	xsize, ysize, lllat, lllon, urlat, urlon;
    float	prjang1, prjang2, prjang3;    
    float  	vortLat[MAX_PTS], vortLon[MAX_PTS];
    float	bbLat[MAX_PTS], bbLon[MAX_PTS];
    float	outLat[MAX_PTS], outLon[MAX_PTS];
    float	clstLat[MAX_PTS], clstLon[MAX_PTS], clstDis[MAX_PTS];
    float	rlat[MAX_PTS], rlon[MAX_PTS];
    float	tmpx[MAX_PTS], tmpy[MAX_PTS];
    float	fnlLat[MAX_PTS], fnlLon[MAX_PTS];
    float	slat[MAX_PTS], slon[MAX_PTS];
    
    char	device[8], dfilnam[128], pro[32];
    char	vname[32], **sdesc;
    
    Vor_t	vorstn, coordStn;
    
    FILE	*fpwrite;

/*---------------------------------------------------------------------*/
   
    *iret = 0;

    
    /*
     *  Set up GAREA and PROJ for polygon operations.
     */
    mode = 1;
    ginitp ( &mode, &istat, &ier );

    strcpy ( device, "GN" );

    iunit = 1;
    strcpy ( dfilnam, "CLIPVGF" );
    itype = 1;
    xsize = 500.0F;
    ysize = 500.0F;

    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
             strlen(device), strlen(dfilnam) );

    lllat = 0.0F;
    lllon = -135.0F;
    urlat = 0.0F;
    urlon = 45.0F;
    strcpy ( pro, "str" );
    prjang1 = 90.0F;  prjang2 = -105.0F;  prjang3 = 0.0F;
    gsmprj ( pro, &prjang1, &prjang2, &prjang3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen(pro) );


    /*
     *  Open the output file for writing.
     */
    fpwrite = (FILE *) cfl_wopn ( outfile, &ier );
    if ( ier != G_NORMAL ) {
        *iret = -1;
        printf ( "Unable to open file: %s\n", outfile );
	return;
    }
    

    /*
     *  Write the snap table header.
     */
    snap_writeTblHeader ( fpwrite, &ier );
            
    
    /*
     *  Fetch all VOR points, excluding those in Alaska!
     */
    snap_loadStnPts ( &vorstn, VOR_TABLE, &ier );    
    if ( ier != 0 ) {
        *iret = -2;
        printf ( "Fail to load: %s\n", VOR_TABLE );
	return;        
    }
    
    /*
     *  Fetch coordination points.
     */
    snap_loadStnPts ( &coordStn, COORDPTS_TABLE, &ier );    
    if ( ier != 0 ) {
        *iret = -2;
        printf ( "Fail to load: %s\n", COORDPTS_TABLE );
	return;        
    }
    
    vorPts = vorstn.nstn;
        
    nvor = 0;
    for ( ii = 0; ii < vorPts; ii++ ) {   
	if ( strcmp( vorstn.station[ii].state,
	             VOR_EXCLUDE ) != 0 ) {
	    vortLat[ii] = vorstn.station[ii].lat;
            vortLon[ii] = vorstn.station[ii].lon;
            nvor++;
	}
    }

    
    /*
     *  Loop over all VOR points to calculate snap points around
     *  each VOR point and write them to snap table.
     */    
    sdesc = (char **) malloc ( 400 * sizeof( char *) ) ;
    for ( ii = 0; ii < 400; ii++ ) {
        sdesc[ii] = (char *) malloc( 48 * sizeof( char ) );       
    }
    
    for ( ii = 0; ii < nvor; ii++ ) {
        
        /*
         *  Make a 100nm bubble around the VOR point.
	 */
        snap_makeBubble ( vortLat[ii], vortLon[ii], &bbPts,
		          bbLat, bbLon, &ier );
    	
       
        /*
         *  Get a list of the N closest neighboring VOR points. This
         *  will be the number of VOR points within 200NM of the VOR
         *  points being processed.
	 */
        nclst = 0;

        clo_dist ( &vortLat[ii], &vortLon[ii], &nvor, 
	           vortLat, vortLon, clstDis, &ier );
    
        for ( jj = 0; jj < nvor; jj++ ) {
	    if ( jj != ii && (clstDis[jj] * M2NM) < CLOSEST_DIST ) {
	        clstLat[nclst] = vortLat[jj];
	        clstLon[nclst] = vortLon[jj];
	        nclst++;
	    }
        }        


        /*
         *  For all N closest neighboring VOR points, make a rectangle
         *  and clip the bubble with the rectangle..
	 */
        nout = bbPts;
	for ( kk = 0; kk < bbPts;  kk++ ) {
            outLat[kk] = bbLat[kk];
            outLon[kk] = bbLon[kk];
        }
		
	for ( jj = 0; jj < nclst; jj++ ) {
	    snap_makeVorRectangle ( vortLat[ii], vortLon[ii], 
	                            clstLat[jj], clstLon[jj],
                                    rlat, rlon, &ier );           	    
            cgr_init ( &ier );
            cgr_polydiff ( &nout, outLat, outLon, &nrec, rlat, rlon,  
                           &maxPts, &ntmp, tmpx, tmpy, &ier );
            cgr_done ( &ier );
        
	    nout = ntmp;
	    for ( kk = 0; kk < ntmp;  kk++ ) {
                outLat[kk] = tmpx[kk];
                outLon[kk] = tmpy[kk];
            }
	                	    	    
	}
	
        
	/*
	 *  Locate all snap points within the final clipped bubble.
	 */            
        nfnl = nout;
	for ( kk = 0; kk < nfnl;  kk++ ) {
            fnlLat[kk] = outLat[kk];
            fnlLon[kk] = outLon[kk];
        }            	    	    
        
	strcpy ( vname, vorstn.station[ii].id );
        snap_locateSnapPts ( vortLat[ii], vortLon[ii], vname, 
                             nfnl, fnlLat, fnlLon, &nsnp, 
			     slat, slon, sdesc, &coordStn, &ier );
	
	/*
	 *  Write out the snap points around VOR point ii to snap table.
	 */            
        snap_writeSnapTbl ( fpwrite, nsnp, slat, slon, sdesc, False, iret );
    
    }    

    /*
     *  Add the coordination points
     */
    for ( ii = 0; ii < coordStn.nstn; ii++ ) {

	if ( strlen ( coordStn.station[ ii ].desc ) > (size_t) 3 ) {

	   cst_rpst ( coordStn.station[ ii ].desc, " ", "_", 
	   	      coordStn.station[ ii ].desc, &ier );  
           snap_writeSnapTbl ( fpwrite, 1, &coordStn.station[ ii ].lat, 
	   	&coordStn.station[ ii ].lon, &coordStn.station[ ii ].desc, 
		True, iret );

	}
    }


    /*
     *  clean up.
     */
    cfl_clos ( fpwrite, &ier );

    for ( ii = 0; ii < 400; ii++ ) {
        free ( sdesc[ii] );	      
    }
    
    free ( sdesc );
   
    
    for ( ii = 0; ii < vorstn.nstn; ii++ ) {
        free ( vorstn.station[ii].id );	      
        free ( vorstn.station[ii].desc );	      
        free ( vorstn.station[ii].state );	      
        free ( vorstn.station[ii].cntry );	      
        free ( vorstn.station[ii].col10 );	      
    }

    free ( vorstn.station );

}

/*=====================================================================*/

static void snap_makeBubble ( float vorLat, float vorLon, int *numPts,
			     float *bubbleLat, float *bubbleLon, 
			     int *iret )
/************************************************************************
 * snap_makeBubble							*
 *									*
 * Make a circle surrounding the given VOR point with a radius of       *
 * BUBBLE_RADIUS.							*
 *                                                                      *
 * static void snap_makeBubble ( vorLat, vorLon, numPts, bubbleLat,	*
 *			         bubbleLon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *     vorLat   	float	Latitude of the VOR point		*
 *     vorLon   	float	Longitude of the VOR point		*
 *                                                                      *
 * Output parameters:                                                   *
 *     *numPts   	int	Number of points bounding the bubble	*
 *     *outLat   	float	Latitudes of the bubble bounds		*
 *     *outLon   	float	Longitudes of the bubble bounds		*
 *     *iret		int	Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/05	initial coding				*
 ***********************************************************************/
{
    int		ii;
    float	lat, lon, distance, angle;
    
/*---------------------------------------------------------------------*/

    lat = vorLat;
    lon = vorLon;
    *numPts = (int) ( 360.0 / BUBBLE_DENSITY );
    
    /*
     *   Build a bubble, starting from N and going clockwise.
     */
    distance = BUBBLE_RADIUS * NM2M;
    for ( ii = 0; ii < *numPts; ii++ ) {        
	angle = BUBBLE_DENSITY * ii;
	clo_dltln ( &lat, &lon, &distance, &angle, 
		    &bubbleLat[ii], &bubbleLon[ii], iret );
    }

}
/*=====================================================================*/

static void snap_makeVorRectangle ( float v0Lat, float v0Lon, float v1Lat,
		float v1Lon, float *rLat, float *rLon, int *iret )
/************************************************************************
 * snap_makeVorRevtangle						*
 *									*
 * Make a retangle from VOR points v0 and v1.				*
 *                                                                      *
 * static void snap_makeVorRectangle ( v0Lat, v0Lon, v1Lat, v1Lon,	*
 *			     	       rLat, rLon, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *     v0Lat   		float	Latitude of central VOR point		*
 *     v0Lon   		float	Longitude of central VOR point		*
 *     v1Lat   		float	Latitude of neghboring VOR point	*
 *     v1Lon   		float	Longitude of neghboring VOR point	*
 *                                                                      *
 * Output parameters:                                                   *
 *     *rLat   		float	Latitudes of the retangle		*
 *     *rLon   		float	Longitudes of the retangle		*
 *     *iret		int	Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/05	initial coding				*
 ***********************************************************************/
{
    int		ier;
    float	dist, dir, mlat, mlon, ang;
/*---------------------------------------------------------------------*/

    *iret = 0;
    dist = BUBBLE_RADIUS * NM2M;
    
    /*
     *  Find the coordinates of the middle point (MP) bewteen v0 & v1.
     */
    mlat = ( v0Lat + v1Lat ) / 2.0;
    mlon = ( v0Lon + v1Lon ) / 2.0;
       
    /*
     *  Find the direction from v0 to v1 (degrees from N). 
     */
    clo_direct ( &v0Lat, &v0Lon, &v1Lat, &v1Lon, &dir, &ier );
    if ( dir >= 180.0F ) {
        dir = dir - 180.0F; 
    }
    else {
        dir = dir + 180.0F;     
    }


    /*
     *  Find the coordinates of the rectangle's vortex.
     *  P1: perpendicular to v0_v1, 100nm away from MP, clockwise.
     *  P2: same dirction as v0 to v1, 100nm away from P1.
     *  P3: same dirction as v0 to v1, 100nm away from P4.
     *  P4: perpendicular to v0_v1, 100nm away from MP, counter-clockwise.
     */    
    ang = dir + 90.0;
    clo_dltln ( &mlat, &mlon, &dist, &ang, &rLat[0], &rLon[0], &ier );

    ang = dir - 90.0;
    clo_dltln ( &mlat, &mlon, &dist, &ang, &rLat[3], &rLon[3], &ier );
    
    clo_dltln ( &rLat[0], &rLon[0], &dist, &dir, &rLat[1], &rLon[1], &ier );
        
    clo_dltln ( &rLat[3], &rLon[3], &dist, &dir, &rLat[2], &rLon[2], &ier );
             
}

/*=====================================================================*/

static void snap_locateSnapPts ( float vLat, float vLon, char *vorName,
			     int npts, float *bLat, float *bLon,
			     int *nSnap, float *snapLat, float *snapLon,
			     char **snapDesc, Vor_t *coordPts, int *iret )
/************************************************************************
 * snap_locateSnapPts							*
 *									*
 * Locates the snap points within the clipped bubble.			*
 *                                                                      *
 * static void snap_locateSnapPts ( vLat, vLon, vorName, npts, bLat, 	*
 * 			bLon, nSnap, snapLat, snapLon, snapDesc, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *     vLat   		float	Latitude of VOR point			*
 *     vLon   		float	Longitude of VOR point			*
 *     *vorName   	char	3-character name of VOR			*
 *     npts   		int	Number of the clipped bubble points	*
 *     *bLat   		float	Latitudes of the clipped bubble		*
 *     *bLon   		float	Longitudes of the clipped bubble	*
 *     *coordPts	Vor_t	coordination point information		*	
 *                                                                      *
 * Output parameters:                                                   *
 *     *nSnap   	int	Number of the snap points		*
 *     *recLat   	float	Latitudes of snap points		*
 *     *recLon   	float	Longitudes of snap points		*
 *     **snapDesc   	char	array of text description of snap points*
 *     *iret		int	Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/05	initial coding				*
 * E. Safford/SAIC	06/05	change jj limit to match BUBBLE_RADIUS  *
 * B. Yin/SAIC		06/05	check if a pt is in 10nm to a coord pt	*
 ***********************************************************************/
{
    int		ii, jj, np = 1, ier, inout[1];
    float	dir, dist, tlat, tlon;
    char	*dirStr[] = { "N", "NNE", "NE", "ENE",
    			    "E", "ESE", "SE", "SSE",
    			    "S", "SSW", "SW", "WSW",
    			    "W", "WNW", "NW", "NNW" };
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    /* 
     *  Write the VOR to the output arrays as the first snap point.
     */
    *nSnap = 1;
    snapLat[0] = vLat;
    snapLon[0] = vLon;
    strcpy ( snapDesc[0], vorName );
    
    
    /* 
     *  Compute a possible total of 160 snap points on a 16 point compass, 
     *  starting clockwise from north and then NNE, with 10 points on 
     *  each direction.  The distance between any two points on the same
     *  direction is SNAP_POINT_STEP in nautical miles. If such a point is 
     *  within the clipped bubble, it is considered as a snap point and added 
     *  to the output.
     */
    for ( ii = 0; ii < 16; ii++ ) {
        dir = ii * ANGLE_STEP;  
        for ( jj = _VOR_ptRadius; jj <= BUBBLE_RADIUS_INT; 
						jj += SNAP_POINT_STEP ) {
            dist = jj * NM2M;
            clo_dltln ( &vLat, &vLon, &dist, &dir, &tlat, &tlon, &ier );
            
	    cgr_inpoly ( sys_M, &np, &tlat, &tlon, 
	                 sys_M, &npts, bLat, bLon, inout, &ier );
	    
	    if ( inout[0] == 1 ) {
		if ( snap_okToUsePt( tlat, tlon, coordPts )) {
                   snapLat[*nSnap] = tlat;
                   snapLon[*nSnap] = tlon;
                   sprintf ( snapDesc[*nSnap], "%d%s_%s", jj, dirStr[ii], vorName );
                   (*nSnap)++;
		}
	    }
	    else {
	        break;
	    }
	}    
    }       
}

/*=====================================================================*/

static void snap_writeTblHeader ( FILE *fp, int *iret )
/************************************************************************
 * snap_writeTblHeader							*
 *                                                                      *
 * Writes out header information to the snap table file, 		*
 *                                                                      *
 * static void snap_writeTblHeader ( fp, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *     *fp   		FILE	Pointer to the output file		*
 *                                                                      *
 * Output parameters:                                                   *
 *     *iret		int	Return code				*
 *     					 0 - Normal			*
 *     					-1 - Error			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC	 	01/05	initial coding				*
 ***********************************************************************/
{
    char	buffer[2048];    
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    strcpy ( buffer, "! SNAP.TBL\n");
    strcat ( buffer, "!\n");
    strcat ( buffer, "! This table contains aviation VOR points and all VOR-relative points.\n");
    strcat ( buffer, "! VOR-relative points, or 'snapable' points are all points relative to\n");
    strcat ( buffer, "! individual VOR points located on a 16 point compass in 10nm intervals.\n");
    strcat ( buffer, "!\n");
    strcat ( buffer, "! The format for the file is given below, with the number of characters in\n");
    strcat ( buffer, "! parentheses.\n");
    strcat ( buffer, "!\n");
    strcat ( buffer, "!!\n");
    strcat ( buffer, "! Log:\n");
    strcat ( buffer, "! J. Wu/SAIC	        01/05   Created table\n");
    strcat ( buffer, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");    
    strcat ( buffer, "!\n");
    strcat ( buffer, "!STID    STNM   NAME                            ");
    strcat ( buffer, "ST CO   LAT    LON   ELV  PRI\n");
    strcat ( buffer, "!(8)     (6)    (32)                            ");
    strcat ( buffer, "(2)(2)  (5)    (6)   (5)  (2)\n");
    
    cfl_writ ( fp, (int)strlen(buffer), (unsigned char*)buffer, iret );
         
}

/*=====================================================================*/

static void snap_writeSnapTbl ( FILE *fp, int np, float *sLat, float *sLon,
			       char **sDesc, Boolean coord, int *iret )
/************************************************************************
 * snap_writeSnapTbl							*
 *                                                                      *
 * Writes out snap points around a VOR point to the snap table file, 	*
 * using the format as in the "vors.tbl".				*
 *                                                                      *
 * static void snap_writeSnapTbl ( fp, np, sLat, sLon, sDesc, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *     *fp   		FILE	Pointer to the output file		*
 *     *nSnap   	int	Number of the snap points		*
 *     *sLat   		float	Latitudes of snap points		*
 *     *sLon   		float	Longitudes of snap points		*
 *     **sDesc   	char	array of text description of snap points*
 *     coord		Boolean	flag for coordination points or VOR	*
 *                                                                      *
 * Output parameters:                                                   *
 *     *iret		int	Return code				*
 *     					 0 - Normal			*
 *     					-1 - Error			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC	 	12/04	initial coding				*
 * B. Yin/SAIC		06/05	added a parameter to flag coord/vor	*
 * S. Danz/NCEP/AWC	01/06	change '(int)' cast to 'G_NINT' macro	*
 ***********************************************************************/
{
    int		ii, ier;
    char	buffer[2048];
    char	stid[9], stnm[7], name[33], st[3], co[3];
    char	tmpstr[33], seq[6], lat[6], lon[7], elv[6], pri[3];
    
    static 	int coordNum = 0;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    for ( ii = 0; ii < np; ii++ ) {	

	if ( coord ) {
	   coordNum++;
	   sprintf ( tmpstr, "%d", coordNum );
	   cst_padString ( tmpstr, '0', 0, 3, seq );
	   sprintf ( stid, "COORD%s", seq );
	}
	else {
	   
	   /* 
	    *  STID(8) - 3 character VOR + 5 digits (00000, 00001, ...) 
	    */
	   strcpy ( stid, sDesc[0] );
	   sprintf ( tmpstr, "%d", ii );
           cst_padString ( tmpstr, '0', 0, 5, seq );
	   strcat ( stid, seq );
	}
	
	/* 
	 *  STNM(6) - unused, fill with '9'. 
	 */
	strcpy ( stnm, "     9" );

	/* 
	 *  NAME(32) - use distance/direction string and pad to 32 characters. 
	 */
	strcpy ( tmpstr, sDesc[ii] );
        cst_padString ( tmpstr, ' ', 1, 32, name );
	
	
	/* 
	 *  ST/CO(2/2) - unused, fill with '-'. 
	 */
	strcpy ( st, "- " );
	strcpy ( co, "- " );

	
	/* 
	 *  LAT/LON(5/6) - write as integer with fixed length. 
	 */
	sprintf( lat, "%5d", G_NINT(sLat[ii]*100.0F) );
	sprintf( lon, "%6d", G_NINT(sLon[ii]*100.0F) );

	/* 
	 *  ELV(5) - unused, fill with '0'. 
	 */
	strcpy ( elv, "    0" );


	/* 
	 *  PRI(2) - 1 for VOR itself, and 2 for every refrence snap point. 
	 */
	if ( ii == 0 ) {
	    strcpy ( pri, " 1" );
	}
	else {
	    strcpy ( pri, " 2" );		
	}
		
	
	/* 
	 *  Write out. 
	 */
	sprintf( buffer, "%s %s %s %s %s %s %s %s %s\n", 
	         stid, stnm, name, st, co, lat, lon, elv, pri );
	    
	cfl_writ ( fp, (int)strlen(buffer), (unsigned char*)buffer, &ier );
            
	if ( ier != 0 ) {
	    *iret = -1;
            break;
	}
	
    }
}

/*=====================================================================*/

static void snap_loadStnPts ( Vor_t *stn, char *fileName, int *iret )
/************************************************************************
 * snap_loadStnPts							*
 *									*
 * This function loads VORS table information into station structure.	*
 *									*
 * static void snap_loadStnPts ( stn, fileName, iret )			*
 *									*
 * Input parameters:							*
 *	*stn		Vor_t	VOR station structure to fill		*
 *	*fileName	char	stn table file				*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				  -1 - Unable to open table		*
 *				  -2 - No records in table		*
 *				  -3 - Invalid type of point		*
 **									*
 * Log:									*
 * J. Wu/SAIC		01/05	initial coding				*
 * B. Yin/SAIC		06/05	added fileName as an input parameter	*
 ***********************************************************************/
{
    int		counter, ier, stnm, lat, lon, numstn, elv, pri;
    int		descLen;
    
    char 	buff[120], id[9], desc[64];
    char	state[8], cntry[8], c10[21]="\0", tmpstr[128];

    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    
    /*
     *  Open the table. If not found return an error.
     */
    fp = (FILE *) cfl_tbop ( fileName, "stns", &ier );
    if ( fp == NULL  ||  ier != 0 )  {
	stn->nstn = 0;
        *iret = -1;
        return;
    }

    cfl_tbnr( fp, &numstn, &ier );

    if ( numstn != 0 )  {

        /* 
         *  Allocate the structure elements.
         */
        stn->nstn = numstn; 
        stn->station = (VInfo_t *) malloc( (size_t)numstn * sizeof(VInfo_t) );

    }
    else  {
	cfl_clos(fp, &ier);
        *iret = -2;
        return;
    }

    rewind(fp);


    /* 
     *  For every line in the station table list, read in the record,
     *  parse out the fields, and add to the structure.
     */

    counter = 0;
    while ( counter < numstn ) {
    
	cfl_trln(fp, sizeof(buff), buff, &ier);

	if ( ier == 0 )  {

	    id[ 0 ]	= '\0';
	    desc[ 0 ]	= '\0';
	    state[ 0 ]	= '\0';
	    cntry[ 0 ]	= '\0';
	    c10[ 0 ]	= '\0';
	    stnm 	= 0;
	    lat 	= 9999;
	    lon 	= 9999;
	    elv 	= 9999;
	    pri 	= 9999;

	    strncpy( tmpstr, buff, 9 );
	    tmpstr[9]='\0';
	    sscanf( tmpstr, "%s", id );
	    stn->station[counter].id = (char *)malloc(sizeof(char)*strlen(id)+1);
	    strcpy( stn->station[counter].id, id );

	    strncpy( tmpstr, &buff[9], 7 );
	    tmpstr[7]='\0';
	    sscanf( tmpstr, "%d", &stnm );
	    stn->station[counter].nm = stnm;

	    strncpy( tmpstr, &buff[16], 33 );
	    tmpstr[33]='\0';
	    cst_rxbl( tmpstr, desc, &descLen, &ier );
	    desc[ strlen( desc ) - 1 ] = '\0';
	    stn->station[counter].desc = (char *)malloc(sizeof(char)*strlen(desc)+1);
	    cst_lcuc ( desc, stn->station[counter].desc, &ier );

	    strncpy( tmpstr, &buff[49], 3 );
	    tmpstr[3]='\0';
	    sscanf( tmpstr, "%s", state );
	    stn->station[counter].state=(char *)malloc(sizeof(char)*strlen(state)+1);
	    strcpy( stn->station[counter].state, state );

	    strncpy( tmpstr, &buff[52], 3 );
	    tmpstr[3]='\0';
	    sscanf( tmpstr, "%s", cntry );
	    stn->station[counter].cntry=(char *)malloc(sizeof(char)*strlen(cntry)+1);
	    strcpy( stn->station[counter].cntry, cntry );

	    strncpy( tmpstr, &buff[55], 22 );
	    tmpstr[22]='\0';
	    sscanf( tmpstr, "%d %d %d %d", &lat, &lon, &elv, &pri );
	    stn->station[counter].lat = (float)lat / 100.0F;

	    stn->station[counter].lon = (float)lon / 100.0F;

	    stn->station[counter].elv = elv;

	    stn->station[counter].pri = pri;

	    strncpy( tmpstr, &buff[77], 21 );
	    tmpstr[21]='\0';
	    sscanf( tmpstr, "%s", c10 );
	    stn->station[counter].col10 = (char *)malloc(sizeof(char)*strlen(c10)+1);
	    strcpy( stn->station[counter].col10, c10 );

	    counter++;

    	}

    }

    cfl_clos(fp, &ier);

}
/*======================================================================*/

static Boolean snap_okToUsePt ( float lat, float lon, Vor_t *coordPts )
/************************************************************************
 * snap_okToUsePt							*
 *									*
 * This function checks if a point is in the range of 			*
 * _COORD_ptRadius from any coordination point.				*
 *									*
 * static Boolean snap_okToUsePt ( lat, lon, coordPts )			*
 *									*
 * Input parameters:							*
 *	lat		float	lat of a point				*
 *	lon		float	lon of a point				*
 * 	*coordPts	Vor_t	coordination point information		*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 * Return parameters:							*
 *			Boolean	True 	if the point( lat, lon ) is ok 	*
 *					to use (out of the range of the	*
 *				    	threshold).			*
 *				False	if the point is in the range of	*
 *					the threshold form a coord point*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/05	Created					*
 * E. Safford/SAIC	10/05	use all coordPts, incl VORs		*
 ***********************************************************************/
{
    int		ii, ier, npts;
    float	dist;
/*---------------------------------------------------------------------*/

    npts = 1;

    for ( ii =0; ii < coordPts->nstn; ii++ ) {

        clo_dist ( &lat, &lon, &npts, &(coordPts->station[ ii ].lat),
	 	      &(coordPts->station[ ii ].lon), &dist, &ier );

        if ( (dist * M2NM) < _COORD_ptRadius ) return False;

    }

    return True;

}
/*======================================================================*/

static void snap_usage ( void )
/************************************************************************
 * snap_usage								*
 *									*
 * This function prints out the usage message.        			*
 *									*
 * void snap_usage ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * E. Safford/SAIC	07/05	Created					*
 * E. Safford/SAIC	07/05	correct default values			*
 * E. Safford/SAIC	10/05	correct executable name 		*
 ***********************************************************************/
{

    printf ( "\n");
    printf ( "\n");
    printf ( "Usage: build_snap [-v #] [-c #] outfile\n" );
    printf ( "\n");
    printf ( "      This program builds a snap table.\n" );
    printf ( "\n");
    printf ( "      outfile = Required.  The output file name for the new table.      \n");
    printf ( "\n");
    printf ( "      -v #    = Optional VOR radius setting.  \n");
    printf ( "                The -v flag should be followed by a numeric  \n");
    printf ( "                value.  Snap points will be located starting \n");
    printf ( "                at the specified VOR radius at 10nm intervals \n");
    printf ( "                along the 16 point compass bearings.  The \n");
    printf ( "                default value is %dnm.\n", DEFAULT_VOR_PT_RADIUS );
    printf ( "\n");
    printf ( "      -c #    = Optional Coordination point radius setting.  \n");
    printf ( "                The -c flag should be followed by a numeric \n");  
    printf ( "                value.  All snap points located closer to a \n"); 
    printf ( "                coordination point than this specified radius \n"); 
    printf ( "                will not be included in the table output. \n");
    printf ( "                The default value is %dnm.\n", DEFAULT_COORD_PT_RADIUS );
    printf ( "\n");

}

