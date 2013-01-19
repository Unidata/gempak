#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "gpc.h"
#include "proto_gpc.h"
#include "cescmn.h"


#define	GFA_COLOR	3
#define LINE_WIDTH	"3"
#define MIN_DIST	100		/* minimum distance from the  */
					/* comparing point to the line*/
			
#define NUM_FA_AREAS	6


void	vg2frzl ( char *vgFile, char *tmpFile, int *xgrid, int *ygrid, 
		  float *grid, char *cycle, char *fhr, char *tag, 
                  char *stat, char *vcord, char *lvlincr, int *iret );

static void gd_bnds2poly ( char *bounds, char *name,
                        float **xpoly, float **ypoly, int *npoly );

static void gd_getFrzlRange( char *vcord, int inc,
       			 int xgrid, int ygrid, float *grid, 
			 char *ranges );

static int gd_updateRange ( char *vgfile );

void	vg2frzl ( char *vgFile, char *tmpFile, int *xgrid, int *ygrid, 
		  float *grid, char *cycle, char *fhr, char *tag, 
                  char *stat, char *vcord, char *lvlincr, int *iret )
/************************************************************************
 * vg2frzl                                                              *
 *                                                                      *
 * This function creates GFA FZLVL elements from grouped contours and	*
 * labels. Grid data that generate the contours are used to determine	*
 * the direction of the FZLVLs.						*
 *                                                                      *
 * vg2frzl ( vgFile, tmpFile, xgrid, ygrid, grid, fhr, tag, stat, vcord	*
 *		lvlincr, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfile		char	the output vg file			*
 *      *tmpFile	char	input temporary vg file that contains	*
 *				contours and labels. After all elements	*
 *				are retrived, this file is deleted.	*
 *      *xgrid		int 	x size of the grid			*
 *      *ygrid		int 	y size of the grid			*
 *	*grid		float	grid					*
 *	*fhr		char	forecast hour				*
 *	*tag		char	GFA tag					*
 *	*stat		char	GFA issuing status			*
 *	*vcord		char	GFA issuing status			*
 *	*lvlincr	char	GFA issuing status			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal                      *
 *                                      -1: can't open vg file          *
 *                                      -2: out of boundary reading grid*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           4/06   Created                                 *
 * B. Yin/SAIC		 4/06	Move the comparing point if it's too cls*
 * B. Yin/SAIC		 5/06	Add the tmp file in the parameter list	*
 * S. Danz/AWC           7/06   Switch to new cvg_writefD               *
 * D.W.Plummer/NCEP     12/06   Add conversion of level '0' to 'SFC'    *
 * D.W.Plummer/NCEP     12/06   Add tolerance to cgr_qrol call          *
 * B. Yin/SAIC		 1/07	Computed freezing level ranges		*
 * B. Yin/SAIC		 2/07	Added new text label attributes		*
 * L. Hinson/AWC         6/08   Add cycle parameter and TAG_GFA_CYCLE;  *
 *                              Update TLAYOUT str with ZFHR parameter  *
 * S. Jacobs/NCEP	 9/10	Removed range check; Added range update	*
 ***********************************************************************/
{
    int		ii, jj, kk, ier, nextEl, curPos, nEl, frzlvl, grpLabel;
    int		one = 1, xgd, ygd, position, closed, npts, *flag, loc;
    int		vert1, vert2, inc, indx, hazType;
    long	fileSize;
    float	midLat, midLon, midgx, midgy, vgrid, fLevel;
    float	dummy1, dummy2, tmp, dist, gdLat, gdLon;
    float       tol=0.0F;
    char        vgfname[ 128 ], lvlStr[ 4 ], txtLat[ 16 ], txtLon[ 16 ];
    char	cmd[ 128 ], subtype[ 4 ], tmpStr[ 128 ];

    FILE        *fptr;
    VG_DBStruct *elements, outEl;
    Boolean	gotIndx;

    static char		fzlRanges[ 256 ];
    static Boolean	init = False;
/*---------------------------------------------------------------------*/

    cvg_open ( tmpFile, 0, &fptr, &ier );
                                                                                
    if ( ier != 0 )  {
                                                                                
       ier = -14;
       er_wmsg ( "GDFRZL", &ier, tmpFile, iret, strlen("GDFRZL"), strlen(tmpFile) );

       *iret = -1;
       return;
                                                                                
    }

    /*
     *  Set default values for FHR, TAG and STAT (issue status).
     */
    if ( strlen ( fhr ) == 0 ) {
       strcpy ( fhr, "0-6" );
    }
                                                                                
    if ( strlen ( tag ) == 0 ) {
       strcpy ( tag, "1W" );
    }
                                                                                
    if ( strlen ( stat ) == 0 ) {
       strcpy ( stat, "NRML" );
    }
                                                                                
    cfl_inqr ( tmpFile, NULL, &fileSize, vgfname, &ier );
                                                                              
    curPos  = 0;
    nextEl  = 0;
    nEl	    = 0;
    ier     = 0;
    elements= NULL;
    flag    = NULL;

    /*
     *  Read elements from the input vg file.
     */
    while ( nextEl < MAX_EDITABLE_ELEMS )  {
                                                                                
        G_REALLOC ( elements, VG_DBStruct, nEl + 1, "vg2frzl elements" );

        cvg_rdrecnoc ( vgfname, fptr, curPos, &elements[ nEl ], &ier );
                                                                                
        if ( ier != 0 ) break;
                                                                                
        if ( elements[ nEl ].hdr.recsz > 0 )  {
                                                                                
           curPos += elements[ nEl ].hdr.recsz;
                                                                                
           if ( !elements[ nEl ].hdr.delete )  {
                                                                                
              nEl++;

              if ( (int)elements[ nEl ].hdr.vg_type == GFA_ELM )  {

		 /*
		  *  Free GFA block pointers. We are not going to use GFA elements.
		  */
                 cvg_freeElPtr ( &elements[ nEl ] );

	      }
                                                                                
           }

        }
                                                                                
        nextEl++;

    }                                       /* read element loop  */
                                                                                
    cvg_clos ( fptr, &ier );

    /*
     *  Remove the temporary vg file.
     */
    sprintf ( cmd, "rm -f %s", tmpFile ); 
    system ( cmd );

    /*
     *  Initialize the group type table and the output GFA elements.
     */
    if ( !init ) {

       ces_gtrtbl ( &ier );
       init = True;

    }

    outEl.hdr.delete         = 0;
    outEl.hdr.version        = 0;
    outEl.hdr.grptyp         = 0;
    outEl.hdr.grpnum         = 0;
    outEl.hdr.range_min_lat  = 26.0F;        /* roughly the bounds of the */
    outEl.hdr.range_min_lon  = -120.0F;      /* continental US            */
    outEl.hdr.range_max_lat  = 48.0F;
    outEl.hdr.range_max_lon  = -67.0F;

    outEl.hdr.vg_class    = CLASS_MET;
    outEl.hdr.vg_type     = GFA_ELM;
    outEl.hdr.smooth      = 0;
    outEl.hdr.filled      = 0;    
    outEl.hdr.maj_col     = GFA_COLOR;
    outEl.hdr.min_col     = GFA_COLOR;

    flag = (int*) calloc ( nEl, sizeof( int ) );

    /*
     *  Get the group Id for "LABEL" group.
     */
    ces_gtgid ( "LABEL", &grpLabel, &ier );

    if ( strlen ( lvlincr ) == 0 ) {

	inc = 500;

    }
    else {

	cst_numb( lvlincr, &inc, &ier );

    }

    /* 
     * Compute freezing level ranges.
     */
    if ( *xgrid > 0 && *ygrid > 0 ) {
	gd_getFrzlRange( vcord, inc, *xgrid, *ygrid, grid, fzlRanges );
    }

    /*
     *  Loop over all elements. Find Label groups with a text and at least
     *  one contours. Write them into GFA FZLVLs.
     */
    for ( ii = 0; ii < nEl; ii++ ) {

	if ( ( elements[ ii ].hdr.grptyp == (char) grpLabel ) && 
	     ( elements[ ii ].hdr.vg_type == SPTX_ELM ) ) {

	   for ( jj = 0; jj < nEl; jj++ ) {

	       if ( ( elements[ jj ].hdr.grpnum == elements[ ii ].hdr.grpnum ) &&
	            ( elements[ jj ].hdr.vg_type == LINE_ELM ) &&
		    flag[ jj ]  == 0 ) {
                                                                            
	 	  flag[ jj ] = 1;
		  npts = elements[ jj ].elem.lin.info.numpts;

    		  outEl.elem.gfa.info.npts = npts;
    		  outEl.elem.gfa.info.nblocks = 0;
  		  outEl.hdr.closed = elements[ jj ].hdr.closed; 

		     
		  /*
		   *  Set the direction of the line.
		   *  First get the middle point of the line. Then get a
		   *  grid near it. Find whether the grid is on the left or right 
		   *  side of the line. If it is on the left, the grid should be
		   *  colder. If it is on the right, the grid should be warmer.
		   */
		  midLat = elements[ jj ].elem.lin.latlon[ npts /2 ];
		  midLon = elements[ jj ].elem.lin.latlon[ npts /2 + npts ];

		  gtrans ( sys_M, sys_G, &one, &midLat, &midLon,
             		   &midgx, &midgy, &ier, strlen(sys_M), strlen(sys_G) );

		  xgd = G_NINT( midgx );
		  ygd = G_NINT( midgy );

		  closed = elements[ jj ].hdr.closed; 
		  position = 0;

		  while ( position == 0 ) {
			
		        ygd = ygd - 2;
			ygd = ygd >= 0 ? ygd : ygd + 5;
			xgd = xgd - 2;
			xgd = xgd >= 0 ? xgd : xgd + 3;
			
		        /*
		         *  Check grid array boundary.
		         */
		        if ( ( xgd < 0 ) || (ygd < 0 ) ||
		             ( ( xgd - 1 + *xgrid * ( ygd - 1) ) > *xgrid * *ygrid ) ) {
			
       			    ier = -15;
       			    er_wmsg ( "GDFRZL", &ier, " ", iret, 
				      strlen("GDFRZL"), strlen(" ") );

       			    *iret = -2;
       			    return;
			
		        }
		  
		        vgrid = grid[ xgd - 1 + *xgrid * ( ygd - 1) ];

		        midgx = xgd;
		        midgy = ygd;
		        gtrans ( sys_G, sys_M, &one, &midgx, &midgy,
             			 &gdLat, &gdLon,
             			 &ier, strlen(sys_G), strlen(sys_M) );

		        /*
			 *  If the comparing is too close to the line,
			 *  move it further away.
			 */
			cgr_segdist ( &npts, &elements[ jj ].elem.lin.latlon[0], 
				      &elements[ jj ].elem.lin.latlon[ npts ], 
				      &gdLat, &gdLon, &dist,
                        	      &vert1, &vert2, &dummy1, &dummy2, &ier );

			if ( dist / M2NM < MIN_DIST ) continue;
			
		        cgr_qrol ( &elements[ jj ].elem.lin.info.numpts, 
		  	     &elements[ jj ].elem.lin.latlon[0], 
			     &elements[ jj ].elem.lin.latlon[ npts ], 
                             &closed, &gdLat, &gdLon, &tol, &position, &ier );

		  }

		  fLevel = atof ( elements[ ii ].elem.spt.text );
		  
	          /*
		   *  Chanage line direction.
		   */
	          if ( ( position == 1 && vgrid > fLevel ) || 
		       ( position == -1 && vgrid < fLevel ) ) {

			for ( kk = 0; kk < npts / 2; kk++ ) {

			    tmp = elements[ jj ].elem.lin.latlon[ kk ];
			    elements[ jj ].elem.lin.latlon[ kk ] = 
			    			elements[ jj ].elem.lin.latlon[ npts - kk - 1 ];
			    elements[ jj ].elem.lin.latlon[ npts - kk - 1 ] = tmp;

			    tmp = elements[ jj ].elem.lin.latlon[ kk + npts ];
			    elements[ jj ].elem.lin.latlon[ kk + npts ] = 
			    			elements[ jj ].elem.lin.latlon[ npts - kk - 1 + npts ];
			    elements[ jj ].elem.lin.latlon[ npts - kk - 1 + npts ] = tmp;

			}

		  }

		  memcpy ( &outEl.elem.gfa.latlon, &elements[ jj ].elem.lin.latlon,
		  	   elements[ jj ].elem.lin.info.numpts * 2 * sizeof( float ) );

    		  cvg_setFld ( &outEl, TAG_GFA_AREATYPE, "FZLVL", &ier );
                  cvg_setFld ( &outEl, TAG_GFA_CYCLE, cycle, &ier );
       		  cvg_setFld ( &outEl, TAG_GFA_FCSTHR, fhr, &ier );
		  cvg_setFld ( &outEl, TAG_GFA_TAG, tag, &ier );
		  cvg_setFld ( &outEl, TAG_GFA_STATUS, stat, &ier );

		  if ( strlen( fzlRanges ) != 0 ) {

		     cvg_setFld ( &outEl, TAG_GFA_FZLRANGE, fzlRanges, &ier );

		  }

		  frzlvl = atoi (  elements[ ii ].elem.spt.text ) / 100;

		  if ( frzlvl == 0 ) {

		     hazType = GFA_HAZARD_FZLVL_SFC;

		  }
		  else {

		     hazType = GFA_HAZARD_FZLVL;

		  }

		  /*
		   *  Set the GFA subtype.
		   */
		  if ( strchr ( fhr, '-' ) == NULL ) {

	 	     sprintf ( subtype, "%d", hazType*10 + GFA_SNAPSHOT );

		  }
		  else if ( strstr ( fhr, "-12" ) != NULL ) {

	 	     sprintf ( subtype, "%d", hazType*10 + GFA_USER_OUTLOOK );

		  }
		  else {

	 	     sprintf ( subtype, "%d", hazType*10 + GFA_USER_SMEAR );

		  }

    		  cvg_setFld ( &outEl, TAG_GFA_SUBTYPE, subtype, &ier );

		  ces_getinx ( &outEl, atoi( subtype ), &indx, &ier );		  

		  if ( ier >= 0 ) {

    		     outEl.hdr.maj_col = set[ indx ].maj_col;
    		     outEl.hdr.min_col = set[ indx ].min_col;

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->linelm );
		     cvg_setFld ( &outEl, TAG_GFA_LINELM, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->lintyp );
		     cvg_setFld ( &outEl, TAG_GFA_LINTYP, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->linwid );
		     cvg_setFld ( &outEl, TAG_GFA_LINEWIDTH, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->info.txtcol );
		     cvg_setFld ( &outEl, TAG_GFA_TXTCLR, tmpStr, &ier );

		     sprintf( tmpStr, "%f", set[ indx ].info.gfa->info.sztext );
		     cvg_setFld ( &outEl, TAG_GFA_TXTSZ, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->info.itxfn );
		     cvg_setFld ( &outEl, TAG_GFA_TXTFN, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->info.ithw );
		     cvg_setFld ( &outEl, TAG_GFA_TXTHW, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->info.iwidth );
		     cvg_setFld ( &outEl, TAG_GFA_TXTWDTH, tmpStr, &ier );

		     sprintf( tmpStr, "%i", set[ indx ].info.gfa->info.ialign );
		     cvg_setFld ( &outEl, TAG_GFA_TXTALGN, tmpStr, &ier );

		     cvg_setFld ( &outEl, TAG_GFA_TXTLYT, 
		     		  set[ indx ].info.gfa->textLayout, &ier );

		     gotIndx = True;

		  }
		  else {

		     gotIndx = False;

		  }

		  if ( frzlvl == 0 ) {

		     strcpy ( lvlStr, "SFC" );

		     if ( !gotIndx ) {

    		     	outEl.hdr.maj_col = 26;
    		     	outEl.hdr.min_col = 26;

		     	cvg_setFld ( &outEl, TAG_GFA_LINELM, "20", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_LINTYP, "33", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_LINEWIDTH, LINE_WIDTH, &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTCLR, "26", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTSZ, "1.00", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTFN, "1", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTHW, "2", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTWDTH, "1", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTALGN, "0", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTLYT, "STA;FHR::ZFHR::TAG;HZD:LVL", &ier );

		     }

		  }
		  else {
			
		     sprintf ( lvlStr, "%03i", frzlvl );
		     
		     if ( !gotIndx ) {

    		     	outEl.hdr.maj_col = 12;
    		     	outEl.hdr.min_col = 12;

		     	cvg_setFld ( &outEl, TAG_GFA_LINELM, "20", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_LINTYP, "34", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_LINEWIDTH, LINE_WIDTH, &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTCLR, "12", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTSZ, "1.00", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTFN, "1", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTHW, "2", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTWDTH, "1", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTALGN, "0", &ier );
		     	cvg_setFld ( &outEl, TAG_GFA_TXTLYT, "STA;FHR::TAG;HZD:LVL", &ier );

		     }

		  }

		  cvg_setFld ( &outEl, "Level", lvlStr, &ier );

		  sprintf ( txtLat, "%f", elements[ ii ].elem.spt.info.lat );
		  sprintf ( txtLon, "%f", elements[ ii ].elem.spt.info.lon );

		  cvg_setFld ( &outEl, TAG_GFA_LAT, txtLat, &ier );
		  cvg_setFld ( &outEl, TAG_GFA_LON, txtLon, &ier );

  		  if ( outEl.hdr.closed ) {
		
 		     cvg_setFld ( &outEl, "<Contour>", "Closed", &ier );

		  }
		  else {
		
 		     cvg_setFld ( &outEl, "<Contour>", "Open", &ier );

		  }

    		  outEl.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof(int) * 2 +
                		sizeof(char) * STD_STRLEN * outEl.elem.gfa.info.nblocks ) +
                		sizeof(float) * outEl.elem.gfa.info.npts * 2;
                                                                                
                  cvg_writefD( &outEl, -1, outEl.hdr.recsz, vgFile, &loc, &ier );

    		  cvg_freeElPtr ( &outEl );		  

	       }

	   }

	}
       
    }
    
    gd_updateRange ( vgFile );

    if ( flag ) free( flag );
    G_FREE( elements, VG_DBStruct );

}

/*=====================================================================*/

static void gd_getFrzlRange ( char *vcord, int inc, int igrid, int jgrid,
       			      float *grid, char * ranges )
/************************************************************************
 * gd_getFrzlRange                                    	                *
 *                                                                      *
 * This routine computes the freezing level ranges for every FA area.  	*
 *                                                                      *
 * static void gd_getFrzlRange (vcord, inc, igrid, jgrid, grid, ranges) *
 *                                                                      *
 * Input parameters:                                                    *
 *      vcord		char	vertical coordinate for the data	*
 *      inc		int	nearest integer ranges will be rounded. *
 *      igrid           int	number of rows in the grid.	        *
 *      jgrid           int	number of columns in the grid.	        *
 *      *grid           float	grid values.			        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ranges		char	range string	 			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		11/06	Created			                *
 * S. Jacobs/NCEP	 9/10	Added vcord to the call to properly	*
 * 				get SFC information			*
 ***********************************************************************/
{

    int	    ii, jj, nn, totalGrids, npoly, ier, *inout;
    float   *xgrid, *ygrid, *xpoly, *ypoly;
    float   maxFzl[ NUM_FA_AREAS ], minFzl[ NUM_FA_AREAS ];

    char    tag[ 32 ];
    char    *FA_Area[ NUM_FA_AREAS ] = {"SLC", "SFO", "CHI", "DFW", "BOS", "MIA"};
/*---------------------------------------------------------------------*/
    
    totalGrids = igrid * jgrid;

    G_MALLOC( xgrid, float, totalGrids, "getFrzlRange: xgrid" );
    G_MALLOC( ygrid, float, totalGrids, "getFrzlRange: ygrid" );
    G_MALLOC( inout, int,   totalGrids, "getFrzlRange: inout" );

    /*
     *  Build gird coordinates, which wll be used when calling cgr_inpoly().
     */
    for ( ii = 1; ii <= igrid; ii++ ) {
	for ( jj = 1; jj <= jgrid; jj++ ) {

		xgrid[ ( jj - 1 ) * igrid + ii - 1] = (float)ii;
		ygrid[ ( jj - 1 ) * igrid + ii - 1] = (float)jj;

	}
    }

    ranges[ 0 ] = '\0';

    for ( nn = 0; nn < NUM_FA_AREAS; nn++ ) {

	maxFzl[ nn ] = -99999.0;
	minFzl[ nn ] =  99999.0;

	/*
	 *  Convert the FA area bounds to a lat/lon polygon.
	 */
        sprintf ( tag, "<AREA>%s", FA_Area[ nn ] );
        gd_bnds2poly ( "FA_AREA_BNDS", tag, &xpoly, &ypoly, &npoly );

	/*
	 *  Find out which grids are in the FA area.
	 */
        cgr_inpoly ( sys_G, &totalGrids, xgrid, ygrid,
                     sys_M, &npoly, xpoly, ypoly,
                     inout, &ier );
                                                                                   
	/*
	 *  Find the maximum and minum grid value in the FA area.
	 */
        for ( ii = 0; ii < igrid; ii++ ) {
	    for ( jj = 0; jj < jgrid; jj++ ) {

		if ( inout[ jj * igrid + ii ] ) {

		   /*
		    *  Initialize
		    */
		    if ( strcasecmp ( vcord, "frzl" ) == 0 ) {
		       if ( maxFzl[ nn ] < 0 ) maxFzl[ nn ] = grid[ jj * igrid + ii ];
		       if ( minFzl[ nn ] < 0 ) minFzl[ nn ] = grid[ jj * igrid + ii ];
		    }

		   if ( grid[ jj * igrid + ii ] > maxFzl[ nn ] ) {

			 maxFzl[ nn ] = grid[ jj * igrid + ii ];

		   }
		   else if ( grid[ jj * igrid + ii ] < minFzl[ nn ] ) {

			 minFzl[ nn ] = grid[ jj * igrid + ii ];

		   }
		   
		}
	    }
	}

	/* 
	 *  Round the results.
	 */
	if ( strcasecmp ( vcord, "frzl" ) == 0 ) {
	    if ( inc != 0 ) {
		
	       maxFzl[ nn ] = ( (int) maxFzl[ nn ] / inc + 1 ) * inc;
	       minFzl[ nn ] = ( (int) minFzl[ nn ] / inc ) * inc;

	    }

	    sprintf( ranges, "%s%s;%3.3i;%3.3i;", ranges, FA_Area[ nn ], 
			(int)maxFzl[ nn ]/100, (int)minFzl[ nn ]/100 ); 
	}
	else {

	    if ( minFzl[nn] <= 0 ) { 
		sprintf( ranges, "%s%s;%s;%s;", ranges, FA_Area[ nn ], 
			"SFC", "SFC" ); 
	    }
	    else {
		sprintf( ranges, "%s%s;%s;%s;", ranges, FA_Area[ nn ], 
			"NON", "NON" ); 
	    }

	}

        G_FREE ( xpoly, float );
        G_FREE ( ypoly, float );

    }

    G_FREE ( xgrid, float );
    G_FREE ( ygrid, float );
    G_FREE ( inout, int );

}

/*=====================================================================*/

static void gd_bnds2poly ( char *bounds, char *name, float **xpoly, 
			float **ypoly, int *npts )
/************************************************************************
 * gd_bnds2poly                                       	                *
 *                                                                      *
 * Reads all bound parts into a polygon in lat/lon coordinate.  	*
 *                                                                      *
 * static void gd_bnds2poly ( bounds, name, xpoly, ypoly, npts )        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *bounds         char            Bounds name                     *
 *      *name           char            FA area tag   		        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *xpoly		float		lat array 			*
 *      *ypoly		float		lon array 			*
 *      *npts           int             number of points in the array   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		11/06	Created			                *
 ***********************************************************************/
{
    int                 minp, maxp, next, np, ier;
    float               filter, *xlat, *ylon;
/*---------------------------------------------------------------------*/
                                                                                
    /*
     * Set bounds type and tag
     */
    clo_init  ( &ier );
    clo_bstype ( bounds, &ier );
    clo_bstag  ( name, &ier );
                                                                             
    /*
     * Initialization
     */
    minp   = 0;
    maxp   = LLMXPT;
    filter = 0.0F;
    next   = 0;
                                                                          
    /*
     * Read each bounds part 
     */
    G_MALLOC ( *xpoly, float, maxp, "bnds2poly: xpoly" );
    G_MALLOC ( *ypoly, float, maxp, "bnds2poly: ypoly" );
                                                                         
    *npts = 0;
    clo_bgnext ( &minp, &maxp, &filter, npts, *xpoly, *ypoly, &next );
                                                                 
    G_MALLOC ( xlat, float, maxp, "bnds2poly: xlat" );
    G_MALLOC ( ylon, float, maxp, "bnds2poly: ylon" );

    while ( 1 ) {
                                                                         
        clo_bgnext ( &minp, &maxp, &filter, &np, xlat, ylon, &next );

	if ( next != 0 ) break;

	memmove( &(*xpoly)[ *npts - 1 ], xlat, np * sizeof( float ) );
	memmove( &(*ypoly)[ *npts - 1 ], ylon, np * sizeof( float ) );

	*npts += np;

    }
                                                                                   
    G_FREE ( xlat, float );
    G_FREE ( ylon, float );
                                                                                
}
                                                                                 
/*=====================================================================*/

static int gd_updateRange ( char *vgfile )
/************************************************************************
 * gd_updateRange                                     	                *
 *                                                                      *
 * This routine will update the freezing level range information by 	*
 * merging all of the individual ranges. This gets the SFC level into	*
 * the range properly.							*
 *                                                                      *
 * static void gd_updateRange ( vgfile )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *vgfile		char            input vg file name 	        *
 *                                                                      *
 * Return parameters:                                                   *
 *      		int		-1 if cannot process		*
 *                                       0 if all ok			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 9/10	Created					*
 ***********************************************************************/
{
    int		ier, nextEl, curPos;
    long	fileSize;
    char        vgfname[ 128 ];
    char	ranges[ 128 ];
    char	save_range[ 128 ];

    char	**carr;
    int		nc, ii, jj, kk, nn, loc, ier1;
    char	maxFzl[NUM_FA_AREAS][8], minFzl[NUM_FA_AREAS][8];
    char	save_maxFzl[NUM_FA_AREAS][8], save_minFzl[NUM_FA_AREAS][8];
    char	area[NUM_FA_AREAS][8];
    Boolean	place=False;

    FILE        *fptr;
    VG_DBStruct el, *outEl;
/*---------------------------------------------------------------------*/

    cfl_inqr ( vgfile, NULL, &fileSize, vgfname, &ier );
    if ( ier != 0 ) return (-1);

    /*
     *  Read elements from the input vg file.
     */
    curPos  = 0;
    nextEl  = 0;
    ier     = 0;
    ranges[ 0 ] = '\0'; 
    save_range[ 0 ] = '\0'; 

    carr = (char **) malloc(sizeof(char *) * (NUM_FA_AREAS*3));
    for ( ii = 0; ii < (NUM_FA_AREAS*3); ii++ ) {
	carr[ii] = (char *) malloc(8);
    }

    for ( ii = 0; ii < NUM_FA_AREAS; ii++ ) {
	strcpy ( save_maxFzl[ii], "0" );
	strcpy ( save_minFzl[ii], "999" );
    }

/*---------------------------*/
    cvg_open ( vgfile, 0, &fptr, &ier );

    /*
     * Check for different range strings.
     */
    while ( nextEl < MAX_EDITABLE_ELEMS )  {
                                                                                
        cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );
                                                                                
        if ( ier != 0 ) break;
                                                                                
        if ( el.hdr.recsz > 0 )  {
                                                                                
           curPos += el.hdr.recsz;
                                                                                
           if ( (int)el.hdr.vg_type == GFA_ELM &&
                    !el.hdr.delete )  {

	      cvg_getFld( &el, TAG_GFA_FZLRANGE, ranges, &ier );
              cvg_freeElPtr ( &el );

	      if ( ( ier == 0 ) && strlen( ranges ) != 0 ) {

		 if ( strcmp ( ranges, save_range ) != 0 ) {
		     strcpy ( save_range, ranges );
		     cst_clst ( ranges, ';', " ", (NUM_FA_AREAS*3), 8, carr, &nc, &ier );

		     ii = 0;
		     for ( jj = 0; jj < NUM_FA_AREAS; jj++ ) {
			 strcpy (   area[jj], carr[ii] ); ii++;
			 strcpy ( maxFzl[jj], carr[ii] ); ii++;
			 strcpy ( minFzl[jj], carr[ii] ); ii++;

			 if ( ( strcmp ( maxFzl[jj], "SFC" ) != 0 ) && 
			      ( strcmp ( maxFzl[jj], "NON" ) != 0 ) ) {
			     cst_numb ( maxFzl[jj], &kk, &ier );
			     cst_numb ( save_maxFzl[jj], &nn, &ier );
			     if  ( kk > nn )  strcpy ( save_maxFzl[jj], maxFzl[jj] );
			 }

			 if ( strcmp ( minFzl[jj], "SFC" ) == 0 ) {
			     kk = 0;
			 }
			 else if ( strcmp ( minFzl[jj], "NON" ) != 0 ) {
			     cst_numb ( minFzl[jj], &kk, &ier );
			 }
			 else {
			     kk = 999;
			 }
			 if ( strcmp ( save_minFzl[jj], "SFC" ) == 0 ) {
			     nn = 0;
			 }
			 else {
			     cst_numb ( save_minFzl[jj], &nn, &ier );
			 }
			 if  ( kk < nn )  strcpy ( save_minFzl[jj], minFzl[jj] );

		     }
		 }

	      }
	      
           }

        }
                                                                                
        nextEl++;

    }					 /* read element loop  */

    cvg_clos ( fptr, &ier );
/*---------------------------*/

    ranges[ 0 ] = '\0'; 
    for ( jj = 0; jj < NUM_FA_AREAS; jj++ ) {
	sprintf( ranges, "%s%s;%s;%s;", ranges, area[jj], 
			save_maxFzl[jj], save_minFzl[jj] ); 
    }

/*---------------------------*/
    cvg_open ( vgfile, 1, &fptr, &ier );
    curPos = 0;

    while ( curPos < fileSize )  {
                                                                                
        cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );
                                                                                
        if ( ier != 0 ) break;
                                                                                
        if ( el.hdr.recsz > 0 )  {

	   loc = el.hdr.recsz;
                                                                                
           if ( (int)el.hdr.vg_type == GFA_ELM &&
                    !el.hdr.delete )  {

	      outEl = (VG_DBStruct *) malloc ( sizeof(VG_DBStruct) );
	      memcpy ( outEl, &el, sizeof(VG_DBStruct) );

	      el.hdr.delete = 1;
	      cfl_seek( fptr, (long)curPos, 0, &ier );
	      if ( MTMACH == MTULTX ||
		   MTMACH == MTALPH ||
		   MTMACH == MTLNUX ) {
		  cvg_swap( SWPHDR, G_FALSE, el, &el, &ier1 );
	      }
	      cfl_writ( fptr, sizeof(el.hdr), (unsigned char *)&el, &ier );

	      cvg_setFld( outEl, TAG_GFA_FZLRANGE, ranges, &ier );
	      cvg_write( outEl, -1, outEl->hdr.recsz, fptr, place, &ier );

              cvg_freeElPtr ( &el );

           }

           curPos += loc;

        }
                                                                                
    }

    cvg_clos ( fptr, &ier );
/*---------------------------*/

    for ( ii = 0; ii < (NUM_FA_AREAS*3); ii++ ) {
	free (carr[ii] );
    }
    free ( carr );

    return (0);

}
