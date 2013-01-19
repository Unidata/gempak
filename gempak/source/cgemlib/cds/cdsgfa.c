#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

#define	SMALLF		0.0001
#define FONT_TYPE	2
#define TEXT_FONT	1
#define TEXT_SIZE	1
#define TEXT_WIDTH	1

static	int	_gfaColor;
static  int	_gfaLineWidth;
static  int	_gfaLineElm;
static  int	_gfaLineType;

static void cds_dispOpenFzl(  const VG_DBStruct *el, const int index );
static void cds_drawFzlvlContour ( const VG_DBStruct *el, const int index );


void cds_gfa ( VG_DBStruct *el, int index, int *iret )
/************************************************************************
 * cds_gfa								*
 *									*
 * This function displays GFAs to the output device.			*
 *									*
 * cds_gfa ( el, index, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	index		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC            01/04   create based on cds_ccf		*
 * J. Wu/SAIC            02/04   rewrote for GFA structure change	*
 * J. Wu/SAIC            02/04   add options for plotting attribute box	*
 * J. Wu/SAIC            03/04   add non-convective SIGMET		*
 * J. Wu/SAIC            06/04   add subtype GFA_GFA			*
 * J. Wu/SAIC            08/04   adjust text color & display format	*
 * E. Safford/SAIC	 08/04	 change gfa top & bottom to strings     *
 *				  display missing as xxx, 0 as SFC	*
 * E. Safford/SAIC	 08/04	 allow xxx/SFC as a flght lvl display	*
 * J. Wu/SAIC            09/04   remove airmet & non-convective sigmet	*
 * H. Zeng/SAIC		 10/04	 added arrowed line display		*
 * J. Wu/SAIC            10/04   access GFA attributes with cvg_getFld	*
 * B. Yin/SAIC           11/04   removed the description tag		*
 * B. Yin/SAIC           11/04   added the gfa tag value to text box	*
 * B. Yin/SAIC           11/04   modified top/bottom display logic	*
 * B. Yin/SAIC           06/05   added fzl into the bottom levels	*
 * E. Safford/SAIC	 07/05	 remove update# from display		*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 * B. Yin/SAIC		12/05   handle open line for FZLVL		*
 * B. Yin/SAIC		01/06	added level to FZLVL text		*
 * E. Safford/SAIC	01/06	call cds_drawFzlvlContour for closed    *
 *				  FZLVL hazards				*
 * B. Yin/SAIC          02/06   define text type, font, width, and size	*
 * S. Danz/AWC          03/06   Use cds_gfatxt to create text box      	*
 * B. Yin/SAIC          07/06   Use special lines for some hazards     	*
 * B. Yin/SAIC          07/06   Use solid line if no line type		*
 * B. Yin/SAIC          08/06   Use attributes in uattribd tbl for gpmap*
 * S.Danz/AWC		08/06	Process arrow endpoint information	*
 * L. Hinson/AWC        11/06   Add checks to see if textLayout string  *
 *                              contains NIL.  If so, don't plot labels,*
 *                              or arrows, and exit out of routine.     *
 * E. Safford/SAIC	04/07	clean up memory leak & use G_* macros	*
 * L. Hinson/AWC        06/07   Add arrow size tag                      *
 ***********************************************************************/
{
    int		ii, ier, idx, np, npts, npls;
    int		color, type, width, ttype,  *inout, one;
    float	*xpt, *ypt, *xpl, *ypl, *xdev, *ydev, area;
    float	xcent, ycent, xcent_lat, ycent_lon, blat, blon;
    char	areatypestr[24], linelm[32], lintyp[32],value[32], linwid[32];
    VG_DBStruct	el_tmp;
    char        textLayoutStr[256];
    Boolean     textLayoutNIL = False;
/*---------------------------------------------------------------------*/

    *iret = 0;


    /*
     *  Use either the original element attributes 
     *  the cdsUattr color if it's not 0.
     */
    _gfaColor     = 0;
    _gfaLineWidth = 0;

    cds_getinx ( el, &idx, &ier );

    if( ier >= 0 ) {

	/*
	 *  Get the gfa color.
	 */
        if( cdsUattr[idx].maj_col == 0 ) {
	    _gfaColor = el->hdr.maj_col;
        }
        else {
	    _gfaColor = cdsUattr[idx].maj_col;
        }
        if (_gfaColor <= 0) {
          *iret = 0;
          return;
        }        
        /*
	 *  Get the line width.
	 */
	if( cdsUattr[idx].info.gfa->linwid == 0 ) {
            cvg_getFld ( el, TAG_GFA_LINEWIDTH, linwid, &ier );    
	    if( ier >= 0 ) {
		_gfaLineWidth = atoi( linwid );
	    }
	}
	else {
	    _gfaLineWidth = cdsUattr[idx].info.gfa->linwid;
	}

        /*
	 *  Get the line element (line or special line)
	 */
	if( cdsUattr[idx].info.gfa->linelm == 0 ) {
            cvg_getFld ( el, TAG_GFA_LINELM, linelm, &ier );    
	    if( ier >= 0 ) {
		_gfaLineElm = atoi( linelm );
	    }
	}
	else {
	    _gfaLineElm = cdsUattr[idx].info.gfa->linelm;
	}
       
        /*
	 *  Get the line type (line or special line)
	 */
	if( cdsUattr[idx].info.gfa->lintyp == 0 ) {
            cvg_getFld ( el, TAG_GFA_LINTYP, lintyp, &ier );    
	    if( ier >= 0 ) {
		_gfaLineType = atoi( lintyp );
	    }
	}
	else {
	    _gfaLineType = cdsUattr[idx].info.gfa->lintyp;
	}
        if ( cdsUattr[idx].info.gfa->textLayout[0] == '\0' ) {
          cvg_getFld ( el, TAG_GFA_TXTLYT, textLayoutStr, &ier);
        } else {
          strcpy(textLayoutStr, cdsUattr[idx].info.gfa->textLayout);
        }
        /* Check to see if textLayoutString for GFA Text contains NIL */
        /*  If so, set the variable textLayoutNIL to TRUE. */
        if (strstr(textLayoutStr,"NIL") != NULL) {
          textLayoutNIL = True;
        }

    }

    /*
     *  FZLVL hazards are drawn with arrowed lines.  If they are 
     *  closed then the label is applied in the same way as other 
     *  hazards, but if they are open then the algorithm is 
     *  different.  Below cds_dispOpenFzl() handles FZLVL hazard and
     *  label placement, while cds_drawFzlvlContour() just draws the 
     *  hazard itself without the label. 
     */   
    cvg_getFld ( el, TAG_GFA_AREATYPE, areatypestr, &ier );    
    cvg_getFld ( el, TAG_GFA_LINELM, linelm, &ier );   
    cvg_getFld ( el, TAG_GFA_LINTYP, lintyp, &ier );    

    if( strcasecmp( areatypestr, "FZLVL" ) == 0 ) {

	if( el->hdr.closed == 0 )  {
	    cds_dispOpenFzl( el, index );
	    return;
	}

    }

    /*
     *  Build a LINE_ELM to display the GFA bound.
     *  Save the attr settings before display, restore after display.
     */ 
    np = el->elem.gfa.info.npts;

    el_tmp.hdr = el->hdr;
    el_tmp.hdr.vg_class = (char)CLASS_LINES;

    if ( _gfaLineElm == SPLN_ELM ) { 
                                                                                
       el_tmp.hdr.vg_type = (char)SPLN_ELM;
       el_tmp.elem.spl.info.numpts = np;
       el_tmp.elem.spl.info.spltyp = _gfaLineType;  
                                                                              
       el_tmp.elem.spl.info.splwid = _gfaLineWidth; 
       el_tmp.elem.spl.info.splsiz = 1.;
       el_tmp.elem.spl.info.splstr = 0.5;
       el_tmp.elem.spl.info.spldir = 1;
                                                                                
       for ( ii = 0; ii < np; ii++ ) {
           el_tmp.elem.spl.latlon[ii]       = el->elem.gfa.latlon[ii];
           el_tmp.elem.spl.latlon[ii+np]    = el->elem.gfa.latlon[ii+np];
       }
                                                                                
    }
    else {
                                                                                
       el_tmp.hdr.vg_type = (char)LINE_ELM;
    
       el_tmp.elem.lin.info.numpts = np;
       el_tmp.elem.lin.info.lintyp = _gfaLineType; 
       el_tmp.elem.lin.info.lthw   = 0;
       el_tmp.elem.lin.info.lwhw   = 0;

       el_tmp.elem.lin.info.width  = _gfaLineWidth; 
    
       for ( ii = 0; ii < np; ii++ ) {
           el_tmp.elem.lin.latlon[ii]	= el->elem.gfa.latlon[ii];
           el_tmp.elem.lin.latlon[ii+np]	= el->elem.gfa.latlon[ii+np];
       }
        
    }

    cds_getinx ( &el_tmp, &idx, &ier );
    
    color  = cdsUattr[idx].maj_col; 
    type   = cdsUattr[idx].info.lin->lintyp;
    width  = cdsUattr[idx].info.lin->width;

    cdsUattr[idx].maj_col = cdsUattr[index].maj_col; 
    cdsUattr[idx].info.lin->width = el_tmp.elem.lin.info.width;

    cds_dspelm ( &el_tmp, &ier );

    cdsUattr[idx].maj_col	  = color; 
    cdsUattr[idx].info.lin->lintyp = type;
    cdsUattr[idx].info.lin->width  = width;

    /*
     *  Build a SPLN_ELM before SPTX_ELM is drawn.
     *  Save the attr settings before display, restore after display.
     */ 
    npts = 1;
    G_MALLOC( xpt, float, npts, "cds_gfa: xpt" );
    G_MALLOC( ypt, float, npts, "cds_gfa: ypt" );
    G_MALLOC( inout, int, npts, "cds_gfa: inout" );

    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    blat = atof ( value );

    cvg_getFld ( el, TAG_GFA_LON, value, &ier );
    blon = atof ( value );
    
    xpt[0] = blat;
    ypt[0] = blon;

    if ( xpt[0] <   -90.0F || xpt[0] >  90.0F || 
	 ypt[0] <= -180.0F || ypt[0] > 180.0F    ) {
	G_FREE( xpt, float );
	G_FREE( ypt, float );
        G_FREE( inout, int );
        return;
    }

    npls = el->elem.gfa.info.npts;
    G_MALLOC( xpl, float, npls, "cds_gfa: xpl" );
    G_MALLOC( ypl, float, npls, "cds_gfa: ypl" );

    for ( ii = 0; ii < npls; ii++ ) {
        xpl[ii] = el->elem.gfa.latlon[ii];
        ypl[ii] = el->elem.gfa.latlon[ii+npls];
    }

    /*
     * Check if the center of the text box is inside or outside of 
     * the GFA polygon.
     */
    cgr_inpoly ( sys_M, &npts, xpt, ypt, sys_M, &npls, xpl, ypl, inout, &ier );

    /*
     * If the center of the text box is outside of GFA polygon, and TextLayoutNIL
     * not set display an arrowed line.
     */
    if ( inout[0] == 0 && textLayoutNIL == False) {

	/*
	 * Retreive the location for the arrow endpoint
	 */
	cvg_getFld ( el, TAG_GFA_ARROW_LAT, value, &ier );
	if (ier == 0) {
	    xcent_lat = atof ( value );

	    cvg_getFld ( el, TAG_GFA_ARROW_LON, value, &ier );
	    ycent_lon = atof ( value );
	} else {
             /*
              * Calculate the centeroid of the polygon.
              */
	     G_MALLOC( xdev, float, npls, "cds_gfa: xdev" );
	     G_MALLOC( ydev, float, npls, "cds_gfa: ydev" );

             gtrans ( sys_M, sys_D, &npls, xpl, ypl, xdev, ydev, &ier, 
		      strlen(sys_M), strlen(sys_D) );

             cgr_centroid ( xdev, ydev, &npls, &xcent, &ycent, &area, &ier );

             one = 1;
             gtrans ( sys_D, sys_M, &one, &xcent, &ycent, &xcent_lat, &ycent_lon, 
		      &ier, strlen(sys_D), strlen(sys_M) );
	     /*
	      * Free allocated memories.
	      */
	     G_FREE( xdev, float );
	     G_FREE( ydev, float );
	}

         /*
          * Build a temporary vg element for arrowed line.
          */
	 el_tmp.hdr.delete   = 0;
         el_tmp.hdr.vg_type  = SPLN_ELM;
         el_tmp.hdr.vg_class = CLASS_LINES;
	 el_tmp.hdr.filled   = 0;
         el_tmp.hdr.closed   = 0;
         el_tmp.hdr.smooth   = 0;
	 el_tmp.hdr.version  = 0;
         el_tmp.hdr.grptyp   = 0;
         el_tmp.hdr.grpnum   = 0;

         el_tmp.hdr.maj_col  = _gfaColor;        

         el_tmp.hdr.min_col  = el->hdr.min_col;
         el_tmp.hdr.recsz    = 0;
	 el_tmp.hdr.range_min_lat = 0;
         el_tmp.hdr.range_min_lon = 0;
         el_tmp.hdr.range_max_lat = 0;
         el_tmp.hdr.range_max_lon = 0;

	 el_tmp.elem.spl.info.numpts = 2;
         el_tmp.elem.spl.info.spltyp = 4;
	 el_tmp.elem.spl.info.splstr = 0.5;
         el_tmp.elem.spl.info.spldir = 1;
         if ( fabs(cdsUattr[index].info.gfa->szarrow) < 0.01) { /* szarrow = 0.0? */
           cvg_getFld ( el, TAG_GFA_ARROWSZ, value, &ier);
           if ( ier >= 0 ) {
             el_tmp.elem.spl.info.splsiz = atof(value);
           } else {
             el_tmp.elem.spl.info.splsiz = 1.0;
           }
         } else {
           el_tmp.elem.spl.info.splsiz = cdsUattr[index].info.gfa->szarrow;
         }
	 el_tmp.elem.spl.info.splwid = 1;

         el_tmp.elem.spl.latlon[0] = blat;
	 el_tmp.elem.spl.latlon[1] = xcent_lat;
         el_tmp.elem.spl.latlon[2] = blon;
	 el_tmp.elem.spl.latlon[3] = ycent_lon;
    
         cds_getinx ( &el_tmp, &idx, &ier );
    
         color  = cdsUattr[idx].maj_col; 
   
         cdsUattr[idx].maj_col = el_tmp.hdr.maj_col; 
   
         cds_dspelm ( &el_tmp, &ier );
       
         cdsUattr[idx].maj_col = color; 

    }

    /*
     * Free allocated memories from above.
     */
    G_FREE( xpl, float );
    G_FREE( ypl, float );
    G_FREE( xpt, float );
    G_FREE( ypt, float );
    G_FREE( inout, int );

    /*
     *  Build an SPTX_ELM to display the GFA attribute,
     *  Save the attr settings before display, restore after display.
     *  Note: do not plot attribute box if the location is not valid.
     */ 
    if ( blat < -90.0F ) {
        return;
    }

    /*
     * Create the text box element
     */
    cds_gfatxt(el, &el_tmp, &ier);
    
    if (ier == 3) {  /*exit routine if ier set to 3 */
      *iret = 0;   /* on NIL set in layout string */
      return;
    }
      	              
    cds_getinx ( &el_tmp, &idx, &ier );
    
    color = cdsUattr[idx].maj_col; 
    ttype = cdsUattr[idx].info.spt->text.info.sptxtyp;

    el_tmp.hdr.maj_col  = _gfaColor;        
    cdsUattr[idx].maj_col = el_tmp.hdr.maj_col; /* use original color */

    cds_dspelm ( &el_tmp, &ier );

    cdsUattr[idx].maj_col	    = color; 
    cdsUattr[idx].info.spt->text.info.sptxtyp = ttype;
    
}

/*=====================================================================*/

static void cds_dispOpenFzl ( const VG_DBStruct *el, const int index ) 
/************************************************************************
 * cds_dispOpenFzl							*
 *									*
 * This function displays open line FZLVL to the output device.		*
 *									*
 * cds_dispOpenFzl ( el, index )					*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	index		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC          12/05   Created					*
 * E. Safford/SAIC	01/06	rename cds_dispFzl -> cds_dispOpenFzl   *
 * B. Yin/SAIC          02/06   define text type, font, width and size	*
 * B. Yin/SAIC          08/06   Use attributes in uattribd tbl for gpmap*
 * L. Hinson/AWC        12/06   Check ier after call to cds_gfatxt      *
 * E. Safford/SAIC	04/07	use G_MALLOC/G_FREE macros		*
 * L. Hinson/AWC        06/07   Add arrow size tag                      *
 ***********************************************************************/
{
    int		ii, ier, idx, np, one;
    int		color, ttype, vrt1, vrt2;

    float	xBox[ 5 ], yBox[ 5 ], latNearest, lonNearest;
    float	xNearest, yNearest, txtLat, txtLon;
    float	xTxt, yTxt, smallestDist, *xPts, *yPts;

    Boolean	intersct;

    static VG_DBStruct	elTxt, elArrowLine;
    float	lat[ MAXPTS ], lon[ MAXPTS];
    char        value[32];
/*---------------------------------------------------------------------*/

    cds_drawFzlvlContour ( el, index );
    np = el->elem.gfa.info.npts;

    /*
     *  Load the lat/lon points from the element.
     */
    for ( ii = 0; ii < np; ii++ ) {
        lat[ii]		= el->elem.gfa.latlon[ii];
        lon[ii]   	= el->elem.gfa.latlon[ii+np];
    }

    /*
     *  Create the text box
     */
    cds_gfatxt(el, &elTxt, &ier);
    if (ier == 3) {  /*exit routine if ier set to 3 */
                     /* on NIL set in layout string */
      return;
    }  	              

    crg_gettxtbox ( &elTxt, 2, xBox, yBox );

    one = 1;
    txtLat = elTxt.elem.spt.info.lat;
    txtLon = elTxt.elem.spt.info.lon;
    gtrans ( sys_M, sys_D, &one, &txtLat, &txtLon, &xTxt, &yTxt, &ier, 
	     strlen(sys_M), strlen(sys_D) );

    for ( ii = 0; ii < 4; ii++ ) {
	xBox[ii] += xTxt;
	yBox[ii] += yTxt;
    }

    /*
     *  Close the polygon
     */
    xBox[ 4 ] = xBox[ 0 ];
    yBox[ 4 ] = yBox[ 0 ];

    /* 
     *  Check if the text box intersects the GFA.
     *  If not, draw an arrow line from the box to the GFA
     */
    cgr_linepolyint( sys_M, np, &el->elem.gfa.latlon[ 0 ], 
    		     &el->elem.gfa.latlon[ np ], sys_D, 4,
    		     xBox, yBox, &intersct, &ier );

    if ( !intersct ) {
       /*
        * Build a temporary vg element for arrowed line.
        */
       elArrowLine.hdr.vg_type  = SPLN_ELM;
       elArrowLine.hdr.vg_class = CLASS_LINES;
       elArrowLine.hdr.maj_col  = _gfaColor;
       elArrowLine.hdr.min_col  = _gfaColor;

       elArrowLine.elem.spl.info.numpts = 2;
       elArrowLine.elem.spl.info.spltyp = 4;
       elArrowLine.elem.spl.info.splstr = 0.5;
       elArrowLine.elem.spl.info.spldir = 1;
       if ( fabs(cdsUattr[index].info.gfa->szarrow) < 0.01) { /* szarrow = 0.0? */
         cvg_getFld ( el, TAG_GFA_ARROWSZ, value, &ier);
         if ( ier >= 0 ) {
           elArrowLine.elem.spl.info.splsiz = atof(value);
         } else {
           elArrowLine.elem.spl.info.splsiz = 1.0;
         }
       } else {
         elArrowLine.elem.spl.info.splsiz = cdsUattr[index].info.gfa->szarrow;
       }
       elArrowLine.elem.spl.info.splwid = 1;

       /*
 	*  Find the nearest point on the GFA to the text
	*/
       G_MALLOC( xPts, float, np, "cds_gfa: xPts" );
       G_MALLOC( yPts, float, np, "cds_gfa: yPts" );

       gtrans ( sys_M, sys_D, &np, lat, lon,
		xPts, yPts, &ier, 
		strlen(sys_M), strlen(sys_D) );

       cgr_segdist ( &np, xPts, yPts, &xTxt, &yTxt, 
		     &smallestDist, &vrt1, &vrt2, 
		     &xNearest, &yNearest, &ier );

       /*
        *  If the nearest point is one of the end points of the GFA
	*  use th middle point of the segment.
	*/
       if ( G_ABS( xNearest - xPts[ 0 ] ) < SMALLF &&
	    G_ABS( yNearest - yPts[ 0 ] ) < SMALLF ) {
	  xNearest = ( xPts[ 0 ] + xPts[ 1 ] ) * .5;
	  yNearest = ( yPts[ 0 ] + yPts[ 1 ] ) * .5;

       }
       else if ( G_ABS( xNearest - xPts[ np - 1 ] ) < SMALLF &&
	         G_ABS( yNearest - yPts[ np - 1 ] ) < SMALLF ) {

	  xNearest = ( xPts[ np - 1 ] + xPts[ np - 2 ] ) * .5;
	  yNearest = ( yPts[ np - 1 ] + yPts[ np - 2 ] ) * .5;

       }

       G_FREE( xPts, float );
       G_FREE( yPts, float );

       gtrans ( sys_D, sys_M, &one, &xNearest, &yNearest, 
       	        &latNearest, &lonNearest, &ier, 
		strlen(sys_D), strlen(sys_M) );

       elArrowLine.elem.spl.latlon[0] = txtLat;
       elArrowLine.elem.spl.latlon[1] = latNearest;
       elArrowLine.elem.spl.latlon[2] = txtLon;
       elArrowLine.elem.spl.latlon[3] = lonNearest;
    
       /*
        *  Display the arrow line.
	*/
       cds_getinx ( &elArrowLine, &idx, &ier );
    
       color  = cdsUattr[idx].maj_col; 
   
       cdsUattr[idx].maj_col = elArrowLine.hdr.maj_col; 
   
       cds_dspelm ( &elArrowLine, &ier );
       
       cdsUattr[idx].maj_col = color; 
	
    }

    /*
     *  Display the text box.
     */
    cds_getinx ( &elTxt, &idx, &ier );
    
    color = cdsUattr[idx].maj_col; 
    ttype = cdsUattr[idx].info.spt->text.info.sptxtyp;

    cdsUattr[idx].maj_col = elTxt.hdr.maj_col; /* use original color */
       
    cds_dspelm ( &elTxt, &ier );

    cdsUattr[idx].maj_col	    = color; 
    cdsUattr[idx].info.spt->text.info.sptxtyp = ttype;

}


/*=====================================================================*/

static void cds_drawFzlvlContour ( const VG_DBStruct *el, const int index ) 
/************************************************************************
 * cds_drawFzlvlContour							*
 *									*
 * This function displays the FZLVL contour line to the output device.	*
 * This is an arrowed line (a special line type, not line type).	*
 *									*
 * cds_drawFzlvlContour( el, index )					*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	index		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * E. Safford/SAIC      12/05   Moved from cds_dispFzl			*
 * B. Yin/SAIC		07/06	Use special line type for FZLVL		*
 * B. Yin/SAIC          08/06   Use attributes in uattribd tbl for gpmap*
 ***********************************************************************/
{
    int		ii, ier, idx, np;
    int		color, type, width;

    char	lintyp[32];

    static VG_DBStruct	elSpl;
/*---------------------------------------------------------------------*/

    /*
     *  Initialize the spline element
     */
    elSpl.hdr.vg_class = (char)CLASS_LINES;
    elSpl.hdr.vg_type  = (char)SPLN_ELM;
    elSpl.hdr.vg_type  = SPLN_ELM;
    elSpl.hdr.vg_class = CLASS_LINES;
    elSpl.hdr.maj_col  = _gfaColor;
    elSpl.hdr.min_col  = _gfaColor;
    elSpl.hdr.closed   = el->hdr.closed;

    np = el->elem.gfa.info.npts;

    
    elSpl.elem.spl.info.numpts = np;
    elSpl.elem.spl.info.spltyp = 4;
    elSpl.elem.spl.info.splstr = 0.5;
    elSpl.elem.spl.info.spldir = 1;
    elSpl.elem.spl.info.splsiz = 1;
    elSpl.elem.spl.info.splwid = 1;

    elSpl.elem.spl.info.splwid = _gfaLineWidth; 

    cvg_getFld ( el, TAG_GFA_LINTYP, lintyp, &ier );    
    elSpl.elem.spl.info.spltyp = atoi ( lintyp );
    
    for( ii= 0; ii<np; ii++ ) {
	elSpl.elem.spl.latlon[ ii ] = el->elem.gfa.latlon[ ii ];
	elSpl.elem.spl.latlon[ ii+np ] = el->elem.gfa.latlon[ ii+np ];
    }

    /*
     *  Save the attributes for spline 
     */
    cds_getinx ( &elSpl, &idx, &ier );
    
    color  = cdsUattr[idx].maj_col; 
    type   = cdsUattr[idx].info.spl->spltyp;
    width  = cdsUattr[idx].info.spl->splwid;
    
    /*
     *  Set the attributes of spline
     */
    cdsUattr[idx].maj_col = cdsUattr[index].maj_col; 
    cdsUattr[idx].info.spl->splwid = elSpl.elem.spl.info.splwid;
   
    /*
     *  display the spline element
     */
    cds_dspelm ( &elSpl, &ier );
       
    /*
     *  Restore the spline attributes
     */
    cdsUattr[idx].maj_col	    = color; 
    cdsUattr[idx].info.spl->spltyp  = type;
    cdsUattr[idx].info.spl->splwid  = width;

}
