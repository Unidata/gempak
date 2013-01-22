#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "vgtag.h"


/*  
 * Private functions  
 */
static void crg_settca ( VG_DBStruct *el, int joffset, int elnum, int *iret );
static void crg_setccf ( VG_DBStruct *el, int joffset, int elnum, int *iret );
static void crg_setgfa ( VG_DBStruct *el, int joffset, int elnum, int *iret );
static void crg_setsgwx ( VG_DBStruct *el, int joffset, int elnum, int *iret);

/*  
 * Public functions  
 */
void crg_mkRange ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_mkRange	 							*
 *									*
 * This function sets the range record for an element.			*
 *									*
 * Note: joffset is the offset to the element in WORK_FILE.		*
 *									*
 * void crg_mkRange ( el, joffset, elnum, iret )			*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	joffset		int		Offset to element in WORK_FILE	*
 *	elnum		int		Index of elem. in range arrary	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/01	move from crgset.c			*
 * H. Zeng/EAI          07/02   modified to use crg_setsigmet()         *
 * J. Wu/SAIC		09/02	add case LIST_ELM			*
 * H. Zeng/XTRIA	07/03   added volcano element			*
 * J. Wu/SAIC		09/03	add case JET_ELM			*
 * H. Zeng/XTRIA	09/03   added ash cloud element			*
 * J. Wu/SAIC		01/04	add GFA_ELM				*
 * B. Yin/SAIC		02/04	added TCA_ELM				*
 * B. Yin/SAIC		04/04	modified lat/lon of tca elements	*
 * B. Yin/SAIC		05/04	increased range of tca elements		*
 * B. Yin/SAIC		07/04	modified code for tca water and islands *
 * J. Wu/SAIC		12/04	include text potision in GFA_ELM's range*
 * S. Gilbert		11/05	added zero Watch/Warning case for TCA   *
 *                              and moved TCA processing to local       *
 *                              crg_settca.                             *
 * S. Danz		08/06	Fixed indexing into lat/lon for GFA	*
 * J. Wu/SAIC		07/07	build second range record for GFA box	*
 * L. Hinson/AWC        07/09   Revise SIGCCF_ELM to call crg_setccf    *
 * L. Hinson/AWC        01/12   Add SGWX_ELM
 ***********************************************************************/
{
    int		ier, np;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
    switch ( el->hdr.vg_type)
    {
	case FRONT_ELM:

	    np = el->elem.frt.info.numpts;
	    crg_setltln (elnum, joffset, np, &el->elem.frt.latlon[0], 
			&el->elem.frt.latlon[np], &ier);

	    break;

	case SPLN_ELM:

	    np = el->elem.spl.info.numpts;
	    crg_setltln (elnum, joffset, np, &el->elem.spl.latlon[0], 
		         &el->elem.spl.latlon[np], &ier);

	    break;

	case LINE_ELM:

	    np = el->elem.lin.info.numpts;
	    crg_setltln (elnum, joffset, np, &el->elem.lin.latlon[0], 
			&el->elem.lin.latlon[np], &ier);

	    break;

        case TEXT_ELM:
        case TEXTC_ELM:
	case SPTX_ELM:

	    crg_settxt(el, joffset, elnum, &ier);

	    break;

	case BARB_ELM:
	case ARROW_ELM:
	case DARR_ELM:
	case HASH_ELM:

	    crg_setwnd(el, joffset, elnum, &ier);

            break;

	case MARK_ELM:
        case WXSYM_ELM:
	case CTSYM_ELM:
	case ICSYM_ELM:
	case PTSYM_ELM:
	case PWSYM_ELM:
	case SKSYM_ELM:
	case SPSYM_ELM:
	case TBSYM_ELM:
        case CMBSY_ELM:

	    crg_setsym(el, joffset, elnum, &ier);

            break;

	case WBOX_ELM:

	    crg_setwbx(el, joffset, elnum, &ier);

            break;

	case CIRCLE_ELM:

	    crg_setcir (el, joffset, elnum, el->hdr.range_min_lat,
			el->hdr.range_min_lon, el->hdr.range_max_lat,
			el->hdr.range_max_lon, &ier );

	    break;

	case TRKSTORM_ELM:

	    np = el->elem.trk.info.npts;
	    crg_setltln (elnum, joffset, np, &el->elem.trk.latlon[0], 
			&el->elem.trk.latlon[np], &ier);

	    break;


	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:

	    crg_setsigmet(el, joffset, elnum, &ier);

	    break;


	case SIGCCF_ELM:

            crg_setccf (el, joffset, elnum, &ier );

	    break;

        case VOLC_ELM:

	    crg_setvol(el, joffset, elnum, &ier);

            break;

        case ASHCLD_ELM:

	    crg_setvac(el, joffset, elnum, &ier);

            break;

	case LIST_ELM:

	    crg_setlist(el, joffset, elnum, &ier);

	    break;

	case JET_ELM:

	    np = el->elem.jet.line.spl.info.numpts;
	    crg_setltln (elnum, joffset, np, &el->elem.jet.line.spl.latlon[0], 
			&el->elem.jet.line.spl.latlon[np], &ier);

	    break;

	case GFA_ELM:

	    crg_setgfa ( el, joffset, elnum, &ier );
 	    
	    break;

        case SGWX_ELM:
            
            crg_setsgwx (el, joffset, elnum, &ier );
            
            break;

	case TCA_ELM:

	    crg_settca(el, joffset, elnum, &ier);

	    break;
    } 
       
}

/*=====================================================================*/

static void crg_settca ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_settca	 							*
 *									*
 * This function sets the range record for a TCA element.		*
 *									*
 * Note: joffset is the offset to the element in WORK_FILE.		*
 *									*
 * void crg_settca ( el, joffset, elnum, iret )			        *
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	joffset		int		Offset to element in WORK_FILE	*
 *	elnum		int		Index of elem. in range arrary	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *									*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	12/05	move from crg_mkRange()   		*
 * S. Gilbert/NCEP	01/06	advisoryNum is now a char               *
 * M. Li/SAIC		05/06	Added a check for no break point	*
 ***********************************************************************/
{
    int		ier, np, counter, ii, jj;
    float	lat[ MAXPTS ], lon[ MAXPTS ], sx[4], sy[4];
    float	minlat, minlon, maxlat, maxlon;
    VG_DBStruct eltmp;
/*---------------------------------------------------------------------*/

    if ( el->elem.tca.info.tcaww[0].numBreakPts <= 0 ) return;

    if ( el->elem.tca.info.wwNum == 0 ) {
        /*
         *  Create temporary string element containing the text
         *  that will be displayed in the zero W/W text box.
         *  Use it to estimate the extent of the text box.
         */
        if ( el->elem.tca.info.stormType == 0 )
             sprintf(eltmp.elem.txt.text,"Hurricane %s\nAdvisory %s\n",el->elem.tca.info.stormName,el->elem.tca.info.advisoryNum);
        else if ( el->elem.tca.info.stormType == 1 )
             sprintf(eltmp.elem.txt.text,"Tropical Storm %s\nAdvisory %s\n",el->elem.tca.info.stormName,el->elem.tca.info.advisoryNum);
        else if ( el->elem.tca.info.stormType == 2 )
             sprintf(eltmp.elem.txt.text,"Tropical Depression %s\nAdvisory %s\n",el->elem.tca.info.stormName,el->elem.tca.info.advisoryNum);
        else if ( el->elem.tca.info.stormType == 3 )
             sprintf(eltmp.elem.txt.text,"Subtropical Storm %s\nAdvisory %s\n",el->elem.tca.info.stormName,el->elem.tca.info.advisoryNum);
        else if ( el->elem.tca.info.stormType == 4 )
             sprintf(eltmp.elem.txt.text,"Subtropical Depression %s\nAdvisory %s\n",el->elem.tca.info.stormName,el->elem.tca.info.advisoryNum);
        strcat ( eltmp.elem.txt.text, "No current Watches/Warnings");
        eltmp.hdr.vg_type = TEXT_ELM;
        eltmp.elem.txt.info.rotn = 0.;
        eltmp.elem.txt.info.sztext = el->elem.tca.info.text_size;
        eltmp.elem.txt.info.itxfn = el->elem.tca.info.text_font;
        eltmp.elem.txt.info.ithw = 2;
        eltmp.elem.txt.info.iwidth = el->elem.tca.info.text_width;
        eltmp.elem.txt.info.ialign = 2;   /* centered */
        eltmp.elem.txt.info.lat = el->elem.tca.info.text_lat;
        eltmp.elem.txt.info.lon = el->elem.tca.info.text_lon;
        eltmp.elem.txt.info.offset_x = 0;
        eltmp.elem.txt.info.offset_y = 0;

        /*  
         *   Calculate range of text box
         */
        crg_gettxtbox( &eltmp, 1, sx, sy );
        np = 4;
        gtrans( "D", "M", &np, sx, sy, lat, lon, &ier, 1, 1 );
        lat [ 4 ] = lat [ 0 ] - 10.0;
        lon [ 4 ] = lon [ 0 ] - 10.0;
        lat [ 5 ] = lat [ 2 ] + 10.0;
        lon [ 5 ] = lon [ 2 ] + 10.0;

        np = 6;
    }
    else {

         /*  
          *   Calculate range using the lat/lons of the breakpoints.
          */
          minlat = el->elem.tca.info.tcaww[ 0 ].breakPnt[ 0 ].lat;
          maxlat = el->elem.tca.info.tcaww[ 0 ].breakPnt[ 0 ].lat;
          minlon = el->elem.tca.info.tcaww[ 0 ].breakPnt[ 0 ].lon;
          maxlon = el->elem.tca.info.tcaww[ 0 ].breakPnt[ 0 ].lon;

          counter = 0;

          for ( jj = 0; jj < el->elem.tca.info.wwNum; jj++  ) {
              for ( ii = 0; ii < el->elem.tca.info.tcaww[ jj ].numBreakPts; ii++ ) {
                  lat[ counter ] = el->elem.tca.info.tcaww[ jj ].breakPnt[ ii ].lat;
                  lon[ counter ] = el->elem.tca.info.tcaww[ jj ].breakPnt[ ii ].lon;
                  minlat = MIN ( minlat, lat[ counter ] );
                  minlon = MIN ( minlon, lon[ counter ] );
                  maxlat = MAX ( maxlat, lat[ counter ] );
                  maxlon = MAX ( maxlon, lon[ counter ] );
                  counter++;
              }
          }

          lat [ counter ]     = minlat - 10;
          lon [ counter ]     = minlon - 10;
          lat [ counter + 1 ] = maxlat + 10;
          lon [ counter + 1 ] = maxlon + 10;

          np = counter + 2;
    } 

    crg_setltln (elnum, joffset, np, lat, lon, &ier);

    return;
}
/*=====================================================================*/

static void crg_setccf ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/******************************************************************************
  crg_setccf
  
  This function sets the range record for a CCF element.
  Note: joffset is the offset to the element in WORK_FILE.
  
  static void crg_setccf ( el, joffset, elnum, iret )
  
  Input parameters:
 	*el		VG_DBStruct	Pointer to VG record structure	
 	joffset		int		Offset to element in WORK_FILE	
 	elnum		int		Index of elem. in range arrary	
 									
  Output parameters:							
 	*iret		int		Return code			
 					0 - Normal			
 					-3 - Range array is full	
**
  Log:
  L. Hinson/AWC         07/09    initial coding
*******************************************************************************/
{
  int		ier, np, ii, indx, inout[1], npls;
  float	lat[ MAXPTS ], lon[ MAXPTS ], xpl[1], ypl[1];
  VG_DBStruct eltmp;
  Boolean	buildSecondRec;
  
  np = el->elem.ccf.info.npts;
	    
  for ( ii = 0; ii < np; ii++ ) {
	lat[ ii ] = el->elem.ccf.latlon [ ii ];
	lon[ ii ] = el->elem.ccf.latlon [ ii + np ];	    
  }
  crg_setltln ( elnum, joffset, np, lat, lon, &ier );
  buildSecondRec = False;
  xpl[0] = el->elem.ccf.info.textlat;
  if ( el->hdr.closed == 0 ) {
    buildSecondRec = True;
  } else {
    npls = 1;
    ypl[0] = el->elem.ccf.info.textlon;
    cgr_inpoly (sys_M, &np, lat, lon, sys_M, &npls, xpl, ypl, inout, &ier );
    if (inout[ 0 ] == 0 ) {
      buildSecondRec = True;
    }
  }
  if ( buildSecondRec ) {
    cds_ccftxt ( el, &eltmp, &ier );
    crg_gassocrec( elnum, &indx, &ier );
    if ( ier !=0 || indx < 0 ) {
      crg_newinx( &indx, &ier );
      if (ier < 0 ) {
        *iret = -3;
        return;
      }
      crg_setauxrec( indx, &ier );
    }
    crg_settxt ( &eltmp, joffset, indx, &ier);
    crg_sarec ( elnum, indx, &ier );
  }    
}


/*=====================================================================*/

static void crg_setgfa ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setgfa	 							*
 *									*
 * This function sets the range record for a GFA element.		*
 *									*
 * Note: joffset is the offset to the element in WORK_FILE.		*
 *									*
 * static void crg_setgfa ( el, joffset, elnum, iret )			*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	joffset		int		Offset to element in WORK_FILE	*
 *	elnum		int		Index of elem. in range arrary	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *					-3 - Range array is full	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/07	initial coding				*
 ***********************************************************************/
{
    int		ier, np, ii, indx, inout[1], npls;
    float	lat[ MAXPTS ], lon[ MAXPTS ], xpl[1], ypl[1];
    char	value[32];
    VG_DBStruct eltmp;
    Boolean	buildSecondRec;
/*---------------------------------------------------------------------*/

    /*
     *  Build first range record from GFA polygon points
     */
    np = el->elem.gfa.info.npts;
	    
    for ( ii = 0; ii < np; ii++ ) {
	lat[ ii ] = el->elem.gfa.latlon [ ii ];
	lon[ ii ] = el->elem.gfa.latlon [ ii + np ];	    
    }
    
    crg_setltln ( elnum, joffset, np, lat, lon, &ier );
	    
    
    /*
     *  We need to build a second range record if:
     *  1. it is an open FZLVL contour
     *  2. it is closed but the text centroid is outside of GFA polygon 
     *  
     *  Note: Do not try to create range record before the text box 
     *        being placed.
     */
    buildSecondRec = False;
    
    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    xpl[ 0 ] = atof ( value );
    
    if ( G_ABS ( xpl[ 0 ] ) > 90.0F ) {
	return;        
    }
    
    if ( el->hdr.closed == 0 ) {
         buildSecondRec = True;    
    }
    else {           
        npls = 1;
	
	cvg_getFld ( el, TAG_GFA_LON, value, &ier );
	ypl[ 0 ] = atof ( value );
	
	cgr_inpoly ( sys_M, &np, lat, lon, sys_M, &npls, xpl, ypl, inout, &ier );
        
	if ( inout[ 0 ] == 0 ) {          
            buildSecondRec = True;
        }
    }
         
    
    /*
     *  Create a temporary text element from the GFA text, build a
     *  range record for it but link it to the GFA's location.
     */
    if ( buildSecondRec ) {

	/*
         *  Create a temporary text element from the GFA text
         */
        cds_gfatxt ( el, &eltmp, &ier );


	/*  
         *   make range record for text box but link it to GFA
         */
	crg_gassocrec ( elnum, &indx, &ier );
	
	if ( ier != 0 || indx < 0 ) {
	    
	    crg_newinx ( &indx, &ier );
	    
	    if ( ier < 0 ) {
	        *iret = -3;
	        return;
	    }
	    
	    crg_setauxrec ( indx, &ier );
        }
	
		
	crg_settxt ( &eltmp, joffset, indx, &ier);
        
        
	/*  
         *   set the second range record for "elnum".
         */
	crg_sarec ( elnum, indx, &ier );
    }
    
}

/*====================================================================*/

static void crg_setsgwx ( VG_DBStruct *el, int joffset, int elnum, int *iret)
/*************************************************************************
 crg_setsgwx
 This function sets the range record for a SGWX element.
 Note: joffset is the offset to the element in WORK_FILE.
 static void crg_setsgwx (el, joffset, elnum, iret )
 Input parameters:
 	*el		VG_DBStruct	Pointer to VG record structure	
 	joffset		int		Offset to element in WORK_FILE	
 	elnum		int		Index of elem. in range arrary	
 									
  Output parameters:							
 	*iret		int		Return code			
 					0 - Normal			
 					-3 - Range array is full	
**
  Log:
  L. Hinson/AWC         01/12       initial coding
*************************************************************************/
{
  int		ier, np, ii, indx, inout[1], npls;
  float	lat[ MAXPTS ], lon[ MAXPTS ], xpl[1], ypl[1];
  VG_DBStruct eltmp;
  Boolean	buildSecondRec;
  
  np = el->elem.sgwx.info.npts;
	    
  for ( ii = 0; ii < np; ii++ ) {
	lat[ ii ] = el->elem.sgwx.latlon [ ii ];
	lon[ ii ] = el->elem.sgwx.latlon [ ii + np ];	    
  }
  crg_setltln ( elnum, joffset, np, lat, lon, &ier );
  buildSecondRec = False;
  xpl[0] = el->elem.sgwx.info.textlat;
  if ( el->hdr.closed == 0 ) {
    buildSecondRec = True;
  } else {
    npls = 1;
    ypl[0] = el->elem.sgwx.info.textlon;
    cgr_inpoly (sys_M, &np, lat, lon, sys_M, &npls, xpl, ypl, inout, &ier );
    if (inout[ 0 ] == 0 ) {
      buildSecondRec = True;
    }
  }
  if ( buildSecondRec ) {
    cds_sgwxtxt ( el, &eltmp, &ier );
    crg_gassocrec( elnum, &indx, &ier );
    if ( ier !=0 || indx < 0 ) {
      crg_newinx( &indx, &ier );
      if (ier < 0 ) {
        *iret = -3;
        return;
      }
      crg_setauxrec( indx, &ier );
    }
    crg_settxt ( &eltmp, joffset, indx, &ier);
    crg_sarec ( elnum, indx, &ier );
  }    
}

/*=====================================================================*/
