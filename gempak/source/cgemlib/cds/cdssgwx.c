#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

#define  RADSYM      41.0F

void cds_sgwx ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_sgwx								*
 *									*
 * This function displays SGWX Elements and associated connectors       *
 * to the output device.			                        *
 *									*
 * cds_sgwx (el, indx, iret)						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **			                                                *
 * Log:                                                                 *
 * L. Hinson/AWC    11/11   Created                                     *
 ***********************************************************************/
{
  int		ii, np, ier, istyp, width, lintyp, lthw, lwhw;
  int lineElm, lineType, lineWidth, lineColor;
  int color, type;  
  int idx, npts;
  float *xpt, *ypt, *xpl, *ypl;
  int *inout;
  int npls;
  
  int         ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx, imarkx;
  int         imkhwx, imkwidx, itxfnx, itxhwx, itxwidx, ibrdrx;
  int         irrotnx, ijustx;
  float       densx, sztextx, szmarkx;
  VG_DBStruct el_tmp;
  SGWXType  *psgwx;
/*---------------------------------------------*/

  *iret = 0;
  cds_getinx(el, &idx, &ier );
  if (ier >= 0) {
   /*
    * Save plot attributes.
    */
    gqsmth ( &ismtypx, &densx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    
    lintyp = 1;
    lthw = 0;
    width = 3;
    lwhw = 0;
    gsline (&lintyp, &lthw, &width, &lwhw, &ier);
    
    psgwx = &(el->elem.sgwx);
    np = psgwx->info.npts;
    /*
     * Set the color for the feature
    */
    if (cdsUattr[indx].maj_col == 0) {
        gscolr ( &(el->hdr.maj_col), &ier);
        lineColor = el->hdr.maj_col;
    } else {
        /* Use the Uattrib table */
        gscolr ( &cdsUattr[indx].maj_col, &ier);
        lineColor = cdsUattr[indx].maj_col;
    }    
      
    /* Set the smoothing */
    ii = (cdsUattr[indx].smooth == -1) ?
	 (int) el->hdr.smooth : cdsUattr[indx].smooth;
    istyp = (ii == 0) ? 0 : 2;
    gssmth (&istyp, &cdsSmthDens[ii], &ier); 
     
    /* Set the  line element, line type, line width */
    if (cdsUattr[indx].info.sgwx->lineelm == 0) {
      lineElm = el->elem.sgwx.info.lineelm;
    } else {
      lineElm = cdsUattr[indx].info.sgwx->lineelm;
    }
    if (cdsUattr[indx].info.sgwx->linetype == 0) {
      lineType = el->elem.sgwx.info.linetype;
    } else {
      lineType = cdsUattr[indx].info.sgwx->linetype;
    }
    if (cdsUattr[indx].info.sgwx->linewidth == 0) {
      lineWidth = el->elem.sgwx.info.linewidth;
    } else {
      lineWidth = cdsUattr[indx].info.sgwx->linewidth;
    }
    
    /* Area we a Special Symbol/Weather Symbol? */
    if (np == 1) {
      el_tmp.hdr = el->hdr;
      el_tmp.hdr.vg_class = (char)CLASS_SYMBOLS;
      if (el->elem.sgwx.info.splsym > 0) {
        el_tmp.hdr.vg_type = (char)SPSYM_ELM;        
        el_tmp.elem.sym.data.code[0] = el->elem.sgwx.info.splsym;
      } else {
        el_tmp.hdr.vg_type = (char)WXSYM_ELM;
        el_tmp.elem.sym.data.code[0] = el->elem.sgwx.info.wxsym;
      }
      el_tmp.hdr.maj_col = lineColor;
      el_tmp.hdr.min_col = lineColor;
      el_tmp.hdr.delete = 0;
      el_tmp.hdr.smooth = 0;
      el_tmp.hdr.version = 0;
      el_tmp.hdr.filled = lineColor;
      el_tmp.hdr.closed = 0;
      el_tmp.elem.sym.info.numsym =  1;
      el_tmp.elem.sym.info.width = 1;
      el_tmp.elem.sym.info.size = 1.;
      el_tmp.elem.sym.info.ityp = 0;
      el_tmp.elem.sym.data.latlon[0] = el->elem.sgwx.latlon[0];
      el_tmp.elem.sym.data.latlon[1] = el->elem.sgwx.latlon[1];
      el_tmp.elem.sym.data.offset_xy[0] = 0;
      el_tmp.elem.sym.data.offset_xy[1] = 0;
    } else {
      /* Build a LINE_ELM to display the SGWX bound. */
      np = el->elem.sgwx.info.npts;
      el_tmp.hdr = el->hdr;
      el_tmp.hdr.vg_class = (char)CLASS_LINES;
      el_tmp.hdr.maj_col = lineColor;
      el_tmp.hdr.smooth = ii;

      if (lineElm == SPLN_ELM) {
        el_tmp.hdr.vg_type = (char)SPLN_ELM;
        el_tmp.elem.spl.info.numpts = np;
        el_tmp.elem.spl.info.spltyp = lineType;  

        el_tmp.elem.spl.info.splwid = lineWidth; 
        el_tmp.elem.spl.info.splsiz = 1.;
        el_tmp.elem.spl.info.splstr = 0.5;
        el_tmp.elem.spl.info.spldir = 1;
        for ( ii = 0; ii < np; ii++ ) {
             el_tmp.elem.spl.latlon[ii]       = el->elem.sgwx.latlon[ii];
             el_tmp.elem.spl.latlon[ii+np]    = el->elem.sgwx.latlon[ii+np];
        }
      } else {
        el_tmp.hdr.vg_type = (char)LINE_ELM;
        el_tmp.elem.lin.info.numpts = np;
        el_tmp.elem.lin.info.lintyp = lineType; 
        el_tmp.elem.lin.info.lthw   = 0;
        el_tmp.elem.lin.info.lwhw   = 0;

        el_tmp.elem.lin.info.width  = lineWidth; 

        for ( ii = 0; ii < np; ii++ ) {
             el_tmp.elem.lin.latlon[ii]	= el->elem.sgwx.latlon[ii];
             el_tmp.elem.lin.latlon[ii+np]	= el->elem.sgwx.latlon[ii+np];
        }
      }
    }
    
    cds_getinx ( &el_tmp, &idx, &ier );

    color = cdsUattr[idx].maj_col;
    type = cdsUattr[idx].info.lin->lintyp;
    width = cdsUattr[idx].info.lin->width;

    cds_dspelm (&el_tmp, &ier );

    cdsUattr[idx].maj_col = color;
    cdsUattr[idx].info.lin->lintyp = type;
    cdsUattr[idx].info.lin->width = width;
    
    /*
     *  Build a SPLN_ELM before SPTX_ELM is drawn.
     *  Save the attr settings before display, restore after display.
     */ 
    npts = 1;
    G_MALLOC( xpt, float, npts, "cds_sgwx: xpt" );
    G_MALLOC( ypt, float, npts, "cds_sgwx: ypt" );
    G_MALLOC( inout, int, npts, "cds_sgwx: inout" );
    xpt[0] = el->elem.sgwx.info.textlat;
    ypt[0] = el->elem.sgwx.info.textlon;
    npls = el->elem.sgwx.info.npts;
    G_MALLOC( xpl, float, npls, "cds_sgwx: xpl" );
    G_MALLOC( ypl, float, npls, "cds_sgwx: ypl" );
    for ( ii = 0; ii < npls; ii++ ) {
        xpl[ii] = el->elem.sgwx.latlon[ii];
        ypl[ii] = el->elem.sgwx.latlon[ii+npls];
    }
    cgr_inpoly ( sys_M, &npts, xpt, ypt, sys_M, &npls, xpl, ypl, inout, &ier );
    /*
     * If the center of the text box is outside of GFA polygon, and TextLayoutNIL
     * not set display an arrowed line.
    */
    /* if ( inout[0] == 0  && el->elem.sgwx.info.splsym != RADSYM) { */
    if ( inout[0] == 0 ) {
        el_tmp.hdr.delete   = 0;
        el_tmp.hdr.vg_type  = SPLN_ELM;
        el_tmp.hdr.vg_class = (char)CLASS_LINES;
        el_tmp.hdr.filled   = 0;
        el_tmp.hdr.closed   = 0;
        el_tmp.hdr.smooth   = 0;
        el_tmp.hdr.version  = 0;
        el_tmp.hdr.grptyp   = 0;
        el_tmp.hdr.grpnum   = 0;
        el_tmp.hdr.maj_col  = el->hdr.maj_col;
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
        el_tmp.elem.spl.info.splsiz = 1.0;
        el_tmp.elem.spl.info.splsiz = el->elem.sgwx.info.szarrow;
        /* Set the Arrow Size from the settings table... */
        if ( fabs (cdsUattr[indx].info.sgwx->szarrow < 0.01) ) {
          el_tmp.elem.spl.info.splsiz = el->elem.sgwx.info.szarrow;
        } else {
          el_tmp.elem.spl.info.splsiz = cdsUattr[indx].info.sgwx->szarrow;
        }
                        
        el_tmp.elem.spl.latlon[0] = el->elem.sgwx.info.textlat;
        el_tmp.elem.spl.latlon[1] = el->elem.sgwx.info.arrowlat;
        el_tmp.elem.spl.latlon[2] = el->elem.sgwx.info.textlon;
        el_tmp.elem.spl.latlon[3] = el->elem.sgwx.info.arrowlon;
        if (el_tmp.elem.spl.info.splsiz > 0.01) {
          /* Change width 1 to 3*/
          el_tmp.elem.spl.info.splwid = 3;
          el_tmp.hdr.maj_col = el_tmp.hdr.min_col = 31;        
          cds_dspelm(&el_tmp, &ier);
          el_tmp.elem.spl.info.splwid = 1;
          el_tmp.hdr.maj_col = el_tmp.hdr.min_col = 32;
          cds_dspelm(&el_tmp, &ier);
        }

    }
    G_FREE(xpl, float);
    G_FREE(ypl, float);
    G_FREE(xpt, float);
    G_FREE(ypt, float);
    G_FREE(inout, int);
    
    cds_sgwxtxt(el, &el_tmp, &ier);
    /* Change Text color/box to black */
    /* el_tmp.elem.spt.info.txtcol = 32;*/
    cds_getinx( &el_tmp, &idx, &ier );
    color = cdsUattr[idx].maj_col;
    type = cdsUattr[idx].info.spt->text.info.sptxtyp;
    
    cds_dspelm( &el_tmp, &ier );
    
    cdsUattr[idx].maj_col = color;
    cdsUattr[idx].info.spt->text.info.sptxtyp = type;
    
    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );
    
  }
}
    
   
    
    
    
    

    
    
      
    
    
      
    
      
    
    
