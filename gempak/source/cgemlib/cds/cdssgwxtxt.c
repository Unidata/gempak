#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

void cds_sgwxtxt(const VG_DBStruct *el, VG_DBStruct *txt_el, int *iret)
/******************************************************************************
  cds_sgwxtxt
  
  This routine fills in a given VG_DBStruct with the information from the SGWX
  element necessary to create the text box to display for the element
    
  Input parameters:
  *el       VG_DBStruct     SGWX VG object to process
  
  Output parameters:
  *txt_el   VG_DBStruct     Temporary VG object to create for the text
  *iret     int     Return code
                    0 = Function successful
                   -4 = Invalid pointer in arguments
**
  Log:
  L. Hinson/AWC    11/11  Created
*******************************************************************************/
{
    int ier=0;
    int grpnum;
    int idx;
    /*
     * Create the text box element
     */
    txt_el->hdr.vg_type = SPTX_ELM;
    txt_el->hdr.vg_class = (char)CLASS_TEXT;
    txt_el->hdr.maj_col = el->hdr.maj_col;
    txt_el->elem.spt.info.sptxtyp = el->elem.sgwx.spt.info.sptxtyp;
    
    txt_el->hdr.grptyp = GRPTYP_SGWX;
    crg_ggnxt(GRPTYP_SGWX,&grpnum,&ier);
    txt_el->hdr.grpnum = grpnum;
    txt_el->elem.spt.info.lat = el->elem.sgwx.info.textlat;
    txt_el->elem.spt.info.lon = el->elem.sgwx.info.textlon;
    txt_el->elem.spt.info.offset_x = 0;
    txt_el->elem.spt.info.offset_y = 0;
    txt_el->elem.spt.info.rotn = 0;
    txt_el->elem.spt.info.txtcol = el->hdr.maj_col;
    txt_el->elem.spt.info.lincol = el->hdr.maj_col;
    txt_el->elem.spt.info.filcol = el->hdr.maj_col;
    txt_el->elem.spt.info.turbsym = el->elem.sgwx.spt.info.turbsym;
    /* Now load the setting/userattributes table */
    cds_getinx((VG_DBStruct *) el, &idx, &ier);
    
    if (ier >= 0) {        
      /* Get the Text Size */
      if ( fabs (cdsUattr[idx].info.sgwx->info.sztext) < 0.01 ) { /* sztext == 0.0 ? */
        txt_el->elem.spt.info.sztext = el->elem.sgwx.spt.info.sztext;
      } else {
        txt_el->elem.spt.info.sztext = cdsUattr[idx].info.sgwx->info.sztext;
      }
      /* Get the Text Font */
      if ( cdsUattr[idx].info.sgwx->info.itxfn == 0 ) {
        txt_el->elem.spt.info.itxfn = el->elem.sgwx.spt.info.itxfn;
      } else {
        txt_el->elem.spt.info.itxfn = cdsUattr[idx].info.sgwx->info.itxfn;
      }
      /* Get the Hardware/Software Font Setting */
      if ( cdsUattr[idx].info.sgwx->info.ithw == 0 ) {
        txt_el->elem.spt.info.ithw = el->elem.sgwx.spt.info.ithw;
      } else {
        txt_el->elem.spt.info.ithw = cdsUattr[idx].info.sgwx->info.ithw;
      }
      if ( cdsUattr[idx].info.sgwx->info.iwidth == 0 ) {
        txt_el->elem.spt.info.iwidth = el->elem.sgwx.spt.info.iwidth;
      } else {
        txt_el->elem.spt.info.iwidth = cdsUattr[idx].info.sgwx->info.iwidth;
      }
      if ( cdsUattr[idx].info.sgwx->info.ialign == 0 ) {
        txt_el->elem.spt.info.ialign = el->elem.sgwx.spt.info.ialign;
      } else {
        txt_el->elem.spt.info.ialign = cdsUattr[idx].info.sgwx->info.ialign;
        if (txt_el->elem.spt.info.ialign == 2)
          txt_el->elem.spt.info.ialign = 0;
      }
    }
    strcpy(txt_el->elem.spt.text,el->elem.sgwx.spt.text);
}
