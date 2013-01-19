#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

#define FONT_TYPE	2
#define TEXT_FONT	1
#define TEXT_SIZE	1
#define TEXT_WIDTH	1

static char	*_topsLabels[]	= {"400+", "350-390", "300-340", "250-290"}; 
static char	*_growLabels[]	= {"+", "NC", "-"}; 
static char	*_probLabels[]	= {"50-100%", "25-49%"}; 
static char	*_covrLabels[]	= {"75-100%", "40-74%", "25-39%"}; 

void cds_ccftxt(const VG_DBStruct *el, VG_DBStruct *txt_el, int *iret)
/******************************************************************************
  cds_ccftxt
  
  This routine fills in a given VG_DBStruct with the information from the CCF
  element necessary to create the text box to display for the element
    
  Input parameters:
  *el       VG_DBStruct     CCF VG object to process
  
  Output parameters:
  *txt_el   VG_DBStruct     Temporary VG object to create for the text
  *iret     int     Return code
                    0 = Function successful
                   -4 = Invalid pointer in arguments
                   -5 = Problem in Parsing Text Layout String
                    3 = "NIL" found in Text Layout String
**
  Log:
  L. Hinson/AWC    07/09  Created
*******************************************************************************/
{
  char tag[32];
  int ier=0;
  int grpnum;
  int idx;
  char textLayoutStr[256];
  
  if (!el || !txt_el) {
    *iret = -4;
    return;
  }
  *iret = 0;
  
  textLayoutStr[ 0 ] = '\0';
  
  sprintf ( tag, "<vg_type>%d<vg_class>%d", SPTX_ELM, CLASS_TEXT );
  cvg_t2v ( tag, txt_el, &ier );
  txt_el->hdr.maj_col = el->hdr.maj_col;
  txt_el->elem.spt.info.sptxtyp = 17; /* Bounded, backfilled box */
  txt_el->hdr.grptyp = GRPTYP_CCF;
  crg_ggnxt (GRPTYP_CCF, &grpnum, &ier);
  txt_el->hdr.grpnum = grpnum;
  txt_el->elem.spt.info.lat = el->elem.ccf.info.textlat;
  txt_el->elem.spt.info.lon = el->elem.ccf.info.textlon;
  
  txt_el->elem.spt.info.offset_x = 0;
  txt_el->elem.spt.info.offset_y = 0; 
  txt_el->elem.spt.info.rotn     = 0;
  txt_el->elem.spt.info.txtcol = el->hdr.maj_col;
  txt_el->elem.spt.info.lincol	= el->hdr.maj_col;
  txt_el->elem.spt.info.filcol	= el->hdr.maj_col;
  
  /* Now load the setting/userattributes table */
  cds_getinx((VG_DBStruct *) el, &idx, &ier);
  
  if (ier >= 0 ) {
    /* Get the Text Layout String */
    if ( cdsUattr[idx].info.ccf->textLayout[0]=='\0' ) {
      strcpy(textLayoutStr, el->elem.ccf.textLayout);
    } else {
      strcpy(textLayoutStr, cdsUattr[idx].info.ccf->textLayout);
    }
    if (strstr(textLayoutStr,"NIL") != NULL) {
      *iret = 3;
      return;
    }    
    /* Get the Text Size */
    if ( fabs (cdsUattr[idx].info.ccf->info.sztext) < 0.01 ) { /* sztext == 0.0 ? */
      txt_el->elem.spt.info.sztext = el->elem.ccf.spt.info.sztext;
    } else {
      txt_el->elem.spt.info.sztext = cdsUattr[idx].info.ccf->info.sztext;
    }
    /* Get the Text Font */
    if ( cdsUattr[idx].info.ccf->info.itxfn == 0 ) {
      txt_el->elem.spt.info.itxfn = el->elem.ccf.spt.info.itxfn;
    } else {
      txt_el->elem.spt.info.itxfn = cdsUattr[idx].info.ccf->info.itxfn;
    }
    /* Get the Hardware/Software Font Setting */
    if ( cdsUattr[idx].info.ccf->info.ithw == 0 ) {
      txt_el->elem.spt.info.ithw = el->elem.ccf.spt.info.ithw;
    } else {
      txt_el->elem.spt.info.ithw = cdsUattr[idx].info.ccf->info.ithw;
    }
    if ( cdsUattr[idx].info.ccf->info.iwidth == 0 ) {
      txt_el->elem.spt.info.iwidth = el->elem.ccf.spt.info.iwidth;
    } else {
      txt_el->elem.spt.info.iwidth = cdsUattr[idx].info.ccf->info.iwidth;
    }
    if ( cdsUattr[idx].info.ccf->info.ialign == 0 ) {
      txt_el->elem.spt.info.ialign = el->elem.ccf.spt.info.ialign;
    } else {
      txt_el->elem.spt.info.ialign = cdsUattr[idx].info.ccf->info.ialign;
      if (txt_el->elem.spt.info.ialign == 2)
        txt_el->elem.spt.info.ialign = 0;
    }
  }       
  inc_pccftxt(_topsLabels[el->elem.ccf.info.tops - 1],
              _growLabels[el->elem.ccf.info.growth - 2],
              _probLabels[(el->elem.ccf.info.prob - 1) / 2],
              _covrLabels[el->elem.ccf.info.cover - 1],
              textLayoutStr, txt_el->elem.spt.text, &ier);
              
  if (ier != 0) {
    *iret = -5;
  }
               
}
  
