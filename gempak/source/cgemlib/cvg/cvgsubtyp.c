#include "cvgcmn.h"

void cvg_subtyp ( VG_DBStruct *el, int *subtyp, int *iret )
/************************************************************************
 * cvg_subtyp								*
 *									*
 * This function gets the subtyp for one element.			*
 *									*
 * cvg_subtyp ( el, subtyp, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Vector data structure		*
 *									*
 * Output parameters:							*
 *	*subtyp		int		GEMPAK type for this element	*
 *	*iret		int		Return code			*
 *					 -2 = Failure to return subtype	*
 **									*
 * Log:									*
 * F.J. Yen/NCEP	10/99	Created					*
 * S. Law/GSC		02/00	Added CCF				*
 * A. Hardy/SAIC	11/01   Renamed from cds_subtyp			*
 * A. Hardy/SAIC	 2/01   Cleaned up DEFAULT case			*
 * J. Wu/SAIC	 	06/03   handle LIST elements			*
 * D.W.Plummer/NCEP	06/03	added ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC	 	09/03   add JET_ELM				*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC	 	01/04   add GFA_ELM				*
 * B. Yin/SAIC		02/04	added TCA_ELM				*
 * J. Wu/SAIC		10/04   get GFA sub type using cvg_getFld()	*
 * m.gamazaychikov/SAIC	07/07   add TCE, TCB, TCT elems			*
 * L. Hinson/AWC        01/12   Add SGWX_ELM                            *
 ***********************************************************************/
{
    int		ier;
    char	value[32];
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    switch (el->hdr.vg_type) {
      case FRONT_ELM:
	*subtyp  = (el->elem.frt.info.fcode /100) * 100;
	break;

      case CIRCLE_ELM:
	*subtyp = el->elem.cir.info.lintyp;
	break;

      case LINE_ELM:
	*subtyp = el->elem.lin.info.lintyp;
	break;

      case SPLN_ELM:
	*subtyp = el->elem.spl.info.spltyp;
	break;

      case WXSYM_ELM:
      case CTSYM_ELM:
      case ICSYM_ELM:
      case PTSYM_ELM:
      case PWSYM_ELM:
      case SKSYM_ELM:
      case SPSYM_ELM:
      case TBSYM_ELM:
      case CMBSY_ELM:
      case  MARK_ELM:
	*subtyp = (int) el->elem.sym.data.code[0];
	break;

      case ARROW_ELM:
      case BARB_ELM:
      case DARR_ELM:
      case HASH_ELM:
	*subtyp = el->elem.wnd.info.wndtyp;
	break;

      case WBOX_ELM:
	*subtyp   = el->elem.wbx.info.w_type;
	break;

      case TEXT_ELM:
      case TEXTC_ELM:
	*subtyp = el->elem.txt.info.ithw;
	break;

      case SPTX_ELM:
	*subtyp = el->elem.spt.info.sptxtyp;
	break;

      case TRKSTORM_ELM:
	*subtyp = el->elem.trk.info.subtype;
	break;

      case SIGAIRM_ELM:
      case SIGCONV_ELM:
      case SIGINTL_ELM:
      case SIGNCON_ELM:
      case SIGOUTL_ELM:
	*subtyp = el->elem.sig.info.subtype;
	break;

      case SIGCCF_ELM:
	*subtyp = el->elem.ccf.info.subtype;
	break;

      case ASHCLD_ELM:
	*subtyp = el->elem.ash.info.subtype;
	break;

      case VOLC_ELM:
	*subtyp = (int)el->elem.vol.info.code;
	break;

      case LIST_ELM:
	*subtyp = el->elem.lst.info.subtyp;
	break;

      case JET_ELM:
	*subtyp = el->elem.jet.line.spl.info.spltyp;
	break;

      case GFA_ELM:
	cvg_getFld ( el, TAG_GFA_SUBTYPE, value, &ier );
	*subtyp = G_MAX ( atoi ( value ), 0 );
	break;

      case SGWX_ELM:
        *subtyp = el->elem.sgwx.info.subtype;
        break;

      case TCA_ELM:
	*subtyp = 0;
	break;

      case TCERR_ELM:
	*subtyp = 0;
	break;

      case TCTRK_ELM:
	*subtyp = 0;
	break;

      case TCBKL_ELM:
	*subtyp = 0;
	break;

      default:
	*subtyp = IMISSD;
        *iret = -49;
	break;
    }

}
