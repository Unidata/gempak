#include "cvgcmn.h"

void cvg_setsubtyp ( int new_subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_setsubtyp							*
 *									*
 * This function sets the subtyp for one element.			*
 *									*
 * cvg_setsubtyp ( new_subtyp, el, iret )				*
 *									*
 * Input parameters:							*
 *	new_subtyp	int		GEMPAK type for this element	*
 *									*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	Vector data structure		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -49 = invalid vg_type in *el  	*
 *					 -50 = invalid new subtype value*
 **									*
 * Log:									*
 * E. Safford/SAIC	02/02   initial coding         			*
 * D.W.Plummer/NCEP	06/03	added ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC		09/03   add LIST_ELM & JET_ELM			*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		01/04   add GFA_ELM				*
 * B. Yin/SAIC		02/04	added TCA_ELM				*
 * J. Wu/SAIC		10/04   set GFA sub type using cvg_setFld()	*
 * L. Hinson/AWC        01/12   Add SGWX_ELM                            *
 ***********************************************************************/
{
    int		ier;
    char	value[32];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if (new_subtyp < 0) {
	*iret = -50;
	return;
    }


    switch (el->hdr.vg_type) {
      case FRONT_ELM:
	el->elem.frt.info.fcode = new_subtyp;
	break;

      case CIRCLE_ELM:
	el->elem.cir.info.lintyp = new_subtyp;
	break;

      case LINE_ELM:
	el->elem.lin.info.lintyp = new_subtyp;
	break;

      case SPLN_ELM:
	el->elem.spl.info.spltyp = new_subtyp;
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
	el->elem.sym.data.code[0] = (float)new_subtyp;
	break;

      case ARROW_ELM:
      case BARB_ELM:
      case DARR_ELM:
      case HASH_ELM:
	el->elem.wnd.info.wndtyp = new_subtyp;
	break;

      case WBOX_ELM:
	el->elem.wbx.info.w_type = new_subtyp;
	break;

      case TEXT_ELM:
      case TEXTC_ELM:
	el->elem.txt.info.ithw = new_subtyp;
	break;

      case SPTX_ELM:
	el->elem.spt.info.sptxtyp = new_subtyp;
	break;

      case TRKSTORM_ELM:
	el->elem.trk.info.subtype = new_subtyp;
	break;

      case SIGAIRM_ELM:
      case SIGCONV_ELM:
      case SIGINTL_ELM:
      case SIGNCON_ELM:
      case SIGOUTL_ELM:
	el->elem.sig.info.subtype = new_subtyp;
	break;

      case SIGCCF_ELM:
	el->elem.ccf.info.subtype = new_subtyp;
	break;

      case ASHCLD_ELM:
	el->elem.ash.info.subtype = new_subtyp;
	break;

      case VOLC_ELM:
	el->elem.vol.info.code = (float)new_subtyp;
	break;
      
      case LIST_ELM:
	el->elem.lst.info.subtyp = new_subtyp;
	break;

      case JET_ELM:
	el->elem.jet.line.spl.info.spltyp = new_subtyp;
	break;

      case GFA_ELM:
	sprintf ( value, "%d", new_subtyp );
	cvg_setFld ( el, TAG_GFA_SUBTYPE, value, &ier );
	break;

      case SGWX_ELM:
        el->elem.sgwx.info.subtype = new_subtyp;
        break;

      case TCA_ELM:
	break;

      default:
        *iret = -49;
	break;
    }

}
