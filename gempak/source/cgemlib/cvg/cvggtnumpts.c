#include "cvgcmn.h"

int cvg_gtnumpts ( VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_gtnumpts								*
 *									*
 * This function returns the number of points in the el.               	*
 *									*
 * cvg_gtnumpts ( el, iret )						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct     VG element     			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 0 = normal			*
 *					 1 = single point vg element    *
 *					     like symbol, text, etc.	*
 *					-1 = error			*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/03	initial coding            		*
 * J. Wu/SAIC		02/04	add GFA_ELM            			*
 ***********************************************************************/
{
   int	numPts = 0;
/*---------------------------------------------------------------------*/
   *iret = 0;
   
   switch ( el->hdr.vg_type ) {

      case LINE_ELM:
	 numPts = el->elem.lin.info.numpts;
	 break;

      case FRONT_ELM:
	 numPts = el->elem.frt.info.numpts;
	 break;

      case SPLN_ELM:
	 numPts = el->elem.spl.info.numpts;
	 break;

      case WBOX_ELM:
         numPts = el->elem.wbx.info.numpts;
	 break;

      case WSM_ELM:
	 numPts = el->elem.wsm.info.numpts;
	 break;

      case CIRCLE_ELM:
	 numPts = el->elem.cir.info.numpts;
	 break;

      case TRKSTORM_ELM:
	 numPts = el->elem.trk.info.npts;
	 break;

      case SIGINTL_ELM:
      case SIGNCON_ELM:
      case SIGCONV_ELM:
      case SIGOUTL_ELM:
      case SIGAIRM_ELM:
	 numPts = el->elem.sig.info.npts;
	 break;

      case SIGCCF_ELM:
	 numPts = el->elem.ccf.info.npts;
	 break;

      case ASHCLD_ELM:
	 numPts = el->elem.ash.info.npts;
	 break;

      case JET_ELM:
	 numPts = el->elem.jet.line.spl.info.numpts;
	 break;

      case GFA_ELM:
	 numPts = el->elem.gfa.info.npts;
	 break;

      case WXSYM_ELM:
      case WCNTY_ELM:
      case BARB_ELM:
      case ARROW_ELM:
      case CTSYM_ELM:
      case ICSYM_ELM:
      case PTSYM_ELM:
      case PWSYM_ELM:
      case SKSYM_ELM:
      case SPSYM_ELM:
      case TBSYM_ELM:
      case TEXT_ELM:
      case TEXTC_ELM:
      case MARK_ELM:
      case SPTX_ELM:
      case FILEHEAD_ELM:
      case DARR_ELM:
      case HASH_ELM:
      case CMBSY_ELM:
      case LIST_ELM:
      case VOLC_ELM:
	 *iret  = 1;
	 numPts = 1;
	 break;

      default:
         *iret = -1;
	 break;
   }  /* end of switch */	   
   
   return ( numPts ); 
}
