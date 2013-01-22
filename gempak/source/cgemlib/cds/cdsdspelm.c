#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"


void cds_dspelm ( VG_DBStruct *el, int *iret )
/************************************************************************
 * cds_dspelm								*
 *									*
 * This function displays a VG record (element) to the device.		*
 *									*
 * cds_dspelm ( el, iret )		 				*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	11/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 2/97	Added selected mode			*
 * S. Jacobs/NCEP	 3/97	Added "break" for BARB/ARRW case	*
 * D. Keiser/GSC	 4/97	Added contour				*
 * D. Keiser/GSC	 4/97	Added special line			*
 * E. Wehner/EAi	 5/97	Removed GEPLOT calls			*
 * D. Keiser/GSC	 5/97	Renamed from cvg_dsply			*
 * E. Safford/GSC	 6/97	Added special text    			*
 * E. Wehner/Eai	 8/97	Removed calls for watchbox display	*
 * C. Lin/EAI	 	 8/97	Remove input parms (grP, selected)	*
 * E. Wehner/EAi	 9/97	Remove Graphics Info record		*
 * I. Durham/GSC	 4/98	Added DARR_ELM and HASH_ELM		*
 * F. J. Yen/NCEP	 5/98	Renamed from cds_dspvg.  Removed return *
 *				codes. Process on class instead of type *
 * A. Hardy/GSC		12/98	Added CLASS_CIRCLE                      *
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		05/99	Added CLASS_SIGMETS			*
 * F. J. Yen/NCEP	10/99	Handled User Attribute Table, cdsUattr	*
 * F. J. Yen/NCEP	10/99	Ignored file header			*
 * S. Law/GSC		02/00	Added CCF				*
 * E. Safford/SAIC	02/02	add subtyp replacement w/ new_subtyp	*
 * D. Kidwell/NCEP	 6/02	Added call to gsgtgn                    *
 * J. Wu/SAIC		09/02	add CLASS_LIST				*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * J. Wu/SAIC		06/03	remove constraints on LIST elements	*
 * D.W.Plummer/NCEP	06/03	add code for ash and volc elements	*
 * J. Wu/SAIC		09/03	add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC		01/04	add CLASS_MET -> GFA_ELM		*
 * B. YIN/SAIC		02/04	added CLASS_MET -> TCA_ELM		*
 * m.gamazaychikov/SAIC	06/07	add CLASS_MET -> TCERR,TCTRK,TCBKL elms	*
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 ***********************************************************************/
{
    int		indx, igt, ign;
    char	logstr[2], grp[4];
    int		loglevl, ier, ier1;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( (int)el->hdr.vg_type == FILEHEAD_ELM ) {
	return;
    }

    /*  
     *  Retrieve the index to this type of element in the settings array
     *  by making a call to cds_getinx.
     */ 
    cds_getinx (el, &indx, iret);

    if ( *iret == -2 ) {
        loglevl = 2;
	strcpy (grp, "CDS");
	strcpy (logstr, "\0");
	er_lmsg ( &loglevl, grp, iret, logstr, &ier1, strlen(grp),
	        strlen(logstr) );
	return;
    }

    /*
     *  If major color is -1 in the user setting table, then the
     *	element is not displayed 
     */
    if ( cdsUattr[indx].maj_col == -1 ) {
	return;
    }

    /*
     *  Replace the subtype if there is a valid new_subtyp value.
     */
    if ( cdsUattr[indx].cds_or_ces.new_subtyp >= 0 ) {
	cvg_setsubtyp (cdsUattr[indx].cds_or_ces.new_subtyp, el, &ier);
    }

    /*
     *  Set the current group type and group number.
     */
    igt = (int)el->hdr.grptyp;
    ign = el->hdr.grpnum;
    gsgtgn ( &igt, &ign, &ier );    

    /* 
     *  Process based on the class in the header of the element.
     */
    switch (el->hdr.vg_class)
    {
      case CLASS_FRONTS:
        cds_frnt(el, indx, &ier);
        break;

      case CLASS_WATCHES:
	cds_wbox(el, indx, &ier);
	break;

      case CLASS_SYMBOLS:
        cds_symb(el, indx, &ier);
	break;

      case CLASS_WINDS:
        cds_wind(el, indx, &ier);
	break;

      case CLASS_LINES:
	cds_line(el, indx, &ier);
	break;

      case CLASS_TEXT:
	cds_text(el, indx, &ier);
	break;

      case CLASS_CIRCLE:
	cds_circ(el, indx, &ier);
	break;

      case CLASS_TRACKS:
	cds_track (el, indx, &ier);
	break;

      case CLASS_SIGMETS:
	if (el->hdr.vg_type == SIGCCF_ELM) {
	    cds_ccf (el, indx, &ier);
	}
	else if (el->hdr.vg_type == ASHCLD_ELM) {
	    cds_ash (el, indx, &ier);
	}
	else if (el->hdr.vg_type == VOLC_ELM) {
	    cds_vol (el, indx, &ier);
	}
	else {
	    cds_sig (el, indx, &ier);
	}
	break;
      
      case CLASS_LIST:
	cds_list (el, indx, &ier);
	break;
      
      case CLASS_MET:
	if (el->hdr.vg_type == JET_ELM) {
	    cds_jet (el, indx, &ier);
	}
	else if (el->hdr.vg_type == GFA_ELM) {
	    cds_gfa (el, indx, &ier);
	}
        else if (el->hdr.vg_type == SGWX_ELM) {
	    cds_sgwx (el, indx, &ier);
	}        	
	else if (el->hdr.vg_type == TCA_ELM) {
	    cds_tca (el, &ier);
	}	
	else if (el->hdr.vg_type == TCERR_ELM) {
	    cds_tce (el, &ier);
	}	
	else if (el->hdr.vg_type == TCTRK_ELM) {
	    cds_tct (el, &ier);
	}	
	else if (el->hdr.vg_type == TCBKL_ELM) {
	    cds_tcb (el, &ier);
	}	
	break;
    }
 
}
