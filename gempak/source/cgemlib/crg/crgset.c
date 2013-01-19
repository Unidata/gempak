#include "geminc.h"
#include "gemprm.h"
#include "crgcmn.h"
#include "vgstruct.h"

void crg_set ( VG_DBStruct *el, int joffset, int layer, int *iret )
/************************************************************************
 * crg_set	 							*
 *									*
 * This function sets the range record for an element.			*
 *									*
 * void crg_set ( el, joffset, layer, iret )				*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	joffset		int		Offset to element in file	*
 *	layer		int		Element's layer value		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -3 = Range array is full	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 4/97	Added contour				*
 * D. Keiser/GSC	 4/97	Added special line			*
 * E. Safford/GSC	 4/97	Added text   				*
 * D. Keiser/GSC	 5/97	Added lines				*
 * E. Wehner/EAi	 5/97	Fixed contour, fixed lines		*
 * E. Safford/GSC        6/97	Added special text			*
 * E. Wehner/EAi	 8/97	Convert to crg_elrng and crg lib	*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * C.Lin/EAI	 	10/97	Add WBOX_ELM				*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_elrng.  Cleaned up.	*
 * C.Lin/EAI	 	04/98	Set group info				*
 * D.W.Plummer/NCEP	 4/98	Set vg_type				*
 * F.J.Yen/NCEP		 4/98	Added DARR_ELM and HASH_ELM		*
 * S. Law/GSC		05/98	Updated crg_styp call			*
 * A. Hardy/GSC         10/98   Added CMBSY_ELM                         *
 * A. Hardy/GSC         12/98   Added CIRCLE_ELM                        *
 * S. Law/GSC		05/99	Added TRKSTORM_ELM			*
 * G. Krueger/EAI	05/99	Corrected call to CRG_SETCIR		*
 * S. Law/GSC		08/99	Added SIGINTL_ELM			*
 * S. Law/GSC		08/99	Added remaining SIGMETs			*
 * D.W.Plummer/NCEP	 2/00	Added call to crg_setwbx for WBOX elem	*
 * S. Law/GSC		02/00	Added CCF				*
 * J. Wu/SAIC		12/01	set layer & call crg_mkRange		*
 * J. Wu/SAIC		07/04	set display filter			*
 * J. Wu/SAIC		07/07	set second range record			*
 ***********************************************************************/
{
    int		ier, elnum;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /* 
     * Determine if element already ranged. If so, reuse the slot. 
     */

    crg_getinx(joffset, &elnum, &ier);
    if (ier < 0)
    {
	/*
	 * Get location for the element in the range array
	 */

	crg_newinx( &elnum, &ier);
	if (ier < 0)
	{
	    *iret = -3;
	    return;
	}
    }
    
    crg_mkRange( el, joffset, elnum, &ier );
   
    crg_styp(elnum, el->hdr.vg_class, el->hdr.vg_type, &ier);

    crg_setLayer( elnum, layer, &ier );

    crg_sgrp(elnum, el->hdr.grptyp, el->hdr.grpnum, &ier);

    crg_setfilter ( elnum, el, &ier );
    
    
    /* 
     * If the second range record exists, make sure it has the same
     * class/type//layer/grptyp/grpnum/filter as the primary record's. 
     */
    if ( range[ elnum ].assocRec != NO_ASSOC_REC ) {
        crg_lkattr ( elnum, &ier );  
    }
    
}
