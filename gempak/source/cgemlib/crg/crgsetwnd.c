#include "crgcmn.h"
#include "vgstruct.h"

#define DARR_BASE_SPD	10.0F


void crg_setwnd ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setwnd                                                           *
 *                                                                      *
 * This function sets the range for a wind element.			*
 *                                                                      *
 * crg_setwnd ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing wind		*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97   Created					*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * F.J.Yen/NCEP		 1/98	Rewrote crg_wndrng.  Cleaned up.	*
 * F.J.Yen/NCEP		 4/98	Increased range.			*
 * F.J.Yen/NCEP		 4/98	Added DARR_ELM and HASH_ELM.		*
 * F.J.Yen/NCEP		 4/98	Used base speed of 10 for DARR_ELM.	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Law/GSC		03/99	Cleaned up range sizes			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 ***********************************************************************/
{
    float	rx[1], ry[1];
    float	srx[1], sry[1];
    float	llx, lly, urx, ury;
    float	mysize;
    int 	ier, np;
    float	szmk, sztx, szwb, szws, szab, szah;
    float	sizwba, adjhsiz, speed;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Convert point in element to device...
     */

    np = 1;
    rx[0] = el->elem.wnd.data.latlon[0];
    ry[0] = el->elem.wnd.data.latlon[np];

    gtrans(sys_M, sys_D, &np, rx, ry, srx, sry, &ier, 
	   strlen(sys_M), strlen(sys_D));

    /*
     *  Get wind barb and wind arrow sizes...
     */

    gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, &ier );

    /*
     *  calculate the size (mysize) of this barb in device coords x and y.
     */
    if (el->hdr.vg_type == BARB_ELM) {
        mysize = (el->elem.wnd.info.size * szwb) + (float)EXTRA;
    }
    else if (el->hdr.vg_type == ARROW_ELM || el->hdr.vg_type == DARR_ELM) {
	if (el->hdr.vg_type == DARR_ELM) {
	    speed = DARR_BASE_SPD;
	}
	else if (el->elem.wnd.data.spddir[0] < 1.0F) {
	    speed = 1.0F;
	}
	else {
	    speed = el->elem.wnd.data.spddir[0];
	}

	sizwba = (speed * el->elem.wnd.info.size * szab);
	adjhsiz = el->elem.wnd.info.hdsiz * szah * 0.85F;

	mysize = sizwba + adjhsiz + (float)EXTRA;
    }
    else {
        mysize = el->elem.wnd.info.size * szwb + (float)EXTRA;
    }

    /* 
     * set range points based on the dimension of the element 
     */

    lly = sry[0] - mysize;
    llx = srx[0] - mysize;
    urx = srx[0] + mysize;
    ury = sry[0] + mysize;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);
}
