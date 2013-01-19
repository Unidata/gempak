#include "crgcmn.h"
#include "vgstruct.h"

void crg_settxt ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_settxt                                                           *
 *                                                                      *
 * This function sets the range for a text element and a special text	*
 * element.								*
 *                                                                      *
 * crg_settxt ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing text		*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97   Created					*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Safford/GSC	10/97	Fixed range problem on non-centered txt *
 * E. Safford/GSC	12/97	Modified for new ialign values		*
 * F.J.Yen/NCEP          1/98	Combined crg_txtrng and crg_sptrng.	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 6/98	Rewrote to make the range area smaller	*
 * S. Law/GSC		07/98	Moved most of this to crg_gettxtbox	*
 ***********************************************************************/
{
    float	llx, lly, urx, ury;
    float	sx[4], sy[4];
/*---------------------------------------------------------------------*/

    crg_gettxtbox (el, 1, sx, sy);

    llx = sx[0];
    lly = sy[0];
    urx = sx[2];
    ury = sy[2];

    /*
     * Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, iret);

}
