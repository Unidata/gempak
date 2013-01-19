#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_setwbx ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setwbx                                                           *
 *                                                                      *
 * This function sets the range for a watch box element.		*
 *                                                                      *
 * crg_setwbx ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing watch box	*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 2/00	Created from crg_setsym			*
 * S. Law/GSC		03/00	changed to call crg_gbnd		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 * H. Zeng/XTRIA	02/03   added more extra space for large marker *
 * H. Zeng/XTRIA	10/04	added more extra space for large cnty	*
 ***********************************************************************/
{
    float	llx, lly, urx, ury, llx2, lly2, urx2, ury2, ccx, ccy;
    int 	ier, np;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Convert points in element to device
     */
    np = el->elem.wbx.info.numpts;
    crg_gbnd (sys_M, sys_D, np, 
	      &(el->elem.wbx.latlon[0]), &(el->elem.wbx.latlon[np]), 
	      &llx, &lly, &urx, &ury, &ccx, &ccy);

    np = el->elem.wbx.info.numcnty;
    if (np > 0) {
	crg_gbnd (sys_M, sys_D, np, 
		  &(el->elem.wbx.info.cn_ltln[0]),
		  &(el->elem.wbx.info.cn_ltln[np]),
		  &llx2, &lly2, &urx2, &ury2, &ccx, &ccy);

	if (llx2 < llx) llx = llx2;
	if (urx2 > urx) urx = urx2;
	if (lly2 < lly) lly = lly2;
	if (ury2 > ury) ury = ury2;
    }

    /* 
     *  adjust range points
     */
    llx -= (float)EXTRA * 15.0;
    urx += (float)EXTRA * 15.0;
    lly -= (float)EXTRA * 15.0;
    ury += (float)EXTRA * 15.0;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);

}
