#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"


void crg_setlist ( VG_DBStruct *el,  int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setlist	                                                        *
 *                                                                      *
 * This function sets the range record for a list element.		*
 *                                                                      *
 * crg_setlist ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing list		*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * J. Wu/SAIC           09/02   initial coding				*
 * J. Wu/SAIC           12/02   double the size of the refreshing area	*
 ***********************************************************************/
{
    int         ier;
    float	llx, lly, urx, ury, ccx, ccy, extra;
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     * Get bounds.
     */
    crg_gbnd ( sys_M, sys_D, el->elem.lst.data.nitems,
               &el->elem.lst.data.lat[0], &el->elem.lst.data.lon[0],
               &llx, &lly, &urx, &ury, &ccx, &ccy );

    extra = (float) (2.0 * EXTRA_SM);  /* EXTRA_SM = 30 */
    llx -= extra;
    urx += extra;
    ury += extra;
    lly -= extra;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save ( elnum, joffset, llx, lly, urx, ury, &ier );

}
