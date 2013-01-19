#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_setvol ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setvol                                                           *
 *                                                                      *
 * This function sets the range for a volcano element.			*
 *                                                                      *
 * crg_setvol ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing volcano	*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	07/03	copied from crgsetsym.c			*
 ***********************************************************************/
{
    float       rx[1], ry[1];
    float	srx[1], sry[1];
    float	llx, lly, urx, ury;
    int 	ier, np;
    float       szmk, sztx, szwb, szws, szab, szah;
    float	szwsx; 
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Convert point in element to device...
     */

    np = 1;
    rx[0] = el->elem.vol.latlon[0];
    ry[0] = el->elem.vol.latlon[np];

    gtrans(sys_M, sys_D, &np,
			rx, ry, srx, sry, &ier,
                        strlen(sys_M), strlen(sys_D));
 
    /*
     *  Get symbol size
     */

    gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, &ier );

    /*
     *  To get actual pixel size, multiply szws by size multiplier
     */

    szwsx = szws * el->elem.vol.info.size + (float)EXTRA;

    /* 
     *  set range points based on the dimension of the element 
     */

    lly = sry[0] - szwsx;
    llx = srx[0] - szwsx;
    urx = srx[0] + szwsx;
    ury = sry[0] + szwsx;

    /*
     *  Store the device coordinates in the range array.
     */

    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);

}
