#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"


void crg_setltln ( int elnum, int joffset, int np, float lat[], 
						float lon[], int *iret )
/************************************************************************
 * crg_setltln                                                          *
 *                                                                      *
 * This function sets the range for an element from lat/lon arrays.	*
 *                                                                      *
 * crg_setltln ( elnum, joffset, np, lat, lon, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 * 	joffset		int		File position of the element	*
 *	np		int		Number of lat/lon points	*
 *	lat[]		float		Latitude coordinates		*
 *	lon[]		float		Longitude coordinates		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * F.J.Yen/NCEP		 1/98	Created based on cvg_splrng.		*
 * I. Durham/GSC	 5/98	Changed underscore delc. to an include	*
 * S. Law/GSC		03/00	changed to call crg_gbnd		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 ***********************************************************************/
{
    int         ier;
    float	llx, lly, urx, ury, ccx, ccy;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * get bounds
     */
    crg_gbnd (sys_M, sys_D, np, lat, lon, &llx, &lly, &urx, &ury, &ccx, &ccy);

    llx -= (float)EXTRA_SM;
    urx += (float)EXTRA_SM;
    ury += (float)EXTRA_SM;
    lly -= (float)EXTRA_SM;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);

}
