#include "xwcmn.h"
#include "color.h"

void xqnclr ( int *cbank, int *ncolors, int *iret ) 
/************************************************************************
 * xqnclr                                          			*
 *                                                                      *
 * This function query the color structure in XW driver. It ruturns     *
 *  the number of colors.						*
 *                                                                      *
 * void xqnclr(cbank, ncolors, iret )    				*
 *                                                                      *
 * Input parameters:                                                    *
 *  cbank            int*          color bank ID 			*
 *                                                                      *
 * Output parameters:                                                   *
 *  ncolors         int*           number of colors 			*
 *                                                                      *
 * Return parameters:                                                   *
 *  iret            int*           Return Code  			*
 *              		   G_NORMAL  = normal return 		*
 *				   G_NCBALOC = bank not allocated	*
 *				   G_NICBANK = invalid color bank ID    *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 7/97	From XQCLRS				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Exit if color bank number invalid
 */		
	if ( (*cbank >= ColorBanks.nbank ) || 
		( *cbank < 0 ) ) {
	    *iret = G_NICBANK;
	    return;
	}

/*
 *	Check that color bank has already been allocated
 */
	if ( (*cbank == GraphCid && !GColorIsInitialized ) ||
	     (*cbank >  GraphCid && !allocflag[*cbank]) ) {
	    *iret = G_NCBALOC;
	    return;
	}
	
	*ncolors = ColorBanks.banks[*cbank];

}

