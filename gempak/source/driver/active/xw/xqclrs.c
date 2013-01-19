#include "xwcmn.h"
#include "color.h"

void xqclrs ( int *cbank, int *ncolors, Pixel *colors, int *iret ) 
/************************************************************************
 * xqclrs                                          			*
 *                                                                      *
 *  This function queries the color structure in the XW driver.  It	*
 *  returns the number of colors and the color index array.    		*
 *                                                                      *
 * void xqclrs ( cbank, ncolors, colors, iret )    			*
 *                                                                      *
 * Input parameters:                                                    *
 *  *cbank            int          color bank ID 			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *ncolors          int          number of colors 			*
 *  *colors           Pixel	   color index array 			*
 *                                                                      *
 * Return parameters:                                                   *
 *  *iret             int          Return Code  			*
 *              		   G_NORMAL  = normal return 		*
 *				   G_NCBALOC = bank not allocated	*
 *				   G_NICBANK = invalid color bank ID    *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/94                                           *
 * J. Cowie/COMET	11/95	Made cbank a pointer, use return codes	*
 * C. Lin/EAI		12/95	clrsalloc -> allocflag[cbank]		*
 * S. Jacobs/NCEP	 1/97	Added call to xaiclr to alloc clr banks	*
 * S. Jacobs/NCEP	 8/98	Changed colors from long int to int	*
 * R. Tian/SAIC		05/02	Added *cbank case 3			*
 * T. Piper/SAIC	01/04	Changed colors type to Pixel		*
 * T. Piper/SAIC	01/04	Corrected switch cases of cbank		*
 ***********************************************************************/
{

int	ii, numc, ier;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Exit if color bank number invalid
 */		
	if ( (*cbank >= ColorBanks.nbank ) || 
		( *cbank < GraphCid ) ) {
	    *iret = G_NICBANK;
	    return;
	}

/*
 *	Allocate the colors for cbank.  Send a default value for
 *	the case where the colors will not be shared.
 */
	switch ( *cbank ) {
	    case GraphCid:
		numc = GRAPH_COLORS;
		break;
	    case SatCid:
		numc = SAT_COLORS;
		break;
	    case RadCid:
		numc = RAD_COLORS;
		break;
	    case FaxCid:
		numc = FAX_COLORS;
		break;
	    default:
		numc = GRAPH_COLORS;
		break;
	}
	xaiclr ( cbank, &numc, &ier );

/*
 *	Check that the color bank has already been allocated
 */
	if ( (*cbank == GraphCid && !GColorIsInitialized ) ||
	     (*cbank >  GraphCid && !allocflag[*cbank]) ) {
	    *iret = G_NCBALOC;
	    return;
	}
	
	*ncolors = ColorBanks.banks[*cbank];

	for ( ii = 0; ii < ColorBanks.banks[*cbank]; ii++ ) 
		colors[ii] = ColorBanks.colrs[*cbank][ii];

}

