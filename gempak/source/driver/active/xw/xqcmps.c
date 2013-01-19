#include "xwcmn.h"
#include "color.h"

void xqcmps ( int *icbank, int *ncolr, int *ired, int *igreen, 
						int *iblue, int *iret )
/************************************************************************
 * xqcmps								*
 *									*
 * xqcmps ( icbank, ncolr, ired, igreen, iblue, iret )			*
 *									*
 * This subroutine returns RGB values for the specified color bank. 	*
 *									*
 * Input parameters:							*
 *	*icbank		int		Color bank ID			*
 *									*
 * Output parameters:							*
 *	*ncolr		int		number of colors in the bank	*
 *	*ired		int		Red color component		*
 *	*igreen		int		Green color component		*
 *	*iblue		int		Blue color component		*
 *	*iret		int		Return code			*
 *				      0 = successful 			*
 *				     -1 = bank ID out of range 		*
 *				     -2 = color bank is not allocated   *
 **									*
 * Log:									*
 * C. Lin/EAI	 	5/96						*
 * T. Piper/GSC		3/01	Fixed IRIX6 compiler warnings		*
 * S. Chiswell		8/02	Changed pixel range check for 8 bit	*
 * R. Tian/SAIC		3/03	Added check for color bank		*
 * T. Piper/SAIC	07/04	Minor clean-up				*
 ***********************************************************************/
{
int     ii, ncolors, cbank;
int	xdpth;
XColor  xcolors[MAXCOLORS];

/*---------------------------------------------------------------------*/

	*iret = 0;

	cbank = *icbank;

	if (cbank > ColorBanks.nbank - 1) {
		*iret  = -1;
		return;
	}

	xdpth = DefaultDepth ( (XtPointer)gemdisplay,
				DefaultScreen((XtPointer)gemdisplay) );

	ncolors = ColorBanks.banks[cbank];
	if ( ncolors <= 0 || ncolors > MAXCOLORS ) {
	    *iret = -2;
	    return;
	}

/*
 *      Check that the color bank has already been allocated
 */
        if ( (*icbank == GraphCid && !GColorIsInitialized ) ||
             (*icbank >  GraphCid && !allocflag[*icbank]) ) {
            *iret = G_NCBALOC;
            return;
        }

        for ( ii = 0; ii < ncolors; ii++) {
            xcolors[ii].pixel = ColorBanks.colrs[cbank][ii];
	    if ( ( xdpth == 8 ) && ( xcolors[ii].pixel > (Pixel)255 ) ) {
		    *iret = -2;
		    return;
	    }
            xcolors[ii].flags = DoRed | DoGreen | DoBlue;
        }
        XQueryColors(gemdisplay, gemmap, xcolors, ncolors);

        for ( ii = 0; ii < ncolors; ii++) {
            ired[ii]   = xcolors[ii].red / COLR_SCAL;
            igreen[ii] = xcolors[ii].green / COLR_SCAL;
            iblue[ii]  = xcolors[ii].blue / COLR_SCAL;
        }


	*ncolr = ncolors;

}
