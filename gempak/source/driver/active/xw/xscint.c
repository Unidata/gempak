#include "xwcmn.h"
#include "color.h"

void xscint ( int *iret )
/************************************************************************
 * xscint								*
 *									*
 * This subroutine initializes the graphic colors. 			*
 *									*
 * xscint ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *              G_NORMAL    = normal return.                            *
 **									*
 * Log:									*
 * C. Lin/EAI		 2/96						*
 * S. Wang/GSC		11/97	fix bug in parm of xscrgb() add clr_fil	*
 * T. Piper/SAIC	07/04	general clean-up			*
 ***********************************************************************/
{
int	ii, ncolors, cbank;
XColor	xcolors[MAXCOLORS];
int	ired, igreen, iblue;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	cbank = GraphCid;

	if (GColorIsInitialized == SHARECOLOR) {

	    /*
	     * if share color exists, get the color components,
	     * and set the graphic color table structure.
	     */
	    ncolors = ColorBanks.banks[cbank];
	    ngemC = ncolors;
	    for ( ii = 0; ii < ncolors; ii++) {
	 	xcolors[ii].pixel = ColorBanks.colrs[cbank][ii];	
		xcolors[ii].flags = DoRed | DoGreen | DoBlue;
	    }
	    XQueryColors(gemdisplay, gemmap, xcolors, ncolors); 

	    for ( ii = 0; ii < ncolors; ii++) {
		ired =  xcolors[ii].red / COLR_SCAL;
		igreen = xcolors[ii].green / COLR_SCAL;
		iblue = xcolors[ii].blue / COLR_SCAL;

	        cscrgb(&ii, &ired, &igreen, &iblue, iret);
	    }

	}
	else {

	    /*
	     * if no share color, read table and set the color.
	     */
	    csctbl('\0', iret);
	    if (*iret != G_NORMAL) return;
	    if (ngemC > ColorBanks.banks[cbank]) 
		ncolors = ColorBanks.banks[cbank];
	    else
		ncolors = ngemC;

	    for (ii = 0; ii < ncolors; ii++) {

		ired = gemColrs[ii].red;
		igreen = gemColrs[ii].green;
		iblue = gemColrs[ii].blue;
		xscrgb(&cbank, &ii, &ired, &igreen, &iblue, iret);

		if ( *iret != G_NORMAL ) return;
			
	    }  

	}
		
}

