#include "xwcmn.h"
#include "color.h"

void xcaloc ( int cbank, int *iret )
/************************************************************************
 * xcaloc								*
 *									*
 * This subroutine allocates the read/write color cells for the 	*
 * X Window device driver.						*
 *									*
 * void xcaloc ( cbank, iret )						*
 *									*
 * Input parameters:							*
 *	cbank		int	color bank id				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *              G_NORMAL    = normal return.                            *
 *									*
 *              G_NCLRAL    = color allocation failure                  *
 **									*
 * Log:									*
 * C. Lin/EAI		 1/94						*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * D. Himes/COMET	 8/94	Don't exit on failure, return error code*
 * C. Lin/EAI		 1/95	Add radar color type			*
 * C. Lin/EAI		 3/95	use ColorBanks structure		*
 * C. Lin/EAI		 8/95	add G_ZEROCB				*
 * A. Person/Penn State	06/02	Updated to support 16&24-bit graphics	*
 * T. Piper/SAIC	07/04	Moved 'for' loop into 8-bit depth case	*
 * T. Piper/SAIC	02/05	Added setting of allocflag		*
 * S. Jacobs/NCEP        3/05   Added 32-bit for PC X servers           *
 * T. Piper/SAIC	09/07	Set *iret for SHARECOLOR test		*
 ***********************************************************************/
{
int	ii, err, xdpth;
Pixel	pxls[MAXCOLORS];

/*---------------------------------------------------------------------*/
/*
 * check whether GColorIsInitialized is set
 */
    if ( !GColorIsInitialized ) {
        xgbank(gemdisplay, &err);
    }
/*
 * if share color exists, return.
 */
    if (GColorIsInitialized == SHARECOLOR) {
	allocflag[cbank] = G_TRUE;
	*iret = G_NORMAL;
	return;
    }

/*
 * check whether the color bank id is valid
 */
    if ( (cbank >= ColorBanks.nbank) || (cbank < 0 ) ) {
	*iret = G_NICBANK;
	return;
    }

/*
 * check whether there are any colors in the bank 
 */
    if (ColorBanks.banks[cbank] == 0) {
	*iret = G_ZEROCB;
	return;
    }

/*
 * allocate read/write color cells if 8-bit, otherwise wait
 * for ColorBanks.colrs to be defined later in xscrgb
 * (16-bit and 24-bit).
 */
    xdpth = DefaultDepth ( (XtPointer)gemdisplay,
		DefaultScreen((XtPointer)gemdisplay) );
    if( xdpth == 8 ) {
        *iret = xgrwc ( gemdisplay, gemmap, pxls, 
				ColorBanks.banks[cbank]);
	if  ( *iret == G_NORMAL ) {
	    for ( ii = 0; ii < ColorBanks.banks[cbank]; ii++) {
		ColorBanks.colrs[cbank][ii] = pxls[ii];
	    }
	    allocflag[cbank] = G_TRUE;
	}
    }
    else if ( xdpth == 16 || xdpth == 24 || xdpth == 32 ) {
	allocflag[cbank] = G_TRUE;
	*iret = G_NORMAL;
    }
    else {
	*iret = G_NCLRAL;  /* Disallow other depths */
    }
}

/*=====================================================================*/

int xgrwc ( Display *dpy, Colormap cmap, Pixel pxls[], int ncolors )
/************************************************************************
 * xgrwc                                                   		*
 *                                                                      *
 * This function allocates non-shared block of read/write color cells   *
 *  from the color map.  						*
 *                                                                      *
 * int xgrwc ( dpy, cmap, pxls, ncolors)                    		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dpy        Display        	specifies a connection to an X server.  *
 *  cmap        Colormap       	specifies the ID of the colormap. 	*
 *  pxls[]      Pixel		return array of color pixel index.   	*
 *  ncolors     int	       	number of colors required. 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *  xgrwc	int            	Return code          		        *
 *				G_NORMAL = successful.                  *
 *      			G_NCLRAL = color allocation failure    	*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       02/95  use error code G_NCLRAL                      *
 ***********************************************************************/
{
    if ( XAllocColorCells(dpy, cmap, False, NULL, 0, pxls, ncolors) ) {
	return(G_NORMAL);
    }
    else {
	return(G_NCLRAL);
    }
}
