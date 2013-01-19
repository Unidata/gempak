#include "xwcmn.h"
#include "color.h"

void xaiclr ( int *cbank, int *numc, int *iret )
/************************************************************************
 * xaiclr								*
 *									*
 * This routine allocates the color cells for the image color banks.	*
 *									*
 * xaiclr ( cbank, numc, iret )						*
 *									*
 * Input parameters:							*
 *	*cbank		int		Color bank ID			*
 *	*numc		int		Default number of colors	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/97						*
 * S. Jacobs/NCEP	 2/97	Added init of colors to gray		*
 * C. Lin/EAI	         2/98	Added check of graphics colors		*
 * T. Piper/SAIC	02/05	Moved allocflag; set in xcaloc		*
 ***********************************************************************/
{

int	ii, ier, ic, jcolr, ired, igreen, iblue, ierr;
Pixel	pxls[MAXCOLORS];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	if ( *cbank == GraphCid )
		return;

/*
 *	Allocate the satelllite or radar colors, if needed.
 */
	if  ( ! allocflag[*cbank] )  {

	    xcaloc ( *cbank, &ier );
	    if  ( ier == G_ZEROCB )  {

/*
 *  WARNING:  This section has me a bit concerned for it does NOT
 *            have the check for xdpth like xcaloc does!?!
 */

/*
 *		IF (ncolors = 0) allocate default non-shareble colors.
 */
		ColorBanks.banks[*cbank] = *numc;
		ier = xgrwc ( gemdisplay, gemmap, pxls,
			    ColorBanks.banks[*cbank]);

		if  ( ier == G_NORMAL )  {
/*
 *		    Allocate the memory for each color bank.
 */
		    ColorBanks.colrs[*cbank] = (Pixel *)
			       malloc(ColorBanks.banks[*cbank] * 
					    sizeof(Pixel));

		    for ( ii = 0; ii < ColorBanks.banks[*cbank]; ii++) {
			ColorBanks.colrs[*cbank][ii] = pxls[ii];
		    }
		    allocflag[*cbank] = G_TRUE;
		}
	    }

	    if  ( ier == G_NCLRAL )  {
		*iret = ier;
		return;
	    }
/*
 *	    Initialize the colors of the color bank to a set of grays.
 */
	    for ( ic = 0; ic < ColorBanks.banks[*cbank]; ic++ )  {
		jcolr  = ic;
		ired   = G_NINT ( ( (float)ic /
				    ColorBanks.banks[*cbank]-1 ) * 255 );
		igreen = ired;
		iblue  = ired;
		xscrgb ( cbank, &jcolr, &ired, &igreen, &iblue, &ierr );
	    }
	}
}
