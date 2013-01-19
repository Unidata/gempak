#include "faxcmn.h"

void rwrpxl ( int bnum, int lnum, int *iret )
/************************************************************************
 * rwrpxl								*
 *									*
 * This routine writes a pixel into a raster plane at a specified	*
 * location.								*
 *									*
 * rwrpxl ( bnum, lnum, iret )						*
 *									*
 * Input parameters:							*
 *	bnum		int		Bit (x value)			*
 *	lnum		int		Scan line (y value)		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/EAi	12/96	Remove POW calls			*
 * E. Wehner/EAi	 3/97	Provide rotation to pixels when needed	*
 * S. Jacobs/NCEP	 7/97	Rewrote and simplified bit masking	*
 * S. Jacobs/NCEP	 7/97	Added rotation for 180 and 270		*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 ***********************************************************************/
{

	int		jbit, jlin, ix, ib, ioff;

/*---------------------------------------------------------------------*/

	*iret =  G_NORMAL;

/*
 *	Apply the counter-clockwise rotation to the raster image.
 */
	if  ( krot[0] == 90 )  {
	    jbit = lnum;
	    jlin = klin[0] - bnum + 1;
	}
	else if  ( krot[0] == 270 )  {
	    jbit = kbit[0] - lnum + 1;
	    jlin = bnum;
	}
	else if  ( krot[0] == 180 )  {
	    jbit = kbit[0] - bnum + 1;
	    jlin = klin[0] - lnum + 1;
	}
	else {
	    jbit = bnum;
	    jlin = lnum;
	}

/*
 *	Calculate which bit to set.
 */
	ix = jbit / 8;
	ib = jbit % 8;

/*
 *	Check if the bit is in range.
 */
	if  ( (jbit < kbit[0]) && (jlin < klin[0]) )  {

	    ioff = ((jlin-1)*(kbit[0]/8)) + ix;

	    if  ( pixval )  {

/*
 *	    	If this bit should be turned on, then OR the byte
 *	    	with MSKON.
 */
		if  ( faxflg )  {
		    *(rasimg + ioff) |= mskon[ib];
		}
		else {
		    *(rasimg + ioff) |= mskonr[ib];
		}
	    }
	    else {

/*
 *		Otherwise if the bit should be turned off, then AND
 *		the byte with MSKOFF.
 */
		if  ( faxflg )  {
		    *(rasimg + ioff) &= mskoff[ib];
		}
		else {
		    *(rasimg + ioff) &= mskoffr[ib];
		}
	    }

	}

}
