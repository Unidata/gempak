#include "tiffcmn.h"

void twrpxl ( int bnum, int lnum, int *iret )
/************************************************************************
 * twrpxl								*
 *									*
 * This routine writes a pixel into a raster plane at a specified	*
 * location.								*
 *									*
 * twrpxl ( bnum, lnum, iret )						*
 *									*
 * Input parameters:							*
 *	bnum		int		Bit (x value)			*
 *	lnum		int		Scan line (y value)		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 * S. Jacobs/NCEP	 9/00	Added uncompressed data			*
 ***********************************************************************/
{

	int		jbit, jlin, ix, ib, ioff;

/*---------------------------------------------------------------------*/

	*iret =  G_NORMAL;

/*
 *	Apply the counter-clockwise rotation to the raster image.
 */
	if  ( krot == 90 )  {
	    jbit = lnum;
	    jlin = klin - bnum + 1;
	}
	else if  ( krot == 270 )  {
	    jbit = kbit - lnum + 1;
	    jlin = bnum;
	}
	else if  ( krot == 180 )  {
	    jbit = kbit - bnum + 1;
	    jlin = klin - lnum + 1;
	}
	else {
	    jbit = bnum;
	    jlin = lnum;
	}

/*
 *	If the data is compressed, set the appropriate bit.
 */
 	if  ( ktype == 0 )  {

/*
 *	    Calculate which bit to set.
 */
	    jbit--;

	    ix = jbit / 8;
	    ib = jbit % 8;

/*
 *	    Check if the bit is in range.
 */
	    if  ( (jbit < kbit) && (jlin < klin) )  {

		ioff = ((jlin-1)*(kbit/8)) + ix;

		if  ( pixval )  {

/*
 *	    	   If this bit should be turned on, then OR the byte
 *	    	   with MSKON.
 */
		    *(rasimg + ioff) |= mskon[ib];

		}
		else {

/*
 *		   Otherwise if the bit should be turned off, then AND
 *		   the byte with MSKOFF.
 */
		    *(rasimg + ioff) &= mskoff[ib];

		}

	    }
	}
	else {
/*
 *	    If the data is not compressed set the appropriate byte.
 */
 	    if  ( pixval )  {
	    	rasimg[(lnum*kbit)+bnum] = 255;
	    }
	    else {
	    	rasimg[(lnum*kbit)+bnum] = 0;
	    }

	}

}
