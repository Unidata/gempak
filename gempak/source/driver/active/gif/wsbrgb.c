#include "gifcmn.h"

void wsbrgb ( int *icbank, int *index, int *Red, int *Green, int *Blue, int *iret )
/************************************************************************
 * wsbrgb								*
 *									*
 * This function sets the LUT color table for the gif image.		*
 * There will always be at least one color already in use,		* 
 * since black is allocated when the image is opened. GEMPAK 		*
 * has only 32 colors defined, and our GIF has 256, so there's 		*
 * no reason to worry about the color table being filled.		*
 *									*
 * wsbrgb ( icbank, index, Red, Green, Blue, iret )			*
 *									*
 * Input parameters:							*
 *  *icbank		int	color bank index			*
 *  *index		int	color index				*
 *  *Red		int	red index				*
 *  *Green		int	green index				*
 *  *Blue		int	blue index				*
 *									*
 * Ouput parameters:							*
 *  *iret		int	return value				*
 *									*
 **Log:									*
 * J. Nielsen-G/TAMU	12/96						*
 * T. Lee/GSC		 7/00	Renamed from gdr_LUTColor		*
 ***********************************************************************/
{
	int status;
/*-----------------------------------------------------------------------*/

	*iret = G_NORMAL;

	/*
	 * Make sure that the index is within acceptable bounds.
	 */
	if ( ( *index < 0 ) || ( *index >= MXCOL ) ) return;

	/*
	 * Check to see if the requested color is already in use.
	 */
	if ( (status =  gdImageColorExact
			(Current_Im, *Red, *Green, *Blue) ) == -1 ) {
	    LUT [*index] = gdImageColorAllocate
			(Current_Im,*Red,*Green,*Blue);
	}
	else {
	    LUT [*index] = status; 

	}

	if ( *icbank >= 1 ) {
	    LUT [ *index ] = CurrentColorIndex;
	}
}
