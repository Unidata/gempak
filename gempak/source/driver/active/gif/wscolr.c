#include "gifcmn.h"

void wscolr ( int *Red, int *Green, int *Blue, int *iret )
/************************************************************************
 * wscolr								*
 *									*
 * This function sets the current color index for the gif image. There	*
 * will always be at least one color already in use, since black is	*
 * allocated when the image is opened. GEMPAK has only 32 colors	*
 * defined, and the GIF has 256, so there's no reason to worry about 	*
 * the color table being filled.					*
 *									*
 * wscolr ( Red, Green, Blue, iret )					*
 *									*
 * Input parameters:							*
 *  *Red		int	red index				*
 *  *Green		int	green index				*
 *  *Blue		int	blue index				*
 *									*
 * Output parameter:							*
 *  *iret		int	return value				*
 *									*
 **									*
 * Log:									*
 * D. Austin		 5/96						*
 * T. Lee/GSC	 	 7/00	Clean up; Renamed from gdr_CurrentColor	*
 ***********************************************************************/
{
	int status;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	/*
	 * Check if the requested color is already in use.
	 */

	if  ( (status = gdImageColorExact (Current_Im,*Red,*Green,*Blue) )
	    == -1) {
	    CurrentColorIndex = gdImageColorAllocate 
				( Current_Im, *Red, *Green, *Blue );
	}
	else {
	    CurrentColorIndex = status; 
	}
}
