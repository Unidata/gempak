#include "gifcmn.h"

void wclear ( int *iret )
/************************************************************************
 * wclear								*
 *									*
 * This function clears the canvas of the current gif image.		*
 *									*
 * wclear ( iret )							*
 * 									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * Dan Austin		 5/96						*
 * T. Lee/GSC		 7/00	Cleaned up				*
 ***********************************************************************/
{
	*iret = G_NORMAL;
	/* 
 	 * Fill the entire image with the background color.
 	 */

	gdImageFilledRectangle( Current_Im,
				0,
				0,	
				Current_Im->sx,
				Current_Im->sy,
				CurrentBGColorIndex);
}
