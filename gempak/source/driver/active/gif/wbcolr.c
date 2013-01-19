#include "gifcmn.h"

/*
 * definition of background color
 */
#define ZERO	0

void wbcolr ( int *Red, int *Green, int *Blue , int *iret )
/************************************************************************
 * wbcolr								*
 *									*
 * This function sets the current background (BG) color. This is the	*
 * first color allocated by GD for the image and is always index 0 in	*
 * the GD color table.  To change, you must allocate the color first,	*
 * then immediately reallocate.						*
 *									*
 * wbcolr ( Red, Green, Blue, iret )					*
 *									*
 * Input and out put parameters:					*
 *  *Red		int	red index				*
 *  *Green		int	green index				*
 *  *Blue		int	blue index				*
 *									*
 * Output parameters:                                                   *
 *  *iret		int	Return code				*
 *									*
 **									*
 * Log:                                                                 *
 * Dan Austin		 6/96						*
 * T. Lee/GSC		 7/00	Renamed from gdr_BGColor.c		*
 ***********************************************************************/
{

	*iret = G_NORMAL;
	/*
	 * Test for special case where the background color is to
	 * become the current color.
	 */

	if ( *Red == -1 ) /* Set in hscolr(). clumsy but it works.*/
	{
		CurrentColorIndex = CurrentBGColorIndex;
		return;
	}

	/* 
	 * Allocate the background color The index of the BG is zero.
	 * If the BG color has been set, then reset it.
	 */
	if (CurrentBGColorIndex != 999)
		gdImageColorDeallocate (Current_Im, ZERO);
	
        CurrentBGColorIndex = gdImageColorAllocate
			      (Current_Im, *Red, *Green, *Blue);

        if (CurrentBGColorIndex != 0)
        {
                printf (" Unable to allocate BG color\n");
                exit(-1);
        }

}
