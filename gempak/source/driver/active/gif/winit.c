#define GDR_GLOBAL

#include "gifcmn.h"
#include "pattern.h"

void winit ( int *iright, int *ibot, int *iret )
/************************************************************************
 * winit								*
 *									*
 * This function initializes routines for GIF driver.  It is called	*
 * by HOPEN ().								*
 *									*
 * winit ( iright, ibot, iret )						*
 *									*
 * Input parameters:							*
 *  *iright		int	x-value					*
 *  *ibot		int	y-value					*
 *									*
 * Output parameters:                                                   *
 *  *iret               int     Return code                             *
 *									*
 **                                                                     *
 * Log:									*
 * Dan Austin		 5/96						*
 * T. Lee/GSC		 7/00   Cleaned up				*
 * S. Danz/AWC		11/03	Added include of pattern.h              *
 *                              changed xsize/ysize to int from int*    *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	/* 
	 * Create the initial structure.
	 */
	if ( (Current = (gdr_image *)malloc(sizeof(gdr_image))) == NULL )
	{
	    printf( "WINIT: malloc failed for gdr_image\n" );
	    exit(-1);
	}

	/*
	 * Fill in as much as we can.
	 */
	Current -> xsize = *iright;
	Current -> ysize = *ibot;

	/*
	 * Open the gd image file.
	 */
	if ( (Current_Im = gdImageCreate (Current->xsize,
					  Current->ysize) ) == NULL)
	{
	    printf(" gdImageCreate returned NULL\n"); 
	    exit(-1);
	}

	CurrentBGColorIndex = 999;

	/*
	 * Set return flag.
	 */

	*iret = G_NORMAL;
}
