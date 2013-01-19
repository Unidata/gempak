#include	"nccmn.h"

void mqdatt ( int *iunit, char *filnam, int *lenf, int *itype, 
		float *xsize, float *ysize, int *ncurwn, int *iret )
/************************************************************************
 * mqdatt								*
 *									*
 * This subroutine queries the device attributes.			*
 *									*
 * mqdatt  ( iunit, filnam, lenf, itype, xsize, ysize, ncurwn, iret )	*
 *									*
 * Output parameters:							*
 *	*iunit		int		Output type (Used for XW only)  *
 *	*filnam		char		Output metafile name		*
 *	*lenf		int		Length of file name		*
 *	*itype		int		Device color type		*
 *	*xsize		float		X size in pixels		*
 *	*ysize		float		Y size in pixels		*
 *	*ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/96	Copied from MSDATT			*
 * R. Tian/SAIC          4/02	Added quary of xsize and ysize		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	*iunit  = kunit;

	strcpy ( filnam, curfil );
	*lenf   = strlen ( curfil );

	*itype  = kctype;

	*xsize  = fxsize / (float)XY_SCALE;
	*ysize  = fysize / (float)XY_SCALE;

	*ncurwn = 0;

}
