#include "ardcmn.h"

void aqdatt ( int *iunit, char *fname, int *lenf, int *itype, 
			float *xsz, float *ysz, int *ncurwn, int *iret )
/************************************************************************
 * aqdatt								*
 * 									*
 * This subroutine queries the device attributes.			*
 * 									*
 * aqdatt ( iunit, fname, lenf, itype, xsz, ysz, ncurwn, iret )		*
 *									*
 * Output parameters:							*
 *	*iunit		int	 	Output type			*
 *	*fname 		char		Name of file as output		*
 *	*lenf		int		Output file name length		*
 *	*itype		int		Output file format		*
 *	*xsz		float		X size in pixels		*
 *	*ysz		float		Y size in pixels		*
 * 	*ncurwn		int		Current window number		*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		9/98		Modified from UQDATT            *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	*iunit  = kunit;

	strcpy ( fname, filnam );
	*lenf   = strlen ( filnam );

	*itype  = kctype; 

	*xsz    = (float) kxsize;
	*ysz    = (float) kysize;

	*ncurwn = 0;

}
