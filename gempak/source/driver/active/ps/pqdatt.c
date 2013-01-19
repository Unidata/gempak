#include "pscmn.h"
#include "color.h"

void pqdatt ( int *iunit, char *fname, int *lenf, int *itype, 
			float *xsz, float *ysz, int *ncurwn, int *iret )
/************************************************************************
 * pqdatt								*
 * 									*
 * This subroutine queries the device attributes.			*
 * 									*
 * pqdatt ( iunit, fname, lenf, itype, xsz, ysz, ncurwn, iret )		*
 *									*
 * Output parameters:							*
 *	*iunit		int		Output type (Used for XW only)  *
 *	*fname		char		Name of file as output		*
 *	*lenf		int		Output file name length		*
 *	*itype		int		Device type (color,bw,gs)	*
 *	*xsz		float		X size in inches or pixels	*
 *	*ysz		float		Y size in inches or pixels	*
 * 	*ncurwn		int		Current window number		*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/96	Copied from PSDATT			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	*iunit  = kunit;

	strcpy ( fname, filnam );
	*lenf   = strlen ( filnam );

	*itype  = kctype;

	*xsz    = xsize;
	*ysz    = ysize;

	*ncurwn = 0;

}
