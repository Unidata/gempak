#include "geminc.h"
#include "gemprm.h"

extern unsigned char *imdata;
unsigned char *imndat;

void im_sbgn ( int *kx, int *ky, int *ixlef, int *iytop, int *ixrit, 
					int *iybot, int *ipix, int *iret )
/************************************************************************
 * im_sbgn								*
 *									*
 * This subroutine subsets a GINI image by area and pixel resolution.	*
 *									*
 * im_sbgn ( kx, ky, ixlef, iytop, ixrit, iybot, ipix, iret )		*
 *									*
 * Input parameters:							*
 *	*kx		int		X size of original image	*
 *	*ky		int		Y size of original image	*
 *	*ixlef		int		Left bound in image coords	*
 *	*iytop		int		Top bound in image coords	*
 *	*ixrit		int		Right bound in image coords	*
 *	*iybot		int		Bottom bound in image coords	*
 *	*ipix		int		Pixel resolution factor		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/96						*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		11/98	Updated prolog				*
 ***********************************************************************/
{

	int		nx, ny, lendat, i, j, ind1, ind2;
	int		ix, iy;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL ;

/*
 *	Allocate space for the output image.
 */
	nx = ( *ixrit - *ixlef + 1 ) / *ipix;
	ny = ( *iybot - *iytop + 1 ) / *ipix;
	lendat = nx * ny;
	imndat = (unsigned char *) malloc (lendat * sizeof(unsigned char));

/*
 *	Subset the original image. Subtract 1 from each coord because the
 *	values were computed in Fortran and count from 1 instead of 0.
 */
	for ( j = 0; j < ny; j++ )
	{
	    for ( i = 0; i < nx; i++ )
	    {
		ind1 = j * nx + i;
		ix   = i * (*ipix) + (*ixlef-1);
		iy   = j * (*ipix) + (*iytop-1);
		ind2 = iy * (*kx) + ix;
		if  ( ( 0 <= ix ) && ( ix < *kx ) &&
		      ( 0 <= iy ) && ( iy < *ky ) )
		{
		    imndat [ind1] = imdata [ind2];
		}
		else
		{
		    imndat [ind1] = 0;
		}
	    }
	}

}
