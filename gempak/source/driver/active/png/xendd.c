#include "xwcmn.h"

void xendd ( int *iret )
/************************************************************************
 * XENDD								*
 *									*
 * This subroutine closes the GIF output file.				*
 *									*
 * XENDD  ( IRET )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		 2/01	Copied from PENDD			*
 * T. Piper/GSC		 3/01	Fixed IRIX6 compiler warnings		*
 ***********************************************************************/
{

        int	ixsize, iysize, ncwn;
/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
        xclosp ( &ixsize, &iysize, &ncwn, iret );

}
