#include "xwcmn.h"

void gfendd ( int *iret )
/************************************************************************
 * gfendd								*
 *									*
 * This subroutine closes the GF output file.				*
 *									*
 * void gfendd ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		 2/01	Copied from PENDD			*
 * T. Piper/GSC		 3/01	Fixed IRIX6 compiler warnings		*
 * T. Piper/SAIC	02/08	Renamed from gf/xendd to gfendd		*
 ***********************************************************************/
{
    int	ixsize, iysize, ncwn;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  Close file at end of plot.
 */
    gfclosp ( &ixsize, &iysize, &ncwn, iret );
}
