#include	"vgcmn.h"

void vsclr2 ( int *icolr, int *iclr2, int *iret )
/************************************************************************
 * vsclr2								*
 *									*
 * This subroutine sets the current color.				*
 *									*
 * vsclr2 ( icolr, iclr2, iret )					*
 *									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
 *      *iclr2          int             Color number                    *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * M. Li/GSC		 7/00	Created     				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	kcolr = *icolr;
	kcolr2 = *iclr2;

}
