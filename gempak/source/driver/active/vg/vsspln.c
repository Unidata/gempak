#include	"vgcmn.h"

void vsspln ( int *msltyp, int *mslstr, int *msldir, float *tslsiz, 
						int *mslwid, int *iret )
/************************************************************************
 * vsspln								*
 *									*
 * This subroutine sets the special line attributes.			*
 *									*
 * vsspln ( msltyp, mslstr, msldir, tslsiz, mslwid, iret )		*
 *									*
 * Input parameters:							*
 *	*msltyp		int		Special line type		*
 *	*mslstr		int		Special line stroke multiplier	*
 *	*msldir		int		Special line direction indicator*
 *	*tslsiz		float		Special line size		*
 *	*mslwid		int		Special line width		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D. Keiser/GSC	 4/97						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	ksltyp = *msltyp;
	kslstr = *mslstr;
	ksldir = *msldir;
	rslsiz = *tslsiz;
	kslwid = *mslwid;

}
