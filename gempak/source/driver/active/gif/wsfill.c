#include "gifcmn.h"

void wsfill ( float *szfil, int *iftyp, int *iret )
/************************************************************************
 * wsfill								*
 *									*
 * This subroutine set the fill pattern type and size.			*
 *									*
 * wsfill ( szfil, iftyp, iret )					*
 *									*
 * Input parameters:							*
 *	*szfil		float		Fill pattern size		*
 *	*iftyp		int		Fill pattern type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Danz/AWC	 	11/03						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global variables for fill pattern type and size.
 */
	tszfil = *szfil;

	if  ( *iftyp < 1 )  {
	    kfillt = 1;
	}
	else if  ( *iftyp > NFILLPAT )  {
	    kfillt = NFILLPAT;
	}
	else  {
	    kfillt = *iftyp;
	}

}
