#include "tiffcmn.h"
#include "pattern.h"

void tsfill ( float *szfil, int *iftyp, int *iret )
/************************************************************************
 * tsfill								*
 *									*
 * This subroutine set the fill pattern type and size.			*
 *									*
 * tsfill ( szfil, iftyp, iret )					*
 *									*
 * Input parameters:							*
 *	*szfil		float		Fill pattern size		*
 *	*iftyp		int		Fill pattern type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 * m.gamazaychikov/SAIC 01/03   Changed NPATFL to NFILLPAT              *
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
