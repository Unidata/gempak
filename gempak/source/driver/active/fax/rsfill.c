#include "faxcmn.h"
#include "pattern.h"

void rsfill ( float *szfil, int *iftyp, int *iret )
/************************************************************************
 * rsfill								*
 *									*
 * This subroutine set the fill pattern type and size.			*
 *									*
 * rsfill ( szfil, iftyp, iret )					*
 *									*
 * Input parameters:							*
 *	*szfil		float		Fill pattern size		*
 *	*iftyp		int		Fill pattern type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/97						*
 * S. Jacobs/NCEP	 3/98	Added fill pattern include file		*
 * m.gamazaychikov/SAIC 01/03   Changed NPATFL to NFILLPAT              *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global variables for fill pattern type and size.
 */
	tsfill = *szfil;

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
