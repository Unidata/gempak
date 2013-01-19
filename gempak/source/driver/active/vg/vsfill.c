#include "vgcmn.h"

void vsfill ( float *szfil, int *iftyp, int *iret )
/************************************************************************
 * vsfill								*
 *									*
 * This subroutines sets the fill pattern type and size.		*
 *									*
 * vsfill ( szfil, iftyp, iret )					*
 *									*
 * Input parameters:							*
 *	*szfil		float		Fill pattern size		*
 *	*iftyp		int		Fill pattern type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * S. Jacobs/NCEP	 3/98						*
 * m.gamazaychikov/SAIC 01/03   Changed NPATFL to NFILLPAT              *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global variables for fill pattern type and size.
 */
	rszfil = *szfil;

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
