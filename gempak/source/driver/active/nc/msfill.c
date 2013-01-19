#include "nccmn.h"

void msfill ( float *szfil, int *iftyp, int *iret )
/************************************************************************
 * msfill								*
 *									*
 * This subroutines sets the fill pattern type and size.		*
 *									*
 * msfill ( szfil, iftyp, iret )					*
 *									*
 * Input parameters:							*
 *	*szfil		float		Fill pattern size		*
 *	*iftyp		int		Fill pattern type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * S. Jacobs/NCEP	 3/98						*
 * m.gamazaychikov/SAIC 01/03  	Changed NPATFL to NFILLPAT		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global variables for fill pattern type and size.
 */
	if  ( *iftyp < 1 )  {
	    fstyle_req = 1;
	}
	else if  ( *iftyp > NFILLPAT )  {
	    fstyle_req = NFILLPAT;
	}
	else  {
	    fstyle_req = *iftyp;
	}

}
