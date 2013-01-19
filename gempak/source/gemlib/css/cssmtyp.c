#include "geminc.h"
#include "gemprm.h"

void css_mtyp ( int *mchtyp, int *iret )
/************************************************************************
 *  css_mtyp								*
 *									*
 * This subroutine returns the type of byte order for the current	*
 * workstation. Most UNIX workstations use IEEE formatting and are	*
 * "Big Endian". However, Linux, Ultrix and OSF1 are "Little Endian".	*
 *									*
 * css_mtyp ( mchtyp, iret )						*
 *									*
 *  Output parameters:							*
 *	*mchtyp		int		Machine type			*
 *					  0 = Little Endian		*
 *					  1 = Big Endian		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	11/99	Created					*
 ***********************************************************************/
{
	int	one = 1;
	char	*cp = (char*)&one;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	*mchtyp = (*cp == 0);

}
