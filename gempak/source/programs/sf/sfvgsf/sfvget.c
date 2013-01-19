#include "sfvcmn.h"

void sfvget ( int *ngrp, int *nparm, char stid[], int *nstr, 
						float rdata[], int *iret )
/************************************************************************
 * sfvget								*
 *									*
 * This function gets and returns all data in the given group/station	*
 * number.								*
 *									*
 * sfvget ( ngrp, nparm, stid, nstr, rdata, iret )			*
 *									*
 * Input parameters:							*
 *	*ngrp		int		Current group number		*
 *	*nparm		int		Number of parameters		*
 *									*
 * Output parameters:							*
 *	stid []		char		Station id for this group	*
 *	*nstr		int		Number of character in the stid	*
 *	rdata []	float		Data for this group		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/99						*
 * S. Jacobs/NCEP	 3/99	Added string length to call		*
 ***********************************************************************/
{

	int	i, ier;

/*---------------------------------------------------------------------*/

	*iret = 0;

	strcpy ( stid, stns[*ngrp].stid );
	cst_lstr ( stid, nstr, &ier );

	for ( i = 0; i < *nparm; i++ )  {
	    rdata[i] = stns[*ngrp].rdata[i];
	}

}
