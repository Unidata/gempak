#include "faxcmn.h"

void rpadrec ( int *istart, int irecsz, int *iret )
/************************************************************************
 * rpadrec								*
 *  									*
 * This routine pads the end of a record with zeros. The padding is	*
 * added to fill the record to size IRECSZ.				*
 *  									*
 * rpadrec ( istart, irecsz, iret )					*
 *									*
 * Input parameters:							*
 *	*istart		int		Position to start padding	*
 *	irecsz		int		Size of a record		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * S. Jacobs/NCEP	 7/97	Added faxcmn.h				*
 * S. Jacobs/NCEP	 7/97	Updated header, comments and var names	*
 ***********************************************************************/
{

	int	i, jstop, nrec;

/*---------------------------------------------------------------------*/

	*iret  = G_NORMAL;

/*
 *	Return if no padding is required.
 */
	if  ( ( *istart % irecsz ) == 0 )  return;

/* 
 *	If the record size is non-zero, compute the number of bytes
 *	to pad, otherwise, return.
 */
	if  ( irecsz > 0 )  {
	    nrec = *istart / irecsz;
	}
	else  {
	    return;
	}

/*
 *	Compute the ending byte of the current record.
 */
	jstop = (nrec+1) * irecsz; 

/*
 *	Add the padding for each byte from the start position to the 
 *	end of the record.
 */
	for ( i = *istart; i < jstop; i++ )  {
	    sixbit[i] = 0;
	}

/*
 *	Update the position pointer.
 */
	*istart = jstop;

}
