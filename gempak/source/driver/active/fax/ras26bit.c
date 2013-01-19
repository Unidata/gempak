#include "faxcmn.h"

void ras26bit ( int *iret )
/************************************************************************
 * ras26bit								*
 *									*
 * This routine converts a raster image to the NMC 6-bit fax format.	*
 *									*
 * ras26bit  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96 	Created					*
 * E. Wehner/EAi	 3/97	Remove xsize and ysize			*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Removed reopening of raster file	*
 ***********************************************************************/
{

	int	nbytes, ier;
	char	faxfil[80];
	FILE	*fp;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Create the 6-bit output file.
 */
	sprintf ( faxfil, "%s.6bt", wheel );
	fp = cfl_wopn ( faxfil, &ier );
	if  ( ier != 0 )  {
	    *iret = G_NOWROPN;
	    return;
	}

/*
 *	Clear the 6-bit array, then convert the raster image to 
 *	the 6-bit fax format.
 */
	memset ( sixbit, '\0', msize );
	rcvt6bt ( &nbytes, iret );

/*
 *	If the conversion is successful, write the 6-bit compressed
 *	image to the file.
 */
	if  ( *iret == 0 )  {
	    cfl_writ ( fp, nbytes, sixbit, &ier );
	}

/*
 *	Close the 6-bit file.
 */
	cfl_clos ( fp, &ier );

}
