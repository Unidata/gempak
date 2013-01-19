#include "tiffcmn.h"

void tclosp ( int *iret )
/************************************************************************
 * tclosp								*
 *									*
 * This subroutine closes the raster image and writes the compressed	*
 * image to a file.							*
 *									*
 * tclosp  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

	int	ier, nbytes;
	FILE	*fptr;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Write the data to a Group 4 Fax compressed TIFF file.
 */
	if  ( opnfil )  {

	    opnfil = G_FALSE; 

/* 
 *	    Open the output TIFF file.
 */
	    fptr = cfl_wopn ( filnam, &ier );
	    if  ( ier != 0 )  {
		*iret = G_NOWROPN;
		return;
	    }

/*
 *	    Clear the Group 4 compressed array, then compress the
 *	    raster image.
 */
	    memset ( group4, CHNULL, msize );
	    ttiff ( &nbytes, iret );

	    if  ( *iret == 0 )  {
		cfl_writ ( fptr, nbytes, group4, &ier );
	    }

	    cfl_clos ( fptr, &ier );

	}

}
