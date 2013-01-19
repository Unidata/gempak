#include "geminc.h"
#include "gemprm.h"

extern unsigned char *imndat;
extern unsigned char *imdata;

void im_wgin ( char filnam[], int *lenf, int *offset, int *lendat, int *iret )
/************************************************************************
 * im_wgin								*
 *									*
 * This subroutine writes the image data to an AWIPS GINI file.		*
 *									*
 * im_wgin ( filnam, lenf, offset, lendat, iret )			*
 *									*
 * Input parameters:							*
 *	filnam[]	char		Name of image file		*
 *	*lenf		int		Length of the file name		*
 *	*offset		int		Offset in the file to the data	*
 *	*lendat		int		Length of the data		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/96	Copied from im_rgin			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 ***********************************************************************/
{

	char		tfile[133];
	FILE		*fp;
	int		ier;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL ;

/*
 *	Copy the file name and add a NULL at the end.
 */
	strncpy ( tfile, filnam, *lenf );
	tfile[*lenf] = CHNULL;
	
/*
 *	Open the file and seek to data offset.
 */
 	 
	fp = cfl_uopn ( tfile, &ier );
	if  ( ier != 0 )
	{
	    *iret = G_NIMGFL;
	    return;
	}
 	cfl_seek ( fp, *offset, SEEK_SET, &ier );
	if  ( ier != 0 )
	{
	    *iret = G_NIMGFL;
	    return ;
	}
		
/*
 *	Write the image data.
 */
	 
	cfl_writ ( fp, *lendat, imndat, &ier ) ;
	if  ( ier != 0 )  *iret = -1002;
	cfl_clos ( fp, &ier ) ;

	free ( imndat );
	free ( imdata );

}
