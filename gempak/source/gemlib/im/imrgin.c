#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define clz_rfil        clz_rfil_
#endif

unsigned char *imdata;

void im_rgin ( char filnam[], int *lenf, int *imcflg, int *offset,
		int *lendat, int *iret )
/************************************************************************
 * im_rgin								*
 *									*
 * This subroutine reads the image data from an AWIPS GINI file.	*
 *									*
 * im_rgin ( filnam, lenf, offset, lendat, iret )			*
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
 * S. Jacobs/NCEP	 7/96	Copied from xrgini			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. CHiswell/UCAR     11/03   Modified input list for compression flag*
 ***********************************************************************/
{

	char		defdir[5], tfile[133];
	FILE		*fp;
	int		ier, nread, ibin, ibout;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL ;

	strcpy ( defdir, " " );

/*
 *	Copy the file name and add a NULL at the end.
 */
	strncpy ( tfile, filnam, *lenf );
	tfile[*lenf] = CHNULL;

        imdata = (unsigned char *) malloc (*lendat * sizeof(unsigned char));

        if  ( *imcflg == 1 )
        {
           nread = 0;
           clz_rfil ( tfile, defdir, &nread, offset, lendat, imdata,
                &ibin, &ibout, &ier);
	   if ( ier != 0 )  *iret = -1001;
	   return;
	}
	

/*
 *	Open the file and seek to data offset.
 */
 	 
	fp = cfl_ropn ( tfile, defdir, &ier );
	if  ( ier != 0 )
	{
	    free(imdata);
	    *iret = G_NIMGFL;
	    return;
	}
 	cfl_seek ( fp, *offset, SEEK_SET, &ier );
	if  ( ier != 0 )
	{
	    free(imdata);
	    *iret = G_NIMGFL;
	    return ;
	}

/*	imdata = (unsigned char *) malloc (*lendat * sizeof(unsigned char)); */
		
/*
 *	Read the image data.
 */
	 
	cfl_read ( fp, *lendat, imdata, &nread, &ier ) ;
	if  ( ier != 0 )  *iret = -1001;
	cfl_clos ( fp, &ier ) ;

}
