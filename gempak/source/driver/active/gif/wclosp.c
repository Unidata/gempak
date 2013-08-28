#include "gifcmn.h"

void wclosp ( char *filnam, int *gfplot, int *iret )
/************************************************************************
 * wclosp								*
 *									*
 * This function closes the current gif image.				*
 *									*
 * void wclosp ( filnam, gfplot, iret )					*
 *									*
 * Input and output parameters:						*
 *  *filnam		char	file name				*
 *  *gfplot		int						*
 *									*
 * Output parameters:							*
 *  *iret		int	return value				*
 *									*
 **									*
 * Log:									*
 * D. Austin		 5/96						*
 * T. Lee/GSC		 7/00	Renamed from gdr_closp; Used cfl_wopn	*
 * T. Piper/SAIC	 1/02	Fixed memory leak; freed string		*
 * S. Jacobs/NCEP	 1/02	Moved free of string			*
 * S. Danz/AWC   	11/03	Check if *gfplot is set, not the ptr    *
 *				Only attempt to free if Current is set  *
 * S. Jacobs/NCEP	 7/13	Added file name to error output		*
 ***********************************************************************/
{
	char *space;
	char *string;
	int ier;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	/* 
	 * Check the file.
	 */

	if  ( *gfplot && Current ) {

	    /*
	     * Make sure the new file's name is null terminated.
	     */

	    string = strdup ( filnam );
	    space = (char *) strchr( string, ' ' );
	    *space = '\0';

	    /*
	     * Set the filename in the struct.
	     */
	    Current->fname = string;

	    outfile =  cfl_wopn ( Current->fname, &ier );
	    if ( outfile == NULL ) {
		printf ( " Error: %d - Cannot open output file: %s\n",
		       	ier, Current->fname );
		exit (-1);
	    }

	    /*
	     * Free the temp string for the file name.
	     */
	    free ( string );

	    /* 
	     * Write the current imageout in GIF 89a format.
	     */

	    gdImageGif ( Current_Im, outfile );

	    /*
	     * Close the file.
	     */

	     cfl_clos ( outfile, &ier );
	}

	/*
	 * Free all resources.
	 */

        if ( Current ) {
	    free ( Current );
	    Current = NULL;
	    gdImageDestroy ( Current_Im );
	    Current_Im = NULL;
        }

	/*
	 * Reset the global pointers after closing the file.
	 */
	CurrentBGColorIndex = 0;
	CurrentColorIndex = 999;

}
