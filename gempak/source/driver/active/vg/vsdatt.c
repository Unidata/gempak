#include "vgcmn.h"

void vsdatt ( int *iunit, char *fname, int *itype, int *iret )
/************************************************************************
 * vsdatt								*
 * 									*
 * This subroutine defines the device attributes.			*
 * 									*
 * vsdatt ( iunit, fname, itype, iret )					*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Name of file as output		*
 *	*itype		int		Device type (color,bw,gs)	*
 *									*
 * Output parameters:							*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * S. Jacobs/NCEP	 3/01	Added close and open when file changed	*
 * D. Kidwell/NCEP	 6/02	Added call to vsetgrps                  *
 * D. Kidwell/NCEP	 6/02	Changed vsetgrps call sequence          *
 * S. Jacobs/NCEP	 8/02	Moved call to vsetgrps to after vopen	*
 * T. Piper/SAIC	02/04	Removed lenf, xsz, and ysz parameters	*
 ***********************************************************************/
{

	int	ier;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global output and color scheme types
 */
	kctype = *itype;
	kunit  = *iunit;

/*
 *	If the passed in filename is different from the global filename,
 *	change the name.
 */
	if  ( strcmp ( curfil, fname ) != 0 )
	{

/*
 *	    If the new file name is not empty, set the current file name.
 */
	    if  ( fname[0] != CHNULL )
	    {
		strcpy ( curfil, fname );

		/*
		 * Close the old file.
		 */
		vclosp ( &ier );

		/*
		 * Open the new file.
		 */
		vopen  ( &ier );

		/*
		 * Update all group numbers.
		 */
		vsetgrps ( &ier );

		/*
		 * Set the return code.
		 */
		*iret = G_NEWWIN;
	    }

	}

}
