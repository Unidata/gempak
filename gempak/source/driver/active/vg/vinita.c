#define GLOBAL

#include "vgcmn.h"

void vinita ( int *iunit, char *filnam, int *lenf, int *itype, int *iret )
/************************************************************************
 * vinita								*
 *									*
 * This subroutine is called to initialize the metafile.		*
 *									*
 * vinita  ( iunit, filnam, lenf, itype, iret )				*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*filnam		char		Output metafile name		*
 *	*lenf		int		Length of file name		*
 *	*itype		int		Device color type		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97	Copied from MINITA			*
 * E. Wehner/Eai	 7/97	Initialize group type and group num	*
 * J. Wu/GSC	 	02/01	Opened curfil via vopen() for update	*
 * D. Kidwell/NCEP	 6/02	Added vsetgrps, dropped mxgpnm init     *
 * D. Kidwell/NCEP	 6/02	Changed vsetgrps call sequence          *
 * T. Piper/SAIC	02/04	Removed xsize and ysize parameters	*
***********************************************************************/
{
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global output and color scheme types.
 */
	kctype = *itype;
	kunit  = *iunit;

/*
 *	If the input length is greater than the maximum allowed,
 *	return an error.
 */
	if  ( ( *lenf >= MXFLEN ) || ( *lenf <= 0 ) )  {
	    *iret = G_NOMETA;
	    return;
	}

/*
 *	If the new file name is not empty, set the current file name.
 */
	if  ( filnam[0] != CHNULL )  {
	    strcpy ( curfil, filnam );
	    *iret = G_NEWWIN;
	}

/*
 *	Initialize the group type and group number to 0.
 */
	kgtyp = 0;
	kgnum = 0;

/*
 *	Initialize the color tables in case the user queries the
 *	color components.
 */
	cctabl( NULL, &ier );
	if  ( ier != G_NORMAL )  return;

	csctbl ( NULL, &ier );

/*
 *	Open the file and mark it.
 */
        vopen ( iret );
/*
 *	Update all group numbers.
 */
        vsetgrps ( &ier );
}
