#include "nccmn.h"
#include "color.h"

void mqcomp ( int *index, char *clrnam, int *red, int *green, 
		int *blue, char *xname, int *clen, int *xlen, int *iret )
/************************************************************************
 * mqcomp								*
 *									*
 * mqcomp ( index, clrnam, red, green, blue, xname, clen, xlen, iret )	*
 *									*
 * This subroutine returns the color name, RGB values, and X color name *
 * (if available).							*
 *									*
 * Input parameters:							*
 *	*index		int		Color index			*
 *									*
 * Output parameters:							*
 *	*clrnam		char		color name			*
 *	*red		int		Red color component		*
 *	*green		int		Green color component		*
 *	*blue		int		Blue color component		*
 *	*xname		char		X color name			*
 *	*clen		int		length of color name		*
 *	*xlen		int		length of X color name		*
 *	*iret		int		Return code			*
 *					 0 = successful 		*
 *					-1 = Index out of range 	*
 **									*
 * Log:									*
 * L. Williams/EAI	 2/96						*
 * S. Jacobs/NCEP	 4/96	Copied from the XW to the NC driver	*
 * T. Piper/SAIC	 7/03	Replaced xwcmn.h with geminc.h&gemprm.h	*
 ***********************************************************************/
{
int	ctmp;

/*---------------------------------------------------------------------*/

	*iret = 0;

	if ( *index == 101 )
	    ctmp = 0;
	else
	    ctmp = *index;

	cqcomp( &ctmp, clrnam, red, green, blue, xname, iret );

	if ( *iret ) {  
	   *clen = *xlen = *red = *green = *blue = 0;	
	}
	else {
	   *clen = (int)strlen(clrnam);	
	   *xlen = (int)strlen(xname);
	}

}
