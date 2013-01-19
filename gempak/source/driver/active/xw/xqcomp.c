#include "xwcmn.h"

void xqcomp ( int *index, char *clrnam, int *red, int *green, int *blue,
			char *xname, int *clen, int *xlen, int *iret )
/************************************************************************
 * xqcomp								*
 *									*
 * This subroutine returns the color name, RGB values, and X color name *
 * (if available).							*
 *									*
 * xqcomp ( index, clrnam, red, green, blue, xname, clen, xlen, iret )	*
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
	   *clen = strlen(clrnam);	
	   *xlen = strlen(xname);
	}

}
