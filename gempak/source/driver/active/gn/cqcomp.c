#include "geminc.h"
#include "gemprm.h"
#include "color.h"
#include "proto_xw.h"


void cqcomp ( int *cindex, char *clrname, int *red, int *green, 
				int *blue, char *xname, int *iret )
/************************************************************************
 * cqcomp								*
 *									*
 * This subroutine returns the color name, RGB values, and X color name *
 *									*
 * void cqcomp ( cindex, clrname, red, green, blue, xname, iret )	*
 *									*
 * Input parameters:							*
 *	*cindex		int		Color index			*
 *									*
 * Output parameters:							*
 *	*clrname	char		color name			*
 *	*red		int		Red color component		*
 *	*green		int		Green color component		*
 *	*blue		int		Blue color component		*
 *	*xname		char		X color name			*
 *	*iret		int		Return code			*
 *					 0 = successful 		*
 *					-1 = Index out of range 	*
 **									*
 * Log:									*
 * L. Williams/EAI	 2/96						*
 ***********************************************************************/
{
GmcColor	*gmc;
int		gtindex;

/*---------------------------------------------------------------------*/


	gmc = &gemCmap.color[0];

	/*
	 * Check if index is invalid
	 */

	if ( (*cindex < 0) || (*cindex >= ngemC) ) {
		clrname = '\0';
		*red = 0;
		*green = 0;
		*blue = 0;
		xname = '\0'; 
		*iret = -1;
		return;
	}

	*iret = 0;

	/*
	 * Retrieve name, RGB, and xname
	 */
	if ( clrname )
	   strcpy(clrname, gemColrs[*cindex].name);

	if ( gemColrs[*cindex].index == -1 ) {
		*red = gemColrs[*cindex].red;
		*green = gemColrs[*cindex].green;
		*blue = gemColrs[*cindex].blue;
		if ( xname )
		  strcpy(xname, "No name");
	}
	else{
		gtindex = gemColrs[*cindex].index;

		*red = gmc[gtindex].red;
		*green = gmc[gtindex].green;
		*blue = gmc[gtindex].blue;
		if ( xname )
		  strcpy(xname, gmc[gtindex].xname);
	}
		
}
