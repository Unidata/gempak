#include "pscmn.h"
#include "color.h"

void pscnam ( int *cindex, char *cname, int *len, int *iret )
/************************************************************************
 * pscnam								*
 *									*
 * This routine defines the color by name				*
 *									*
 * pscnam  ( cindex, cname, len, iret )					*
 *									*
 * Input parameters:							*
 *	*cindex		int		Color number			*
 *	*cname		char		Color name			*
 *	*len		int		length of color name		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -1 = invalid color index	*
 *					  -2 = invalid color name	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	11/96						*
 * C. Lin/EAI            2/98    changed calling sequence to pscolr     * 
 ***********************************************************************/
{
	GmcColor	*gmc;
	int		icbnk, ier;
	int		red, green, blue;
	int		ctmp, indx, tindex;
	char		tname[40];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	gmc = &gemCmap.color[0];

	/*
	 * Check for background color.
	 */
	if  ( *cindex == 101 ) {
	    ctmp = 0;
	}
	else if  ( ( *cindex >= 1 ) && ( *cindex < nncolr ) ) {
	    ctmp = *cindex;
	}
	else {
	    *iret = -1;
	    return;
	}

	cname[*len] = '\0';

	/*
	 * Save original data.
	 */
	tindex = gemColrs[ctmp].index;
	strcpy(tname, gemColrs[ctmp].name);
	red = gemColrs[ctmp].red;
	green = gemColrs[ctmp].green;
	blue = gemColrs[ctmp].blue;
	
	cscnam ( &ctmp, cname, &ier );

	indx = gemColrs[ctmp].index;

	if ( ier == 0 ) {

	   /*
	    * Retrieve color name from the GEMPAK color table.
	    */
	   red = gmc[indx].red;
	   green = gmc[indx].green;
	   blue = gmc[indx].blue;
	}

	else {

	     /*
	      * Color name not found in XColor table.  Set the
	      * original index, color name, and RGB values.
	      */
	     gemColrs[ctmp].index = tindex;
	     strcpy(gemColrs[ctmp].name, tname);
	     gemColrs[ctmp].red = red;
	     gemColrs[ctmp].green = green;
	     gemColrs[ctmp].blue = blue;
	     *iret = -2;
	} 

	/*
 	 * Reset color.
 	 */
	icbnk = 0;
	pscolr ( &icbnk, &ctmp, &ier );

}
