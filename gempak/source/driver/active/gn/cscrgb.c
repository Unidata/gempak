#include "geminc.h"
#include "gemprm.h"
#include "color.h"
#include "proto_xw.h"

void cscrgb ( int *indx, int *red, int *green, int *blue, int *iret )
/************************************************************************
 * cscrgb								*
 *									*
 * This subroutine updates the RGB color components located in the	*
 * currently selected graphic colors structure.				*
 *									*
 * cscrgb ( indx, red, green, blue, iret )				*
 *									*
 * Input parameters:							*
 *	*indx		int		Color index			*
 *	*red		int		Red color component		*
 *	*green		int		Green color component		*
 *	*blue		int		Blue color component		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 2 = RGB not in Gempak table	*
 *					 0 = No errors			*
 *					-1 = Index out of range		*
 *					-2 = RGB invalid		*
 **									*
 * Log:									*
 * L. Williams/EAI	 2/96						*
 * S. Jacobs/NCEP	 3/99	Fixed check for Green comp equal -1	*
 ***********************************************************************/
{
GmcColor	*gmc;
int		ii, rgbfnd;
int		fnddup, invrgb;

/*---------------------------------------------------------------------*/

	*iret = 0;
	rgbfnd = 0;
	fnddup = 0;
	invrgb = 1;

	gmc = &gemCmap.color[0];

	/*
	 * Check if index is valid
	 */

	if ( (*indx < 0) || (*indx >= ngemC ) ) {
		*iret = -1;
		return;
	}

	/*
	 * Check if RGB is valid ( 0 - 255 )
	 */
	if ( (*red != -1) || (*green != -1) || (*blue != -1) ) {
	   if ( (*red >= 0 && *red <= 255) &&
	        (*green >= 0 && *green <= 255) &&
	        (*blue >= 0 && *blue <= 255) ) {
		   invrgb = 0;
	   }
	}
	else {
	   if ( gemColrs[*indx].index != -1 )
	      invrgb = 0;
	}


	/*
	 * if RBG is invalid return
	 */
	if ( invrgb ) {
	    *iret = -2;
	    return;
	}


	/*
	 * Check for duplicate processing of RGB
	 */
	if ( ( *red == -1 ) && ( *green == -1 ) && ( *blue == -1 ) ) {
		fnddup = 1;
	}
	else {

	   if ( (gemColrs[*indx].red == *red) &&
	        (gemColrs[*indx].green == *green) &&
	        (gemColrs[*indx].blue == *blue)) {
			fnddup = 1;
	   }

	}

	if ( fnddup ) {
		/*
		 * Duplicate found
		 */
		*iret = 0;
		return;
	}


	/*
	 * Check if RGB is located in the gempak color table.
	 */
	for (ii=0; ii<gemCmap.nc; ii++){
	    if( (gmc[ii].red == *red) && (gmc[ii].green == *green) &&
			(gmc[ii].blue == *blue) ) {
		rgbfnd = 1;
		break;
	    }
	}

	if ( rgbfnd ) {
		/*
	    	 * RGB found in Gempak table
	    	 */
		gemColrs[*indx].index = ii;
		strcpy(gemColrs[*indx].name, gmc[ii].gcname);
		gemColrs[*indx].red = -1;
		gemColrs[*indx].green = -1;
		gemColrs[*indx].blue = -1;
	}
	else {
		/*
		 * RGB not found
		 */
		gemColrs[*indx].index = -1;
		strcpy(gemColrs[*indx].name, "No Name");
		gemColrs[*indx].red = *red;
		gemColrs[*indx].green = *green;
		gemColrs[*indx].blue = *blue;
		*iret = 2;
	}

}
