#include "geminc.h"
#include "gemprm.h"
#include "color.h"
#include "proto_xw.h"


void cscnam ( int *indx, char *clrname, int *iret )
/************************************************************************
 * cscnam								*
 *									*
 * This subroutine assigns a color name to a color number.		*
 *									*
 * cscnam ( indx, clrname, iret )					*
 *									*
 * Input parameters:							*
 *	*indx		int		Color index			*
 *	*clrname	char		color name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 0 = successful & name found in *
 *					     Gempak table		*
 *					 2 = successful & name not found*
 *					     in Gempak table		*
 *					-1 = Invalid index		*
 **									*
 * Log:									*
 * L. Williams/EAI	 2/96						*
 ***********************************************************************/
{
GmcColor	*gmc;
int		ii, fndclr;
int		rc=0;
char		tcname[40], tgcname[40];
char		tmpabv[4], txname[40];
char		tmpnme[40];

/*---------------------------------------------------------------------*/

	fndclr = 0;

	gmc = &gemCmap.color[0];

	/*
	 * Check if index is valid
	 */
	if ( (*indx < 0) || (*indx >= ngemC) ) {
		*iret = -1;
		return;
	}

	/*
	 * Check for duplicate processing
	 */
	cst_lcuc( clrname, tcname, &rc );
	cst_lcuc( gemColrs[*indx].name, tmpnme, &rc );
	if (strcmp(tcname, tmpnme) == 0 ) {
	    if ( gemColrs[*indx].index != -1 )
		*iret = 0;
	    else
		*iret = 2;
	    return;
	}

	/*
	 * Check if color name is located in the gempak color table.
	 */
	for (ii=0; ii < gemCmap.nc; ii++) {
	    cst_lcuc( gmc[ii].gcname, tgcname, &rc );
	    cst_lcuc( gmc[ii].abvname, tmpabv, &rc );
	    cst_lcuc( gmc[ii].xname, txname, &rc );
	    if ( (strcmp( tgcname, tcname ) == 0) ||
		 (strcmp( tmpabv,tcname ) == 0) ||
		 (strcmp( txname, tcname ) == 0) ) {
		   fndclr = 1;
		   *iret = 0;
		   break;
	    }
	}

	/*
	 * Set color name
	 */
	strcpy( gemColrs[*indx].name, clrname );

	if ( fndclr ) {

	   /*
	    * Color name found in Gempak table
	    */
	   gemColrs[*indx].index = ii;
	   gemColrs[*indx].red = -1;
	   gemColrs[*indx].green = -1;
	   gemColrs[*indx].blue = -1;
	}
	else {
	   /*
	    * Color name not found in table.
	    */
	   gemColrs[*indx].index = -1;
	   gemColrs[*indx].red = -1;
	   gemColrs[*indx].green = -1;
	   gemColrs[*indx].blue = -1;
	   *iret = 2;
	}

}
