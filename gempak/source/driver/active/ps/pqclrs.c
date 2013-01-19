#include "pscmn.h"
#include "color.h"

void pqclrs ( int *cbank, int *ncolors, int *colors, int *iret )
/************************************************************************
 * pqclrs                                          			*
 *                                                                      *
 * This function queries the color structure in PS driver. It returns	*
 * the number of colors and the color index array.			*
 *                                                                      *
 * pqclrs ( cbank, ncolors, colors, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*cbank		int		Color bank ID			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ncolors	int		Number of colors		*
 *	*colors		int		Color index array		*
 *	*iret		int		Return Code			*
 *					G_NORMAL  = normal return	*
 *					G_NICBANK = invalid clr bank ID	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 1/97	Copied from XQCLRS			*
 * S. Jacobs/NCEP	 4/97	Set number of colors in common area	*
 * S. Jacobs/NCEP	 8/98	Changed colors from long int to int	*
 * R. Tian/SAIC		05/02	Added clrbank[] entry for fax		*
 * T. Piper/SAIC	01/04	Added color.h				*
 ***********************************************************************/
{

int	ii;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Exit if color bank number invalid
 */		
	if  ( ( *cbank > FaxCid ) || ( *cbank < GraphCid ) )  {
	    *iret = G_NICBANK;
	    return;
	}

/*
 *	Set the number of colors for cbank.
 */
	if  ( *cbank == GraphCid ) {
	    *ncolors = GRAPH_COLORS;
	}
	else {
	    *ncolors = MAXCOLORS;
	}
	clrbank[*cbank].ncolr = *ncolors;

	for ( ii = 0; ii < clrbank[*cbank].ncolr; ii++ )  {
	    colors[ii] = 0;
	}

}
