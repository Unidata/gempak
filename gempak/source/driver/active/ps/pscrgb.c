#include "pscmn.h"
#include "color.h"

void pscrgb ( int *type, int *icolr, int *ired, int *igreen, 
						int *iblue, int *iret )
/************************************************************************
 * PSCRGB								*
 *									*
 * This subroutine defines the color corresponding to a color number	*
 * by specifying the values of the red, green, and blue color		*
 * components.  The color components must be in the range 0 - 1.	*
 *									*
 * PSCRGB  ( type, icolr, ired, igreen, iblue, iret )			*
 *									*
 * Input parameters:							*
 *      type            int*            Type of color cell              *
 *                                         0 --- graphic cell           *
 *                                         1 --- satellite cell		*
 *                                         2 --- radar cell		*
 *      icolr           int*            Color number                    *
 *      ired            int*		Red				*
 *      igreen          int*            Green                           *
 *      iblue           int*            Blue                            *
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 * S. Jacobs/NCEP	 4/96	Changed RGB from float to int		*
 * S. Jacobs/NCEP	12/96	Added check for sat and rad colors	*
 * S. Jacobs/NCEP	 4/97	Removed increment of number of colors	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;
	
	if  ( *type == 0 )  {

/*
 *	    Cannot reset the RGB components of the background.
 */
	    if  ( *icolr == 101 )  return;

/*
 *	    Check to see if this is the current color.
 */
	    if  ( *icolr == mcolr )  resetc = G_TRUE; 

/*
 *	    Set the RGB components of the specified color.
 */
	    cscrgb ( icolr, ired, igreen, iblue, iret );

	}
	else {

/*
 *	    Set the RGB values for the satellite or radar colors.
 */
	    clrbank[*type].color[*icolr].krgun = *ired;
	    clrbank[*type].color[*icolr].kggun = *igreen;
	    clrbank[*type].color[*icolr].kbgun = *iblue;
	}

}
