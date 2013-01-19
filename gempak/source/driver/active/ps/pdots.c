#include "pscmn.h"

void pdots ( int *ix, int *iy, int *ilwid, int *iret )
/************************************************************************
 * PDOTS								*
 *									*
 * This subroutine draws a dot to the PostScript output file.		*
 *									*
 * PDOTS  ( IX, IY, ILWID, IRET )					*
 *									*
 * Input parameters:							*
 *	ix 		int*		X coordinates			*
 *	iy 		int*		Y coordinates			*
 *	ilwid		int*		Line width			*
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI	 	 2/94						*
 * C. Lin/EAI	 	 2/98    changed calling sequence to pscolr	*
 ***********************************************************************/
{

	char 	buff[88];
	int	icbnk, jlwid, lenb, ier;

/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that plot file is open.
 */
	if  ( ! opnfil ) {
	    psopen ( iret );
	    if  ( *iret != G_NORMAL ) return; 
	}
	psplot = G_TRUE;

/*
 *	Set the color to black, if the color scheme type is monochrome.
 */
	if  ( kctype == 0 )
	{
	    if  ( mcolr != 1 )
	    {
		mcolr  = 1;
		resetc = G_TRUE;
	    }
	}

/*
 *	Set color components if they have changed.
 */
	if  ( resetc ) {
	    icbnk = 0;
	    pscolr ( &icbnk, &mcolr, &ier );
	}

/*
 *	Calculate radius of circle to draw.
 */
	jlwid = *ilwid;
	if  ( *ilwid == 1 )  {
	    jlwid = 4;
	}
	else {
	    jlwid = jlwid / 2;
	}

/*
 *	Draw circle and fill it in.
 */
	sprintf ( buff, " N %5d %5d %4d 0 360 AF\n", *ix, *iy, jlwid );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

}
