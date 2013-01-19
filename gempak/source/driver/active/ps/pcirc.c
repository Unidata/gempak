#include "pscmn.h"

void pcirc ( float *xcen, float *ycen, float *xrad, float *yrad, int *iret )
/************************************************************************
 * pcirc								*
 *									*
 * This subroutine draws a circle to the PostScript output file.	*
 *									*
 * pcirc  (  XCEN, YCEN, XRAD, YRAD, IRET )	                        *
 *									*
 * Input parameters:							*
 *	XCEN 		float*		X center coordinate		*
 *	YCEN		float*		Y center coordinate		*
 *	XRAD 		float*		X radius point                  *
 *	YRAD 		float*		Y radius point                  *
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		11/98		Modified from pdots             *
 ***********************************************************************/
{

	char 	buff[88];
	int	icbnk, lenb, ird, ixcnt, iycnt, ier;
	double  dx, dy;
	float   rad;

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
 	dx = *xcen - *xrad;
 	dy = *ycen - *yrad;
	rad = ( float ) sqrt ( dx * dx  +  dy * dy  );
	ird = G_NINT ( rad );
	ixcnt = G_NINT ( *xcen );
	iycnt = G_NINT ( *ycen );
/*
 *	Draw circle.
 */
	sprintf ( buff, " N %5d %5d %5d 0 360 A stroke \n", ixcnt, iycnt, 
	          ird );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

}
