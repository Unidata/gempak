#include "pscmn.h"

#define BUFFLEN ( 400 + 20 )

void ptextc ( float *xr, float *yr, char *cchar, int *lens, 
		int *ixoff, int *iyoff, float *rotat, int *ispanx, 
		int *ispany, int *icleft, int *icrght, int *icbot, 
		int *ictop, int *iret )
/************************************************************************
 * ptextc								*
 *									*
 * This subroutine draws hardware text to the PostScript file.		*
 *									*
 * ptextc ( xr, yr, cchar, lens, ixoff, iyoff, rotat, 			*
 *	    ispanx, ispany, icleft, icrght, icbot, ictop, iret )	*
 *									*
 * Input parameters:							*
 *	*xr 		float		X coordinate 			*
 *	*yr 		float		Y coordinate			*
 *	*cchar		char		Text				*
 *	*lens		int		Length of text			*
 *	*ixoff		int		X offset			*
 *	*iyoff		int		Y offset			*
 *	*rotat		float		Rotation angle			*
 *	*ispanx		int		Direction of increasing x	*
 *	*ispany		int		Direction of increasing y	*
 *	*icleft		int		Left clipping bound		*
 *	*icrght		int		Right clipping bound		*
 *	*icbot		int		Bottom clipping bound		*
 *	*ictop		int		Top clipping bound		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/98	Copied from PTEXT			*
 * S. Jacobs/NCEP	 3/06	Fix size checks to not use != and ==	*
 ***********************************************************************/
{

	char	fontnm[24], buff[BUFFLEN], cjust[3];
	float	xo, yo, x, y, size, adjust;
	int	jxost, ier, maxchr, jlen, ipos;
	int	jx, jy, found, lenb, lenf, istrt;
	int	icbnk;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;

/*
 *  Convert the text justification to PostScript commands.
 */
	if  ( kjust == 1 )  {
	    strcpy ( cjust, "LT" );
	    istrt  = 0;
	    adjust = -100.0 * txsizr;
	}
	else if  ( kjust == 3 )  {
	    strcpy ( cjust, "RT" );
	    istrt  = - ( *lens );
	    adjust = 100.0 * txsizr;
	}
	else  {
	    strcpy ( cjust, "CT" );
	    istrt  = - ( *lens / 2 );
	    adjust = 0.0;
	}

/*
 *  Convert the text location to integers.
 */
	xo = ( *ixoff / 2. ) * txszx * txsizr + adjust;
	x  = *xr + xo * *ispanx;
	jx = G_NINT ( x );

	yo = ( *iyoff / 2. ) * txszy * txsizr - 90. * txsizr;
	y  = *yr + yo * *ispany;
	jy = G_NINT ( y );

/*
 *  Check that graphics file is open.
 */
	if  ( ! opnfil ) { 
	    psopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}
	psplot = G_TRUE;

/*
 *  Set the color to black, if the color scheme type is monochrome.
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
 *  Set color components if they have changed.
 */
	if  ( resetc ) {
	    icbnk = 0;
	    pscolr ( &icbnk, &mcolr, &ier );
	}

/*
 *  If the user wants to clip the text, do the following check.
 *  Otherwise, skip the check and plot the text string.
 *
 *  Check to see if the start point is outside the clipping window.
 *  Then the end point is checked.  Rotation is not taken into 
 *  account.
 */
	jxost  = G_NINT ( jx + istrt * txszx * txsizr );
	if ( ( *ispanx * ( jx - *icleft ) < 0 )  || 
	     ( *ispanx * ( jx - *icrght ) > 0 )  || 
	     ( *ispany * ( jy - *icbot  ) < 0 )  || 
	     ( *ispany * ( jy - *ictop  ) > 0 ) )  return;

/*
 *  Set the text font if it has changed.  Add to list if not
 *  there already.
 */ 
	if  ( irfont != isfont ) { 
	    pfontn ( irfont, fontnm );
	    sprintf ( buff, " /%s FF\n", fontnm );
	    lenb = strlen ( buff );
	    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
	    isfont = irfont;
	    txsizs = 0;
	    found  = G_FALSE;
	    if  ( nfontu > 0 )  {
		cst_find  ( fontnm, (const char **)fontus, nfontu, &ipos, &ier );
		if  ( ipos >= 0 )  found = G_TRUE;
	    }
	    if  ( ! found )  {
		lenf = strlen ( fontnm );
		fontus[nfontu] = (char *)malloc((lenf+1) * sizeof(char));
		strcpy ( fontus[nfontu], fontnm );
		nfontu++; 
	    }
	}

/*
 *  Now set the size.
 */
	if  ( G_ABS(txsizr-txsizs) > .0005  ) { 
	    if  ( G_ABS(txsizs-0.) < .0005 )  {
/*
 *  The PS font size is scaled by 320 because the plot
 *  space is scaled by 32 and the base font is 10-point.
 */
		size = 320. * txsizr;
	    }
	    else {
		strcpy ( buff, " CF\n" );
		lenb = strlen ( buff );
		cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
		size = txsizr / txsizs;
	    }
	    sprintf ( buff, " [%10.3f 0 0 %10.3f 0 0] MS\n", size, size );
	    lenb = strlen ( buff );
	    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
	    txsizs = txsizr;
	}

/*
 *  Check for maximum number of characters before reaching
 *  end of line.
 */
	maxchr = ( *icrght - jxost ) / ( txszx * txsizs );
	jlen   = G_MIN ( maxchr, *lens );
	if ( jlen <= 0 ) return;
	if ( jlen > ( BUFFLEN - 20 ) ) jlen = BUFFLEN - 20;

/*
 *  Write the text to the file.
 */
	cchar[jlen] = '\0';
	sprintf ( buff, " %6d%6d (%s) %s\n", jx, jy, cchar, cjust );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

}
