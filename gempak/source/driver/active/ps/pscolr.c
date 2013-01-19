#include "pscmn.h"

void pscolr ( int *icbnk, int *icolr, int *iret )
/************************************************************************
 * pscolr								*
 * 									*
 * This subroutine sets the color on a graphics device.			*
 * 									*
 * pscolr  ( icbnk, icolr, iret )					*
 * 									*
 * Input parameters:							*
 *	icbnk		int*		Color bank number		*
 *	icolr		int*		Color number			*
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 3/96	Fixed math for RGB calculation 		*
 * S. Jacobs/NCEP	 2/98	Added color bank			*
 ***********************************************************************/
{

	int	icred, icgreen, icblue, ic, ier, lent;
	float	cred, cgreen, cblue;

	char	tmpbuf[40], xnm[20], cnm[20];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure plot file is open.
 */
	if  ( opnfil == 0 )
	{
	    psopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	Graphics color bank.
 */
	if  ( *icbnk == 0 )  {
/*
 *	    Set color 101 to color index 0.
 */
	    if  ( *icolr == 101)  {
		ic = 0;
	    }
	    else  {
/*
 *	    	Use only NNCOLR colors.
 */
		ic = *icolr ;
		if  ( ( ic < 1 ) || ( ic > nncolr ) )  ic = nncolr;
	    }

/*
 *	    Get the color component values.
 */
	    cqcomp ( &ic, cnm, &icred, &icgreen, &icblue, xnm, &ier );
	    if  ( ier != G_NORMAL )  return; 

/*
 *	    Convert color data range to values from 0.0 to 1.0.
 */
	    cred   = (float) icred / 255.0;
	    cblue  = (float) icblue / 255.0;
	    cgreen = (float) icgreen / 255.0;
	}
	else  {
	    cred   = clrbank[*icbnk].color[*icolr].krgun / 255.0;
	    cblue  = clrbank[*icbnk].color[*icolr].kbgun / 255.0;
	    cgreen = clrbank[*icbnk].color[*icolr].kggun / 255.0;
	}

/*
 *	Write color components to plot file.
 */
	sprintf ( tmpbuf, " %5.3f %5.3f %5.3f RGB \n",
		  cred, cgreen, cblue);
	lent = strlen ( tmpbuf );
	cfl_writ ( flun, lent, (unsigned char *)tmpbuf, &ier );

/*
 *	Now that color has been set, reset flag.
 */
	resetc = G_FALSE;
	mcolr  = ic;

}
