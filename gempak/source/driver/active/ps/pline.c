#include "pscmn.h"
#include "color.h"

void pline ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * PLINE								*
 *									*
 * This subroutine draws lines to the PostScript file.			*
 *									*
 * PLINE  ( NP, IX, IY, IRET )						*
 *									*
 * Input parameters:							*
 *	*NP		INT		Number of points		*
 *	IX[NP]		INT		X coordinates			*
 *	IY[NP]		INT		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*IRET		INT		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI	 	 2/94						*
 * A. Chang/EAI	 	 9/94	General clean up			*
 * C. Lin/EAI            2/98   changed calling sequence to pscolr      *
 * S. Jacobs/NCEP	 3/99	Added check for BG color in Monochrome	*
 ***********************************************************************/
{

	char		buff[88];
	unsigned int	next;
	int		ii, plot, ier, ixold, iyold, lenb, icbnk;

/*---------------------------------------------------------------------*/

/*
 *	Make sure that plot file is open.  Put terminal in vector mode.
 */
	if  ( ! opnfil ) { 
	    psopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}
	psplot = G_TRUE; 
	plot = G_FALSE;

/*
 *	Set the color to black, if the color scheme type is monochrome.
 *	Except, keep the background color as 0.
 */
	if  ( kctype == 0 )
	{
	    if  ( ( mcolr != 1 ) && ( mcolr != 0 ) )
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
 *	Set line width if it has changed.
 */
	if  ( iawdth != irwdth )  {  
	    sprintf ( buff, " %4d LW\n", irwdth );
	    lenb = strlen ( buff );
	    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
	    iawdth = irwdth;
	}

/*
 *	Initialize buffer and counter.
 */
	buff[0] = '\0';

/*
 *	Encode each point into the buffer.
 */
	sprintf ( buff, " %5d %5d M", ix[0], iy[0] );
	next  = strlen ( buff );
	ixold = ix[0];
	iyold = iy[0];

	for ( ii = 0; ii < *np; ii++ ) {
	    if ( ( ix[ii] == ixold ) && ( iy[ii] == iyold ) )  {
/*
 *		Do nothing.
 */
		continue;
	    }
	    else  {
		ixold = ix[ii];
		iyold = iy[ii];
		sprintf ( buff+next, " %5d %5d L", ixold, iyold );	
		next += 14;
		if ( next >= 80 ) {
		    sprintf ( buff+next, "\n" );
		    next++;
		    lenb = strlen ( buff );
		    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
		    next = 0;
		    buff[0] = '\0';
		}
		plot = G_TRUE;
	    }
	}

/*
 *	Output last buffer.
 */
	strcpy ( buff+next, " stroke\n" );
	next += 8;
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

/*
 *	Call HDOTS if this is a dot.
 */
	if  ( ! plot )  pdots ( &ix[0], &iy[0], &irwdth, iret );

}
