#include "pscmn.h"

void pfill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * PFILL								*
 *									*
 * This subroutine draws a filled polygon to the PostScript file.	*
 *									*
 * PFILL ( NP, IX, IY, IRET )						*
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
 * A. Chang/EAI		 2/94						*
 * M. Linda/GSC		 2/97	Changed buff[88] to buff[LLMXPT]	*
 * C. Lin/EAI            2/98   Changed calling sequence to pscolr      *
 * S. Jacobs/NCEP	 3/98	Added selection of fill pattern		*
 * J. Wu/SAICP	 	 6/05	reduce buff size from LLMXPT to 128	*
 ***********************************************************************/
{
	char		buff[128], fbuff[256], xnm[20], cnm[20];
	unsigned int	icbnk, next;
	int		ii, ixold, iyold, mp, lenb, lenf, ier,
			icred, icgreen, icblue;
	float		cred, cgreen, cblue;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;
/*
 *	Make sure that plot file is open.
 */
	if  ( ! opnfil ) {
	    psopen ( iret );
	    if ( *iret != G_NORMAL ) return;
	}
	psplot = G_TRUE;

/*
 *	Set color components if they have changed.
 */
	if ( resetc ) {
	     icbnk = 0;
	     pscolr ( (int *)&icbnk, &mcolr, iret );
	}

/*
 *	Set the selected pattern for the fill. First get the RGB
 *	components for the current color. Only add the pattern fill
 *	commands if the user selected a pattern other than solid.
 */
	if  ( kfillt > 1 )  {
	    cqcomp ( &mcolr, cnm, &icred, &icgreen, &icblue, xnm, &ier );
	    cred   = (float) icred / 255.0;
	    cgreen = (float) icgreen / 255.0;
	    cblue  = (float) icblue / 255.0;

	    sprintf ( fbuff, " [/Pattern /DeviceRGB] setcolorspace\n" );
	    sprintf ( fbuff+37,
		      " %5.3f %5.3f %5.3f pattern%d setcolor\n",
		      cred, cgreen, cblue, kfillt );

	    lenf = strlen ( fbuff );
	    cfl_writ ( flun, lenf, (unsigned char *)fbuff, &ier );
	}

/*
 *	Initialize buffer and counter.
 */
	buff[0] = '\0';
	mp      = 1;

/*
 *	Encode each point into the buffer.
 */
	sprintf ( buff, "N %5d %5d M ", ix[0], iy[0] );
	next  = 16;
	ixold = ix [0];
	iyold = iy [0];

	for ( ii = 1; ii < *np; ii++ ) {
	    if ( ( ix[ii] == ixold ) && ( iy[ii] == iyold ) ) {

/*
 *		Do nothing.
 */
		continue;
	    }
	    else {
		mp++;
		ixold = ix[ii];
		iyold = iy[ii];
		sprintf ( buff+next, " %5d %5d L", ixold, iyold );
		next += 14;
		if  ( next > 80 ) {
		    sprintf ( buff+next,"\n" );
		    lenb = strlen ( buff );
		    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
		    next = 0;
		    buff[0] = '\0';
		}
	    }
	}

/*
 *	Add fill command and output last buffer.
 */
	if  ( mp > 2 ) {
	    sprintf ( buff+next, " FL\n" );
	    next += 4;
	    lenb = strlen ( buff );
	    cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );
	}

/*
 *	If a fill pattern was set, reset to solid.
 */
	if  ( kfillt > 1 )  {
	    sprintf ( fbuff, " %5.3f %5.3f %5.3f RGB\n",
		      cred, cgreen, cblue );
	    lenf = strlen ( fbuff );
	    cfl_writ ( flun, lenf, (unsigned char *)fbuff, &ier );
	}

}
