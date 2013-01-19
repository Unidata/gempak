#define PGLOBAL
#define XWPCMN_GLOBAL
#include "pscmn.h"
#include "color.h"

void pinita ( int *iunit, char *fname, int *lenf, int *itype, 
		float *xsz, float *ysz, int *ileft, int *ibot, 
		int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * PINITA								*
 *									*
 * This subroutine is called to initialize a new device driver.		*
 *									*
 * PINITA ( IUNIT, FNAME, LENF, ITYPE, XSZ, YSZ, ILEFT, IBOT, IRIGHT,	*
 *	    ITOP, NUMCLR, IRET )					*
 *									*
 * Input parameters:							*
 *	iunit		int*		Output type (Used for XW only)	*
 *	fname		char* 		Output file name		*
 *	lenf		int*		File name length		*
 *	itype		int*		Pixel type			*
 *					  0 = Monochrome		*
 *					  1 = Grayscale			*
 *					  2 = Color			*
 *	xsz		float*		Xsize				*
 *	ysz		float*		Ysize				*
 *									*
 * Output parameters:							*
 *	ileft		int*		Left device coordinate		*
 *	ibot		int*		Bottom device coordinate	*
 *	iright		int*		Right device coordinate		*
 *	itop		int*		Top device coordinate		*
 *	numclr		int*		Max number of colors for device	*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 * E. Wehner/EAI	 2/96	Remove hardcoded parameters, added	*
 *				color tables				*
 * S. Jacobs/NCEP	 4/96	Added ileft,ibot,iright,itop,nncolr	*
 *				to calling sequence; added calculation	*
 *				of paper and device size		*
 * S. Jacobs/NCEP	 5/96	Removed PIX_BW, PIX_GS and PIX_COLOR;	*
 *				Added new global variables for queries	*
 * S. Jacobs/NCEP	 5/96	Removed coltbl.psb			*
 * S. Jacobs/NCEP	 1/97	Changed itype=0 to use colors for fill	*
 * S. Jacobs/NCEP	 3/97	Changed values of txszx and txszy	*
 * S. Wang/GSC		11/97   Take out graphic color initialization	*
 * S. Jacobs/NCEP	 7/98	Changed values of txszx and txszy	*
 * S. Jacobs/NCEP	 8/98	Changed plot dim to leave 1/4 in margin	*
 * S. Jacobs/NCEP	 9/98	Added large paper 24x36 inches		*
 * S. Jacobs/NCEP	 9/98	Changed init of mcolr from 0 to 1	*
 * S. Jacobs/NCEP	 8/99	Changed 24x36 to 20x32; Added 32x42	*
 * R. Tian/SAIC		05/02	Added clrbank[] entry for fax		*
 ***********************************************************************/
{
	int	ier, one = 1;
	char	tmpfil[133];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *      Set the global output and color scheme types
 */
	kctype = *itype;
	kunit  = *iunit;

/*
 *	Set txszx, txszy (hardware text sizes) here if hardware text is
 *	used.
 *	txszx = bscalc * 7
 *	txszy = bscalc * 9
 */
	txszx  = 196.;
	txszy  = 252.;
	irfont = 1;
	isfont = 0;
	txsizr = 1.;
	txsizs = 0.;
	nfontu = 0;
	mcolr  = 1;

/*
 *	Set the paper size in inches and in pixels.
 *
 *	xsize = x paper size in inches
 *	ysize = y paper size in inches
 *
 *	landscape = landscape/portrait orientation flag
 *
 *	ileft  = left device coordinate
 *	ibot   = bottom device coordinate
 *	iright = right device coordinate
 *	itop   = top device coordinate
 *
 *	Device coords are based on 72 pixels/inch and a scaling
 *	factor of 32 multiplied by the plot area in inches.
 */

	*ileft  = 1;
	*ibot   = 1;

	if  ( ( *xsz >= *ysz ) || ( *xsz < 0 ) || ( *ysz < 0 ) )
	{
	    if  ( ( *xsz >= 33.0 ) || ( *ysz >= 21.0 ) )
	    {
		xsize = 42.5;
		ysize = 32.5;
		strcpy ( pprnam, "/Roll2" );
		*iright = xsize * 72 * 32;
		*itop   = ysize * 72 * 32;
	    }
	    else if  ( ( *xsz >= 18.0 ) || ( *ysz >= 12.0 ) )
	    {
		xsize = 32.5;
		ysize = 20.5;
		strcpy ( pprnam, "/Roll1" );
		*iright = xsize * 72 * 32;
		*itop   = ysize * 72 * 32;
	    }
	    else if  ( ( *xsz >= 11.5 ) || ( *ysz >= 9.0 ) )
	    {
		xsize = 17.0;
		ysize = 11.0;
		strcpy ( pprnam, "/Tabloid" );
		*iright = ( xsize - 0.5 ) * 72 * 32;
		*itop   = ( ysize - 0.5 ) * 72 * 32;
	    }
	    else
	    {
		xsize = 11.0;
		ysize =  8.5;
		strcpy ( pprnam, "/Letter" );
		*iright = ( xsize - 0.5 ) * 72 * 32;
		*itop   = ( ysize - 0.5 ) * 72 * 32;
	    }
	}
	else
	{
	    if  ( ( *xsz >= 21.0 ) || ( *ysz >= 33.0 ) )
	    {
		xsize = 32.5;
		ysize = 42.5;
		strcpy ( pprnam, "/Roll2" );
		*iright = xsize * 72 * 32;
		*itop   = ysize * 72 * 32;
	    }
	    else if  ( ( *xsz >= 12.0 ) || ( *ysz >= 18.0 ) )
	    {
		xsize = 20.5;
		ysize = 32.5;
		strcpy ( pprnam, "/Roll1" );
		*iright = xsize * 72 * 32;
		*itop   = ysize * 72 * 32;
	    }
	    else if  ( ( *xsz >= 9.0 ) || ( *ysz >= 11.5 ) )
	    {
		xsize = 11.0;
		ysize = 17.0;
		strcpy ( pprnam, "/Tabloid" );
		*iright = ( xsize - 0.5 ) * 72 * 32;
		*itop   = ( ysize - 0.5 ) * 72 * 32;
	    }
	    else
	    {
		xsize =  8.5;
		ysize = 11.0;
		strcpy ( pprnam, "/Letter" );
		*iright = ( xsize - 0.5 ) * 72 * 32;
		*itop   = ( ysize - 0.5 ) * 72 * 32;
	    }
	}

	if  ( xsize > ysize )
	    landscape = G_TRUE;
	else
	    landscape = G_FALSE;

/*
 *	Set file to initially closed.
 */
	opnfil = G_FALSE;

/*
 *	Save the file name
 */
	strncpy ( tmpfil, fname, *lenf );
	tmpfil[*lenf] = CHNULL;

/*
 *	If the new file name is not empty, set the current file name.
 */
	if  ( tmpfil[0] != CHNULL )
	{
	    strcpy ( filnam, tmpfil );
	    *iret = G_NEWWIN;
	}

/*
 *	Initialize line width.
 */
	iawdth = 0;
	irwdth = 0;

	pslwid ( &one, &ier );

/*
 *	Set the number of colors to be returned to DEVCHR.
 */
	if ( *itype == 1 )
	    nncolr = 20;
	else
	    nncolr = 32;

	*numclr = nncolr;

/*
 *	Initialize the number of satellite, radar and fax colors.
 */
        clrbank[1].ncolr = 0;
        clrbank[2].ncolr = 0;
        clrbank[3].ncolr = 0;
}
