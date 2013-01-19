#include "nccmn.h"

void mtext ( float *xr, float *yr, char textstr[], int *ilen, 
		int *ixoff, int *iyoff, float *rotat, int *ispanx, 
		int *ispany, int *icleft, int *icrght, int *icbot, 
		int *ictop, int *iret )
/************************************************************************
 * mtext								*
 *									*
 * This subroutine writes a text string to the metafile.		*
 *									*
 * mtext ( xr, yr, textstr, ilen, ixoff, iyoff, rotat,			*
 *	   ispanx, ispany, icleft, icrght, icbot, ictop, iret )		*
 *									*
 * Input parameters:							*
 *	*xr		float		X-coord 			*
 *	*yr		float		Y-coord 			*
 *	textstr[]	char		Text string			*
 *	*ilen 		int		Length of text string		*
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
 *	*iret		int		Return code 			*
 **									*
 * Log:									*
 * C. Lin/EAI		 1/93						*
 * C. Lin/EAI		 4/93	Add Internal Buffer			*
 * A. Chang/EAI          1/94   Modified to buffered I/O		*
 * A. Chang/EAI          5/94   Eliminated scaling code			*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * C. Lin/EAI		10/94	Add text alignment			*
 * S. Jacobs/NCEP	10/94	Removed setting txalgn_req		*
 * S. Jacobs/NCEP	 7/98	Added alignment and bounds checks	*
 * S. Jacobs/NCEP	 7/98	Modified calling sequence		*
 * S. Jacobs/NCEP	12/98	Changed numbyte from short to int	*
 * T. Piper/GSC		12/99	Slight change to the xo calculation	*
 ***********************************************************************/
{
	short		buffer[4];
	short		colbuf[2];
	short		fntbuf[2];
	short		sizbuf[2];
	short		nbyte;
	int		numbyte;
	unsigned char	*txtbuf;
	int		ibyt, ier, istrt, iend, jx, jy, jxoend, jxost;
	float		xo, yo, x, y;

/*---------------------------------------------------------------------*/
	textstr[*ilen] = '\0';
	*iret = G_NORMAL;

/*
 *	Get the justification and apply an adjustment to the location.
 *	Get the end points for the different justifications.
 */
	if  ( txalgn_req == 1 )  {
	    istrt = 0;
	    iend  = *ilen * tsize * txszx;
	}
	else if  ( txalgn_req == 3 ) {
	    istrt = -(*ilen) * tsize * txszx;
	    iend  = 0;
	}
	else {
	    istrt = -(*ilen/2) * tsize * txszx;
	    iend  =   *ilen/2  * tsize * txszx;
	}

/*
 *	Convert the text location to integers.
 */
	xo = ( ( *ixoff - 1.0 ) / 2.0 ) * txszx * tsize;
	x  = *xr + xo * *ispanx;
	jx = G_NINT ( x );

	yo = ( ( *iyoff - 1.0 ) / 2.0 ) * txszy * tsize;
	y  = *yr + yo * *ispany;
	jy = G_NINT ( y );

/*
 *	Check to see if the start point is outside the clipping
 *	window.  Then the end point is checked.  Rotation is not
 *	taken into account.
 */
	jxost  = jx + istrt;
	jxoend = jx + iend;
	if  ( ( *ispanx * ( jx - *icleft ) < 0 )  ||
	      ( *ispanx * ( jx - *icrght ) > 0 )  ||
	      ( *ispany * ( jy - *icbot  ) < 0 )  ||
	      ( *ispany * ( jy - *ictop  ) > 0 )  ||
	      ( *ispanx * ( jxost  - *icleft ) < 0 ) ||
	      ( *ispanx * ( jxoend - *icrght ) > 0 ) )  return;

/*
 *	Check for too many points.
 */
	numbyte = (*ilen) + 2 * NUM_BTSZ;

	if ( numbyte > MAX_TWOBT ) {
	    *iret = G_NMDATA;
	    return;
	}

/*
 *	Open the file, if necessary.
 */
	if ( ! opnfil ) {
	    mopen ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = ier;
		return;
	    }
	}

/*
 *      Set line color.
 */
	if ( lcolor_req != lcolor_set ) {
	    lcolor_set = lcolor_req;

	    nbyte = 1 * NUM_BTSZ;
	    colbuf[0] = LINE_COLOR | nbyte;
	    colbuf[1] = (short) lcolor_set;
	    colbuf[1] <<= 8;

	    fwrite ( colbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Set the text font.
 */
	if ( txfont_req != txfont_set ) {
	    txfont_set = txfont_req;

	    nbyte = 1 * NUM_BTSZ;
	    fntbuf[0] = TEXT_FONT_INX | nbyte;
	    fntbuf[1] = (short) txfont_set;

	    fwrite ( fntbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Set the text size.
 */
	if ( txsize_req != txsize_set ) {
	    txsize_set = txsize_req;

	    nbyte = 1 * NUM_BTSZ;
	    sizbuf[0] = CHAR_HEIGHT | nbyte;
	    sizbuf[1] = (short) txsize_set;

	    fwrite ( sizbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Set the text alignment.
 */
	if ( txalgn_req != txalgn_set ) {
	    txalgn_set = txalgn_req;

	    nbyte = 1 * NUM_BTSZ;
	    sizbuf[0] = TEXT_ALIGN | nbyte;
	    sizbuf[1] = (short) txalgn_set;

	    fwrite ( sizbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Draw the text.
 */
	if ( numbyte < 31 ) {
	    buffer[0] = TEXT | numbyte;
	    ibyt = 1;
	}
	else {
	    buffer[0] = TEXT | LONG_FORM;
	    buffer[1] = numbyte;
	    ibyt = 2;
	}

	buffer[ibyt]   = (short) (jx);
	buffer[ibyt+1] = (short) (jy);

	fwrite ( buffer, NUM_BTSZ, ibyt+2, meta_fp );

	byte_count += NUM_BTSZ * ( ibyt + 2 );

/*
 *	Allocate the temporary text buffer, and write the text.
 */
	ibyt = (*ilen);
	txtbuf = (unsigned char *) malloc ( ibyt + 1 );

	strncpy ( (char *)txtbuf, textstr, ibyt );

	if ( ibyt%2 != 0 ) {
	    txtbuf[ibyt] = '\0';
	    ibyt++;
	}

	fwrite ( txtbuf, 1, ibyt, meta_fp );
	byte_count += ibyt;

	free ( txtbuf );

}
