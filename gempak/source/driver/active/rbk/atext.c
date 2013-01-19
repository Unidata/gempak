#include "ardcmn.h"

#define	LENHDR    	5
#define ONEFOUR         6

void atext ( float *xr, float *yr, char *cchar, int *lens, int *ixoff, 
		int *iyoff, float *rotat, int *ispanx, int *ispany, 
		int *icleft, int *icrght, int *icbot, int *ictop, int *iret )
/************************************************************************
 * atext	                                                        *
 *                                                                      *
 * This subroutine draws hardware text to the RBK file.                 *
 *                                                                      *
 * atext ( xr, yr, cchar, lens, ixoff, iyoff, rotat                     *
 *	   ispanx, ispany, icleft, icrght, icbot, ictop, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*xr		float		X coordinate                    *
 *	*yr		float		Y coordinate                    *
 *	*cchar		char		Text				*
 *	*lens		int		Length of text			*
 *	*ixoff		int		X offset			*
 *	*iyoff		int		Y offset			*
 *	*rotat		float		Rotation angle			*
 *	*ispanx		int		Direction of increasing x	*
 *	*ispany		int		Direction of increasing y	*
 *	*icleft		int		Left clipping bound		*
 *	*icrght		int		Right clipping bound		*
 *	*icbot		int		Bottom clipping bound	 	*
 *	*ictop		int 		Top clipping bound		*
 *									*
 * Output parameters:                                                   *
 *	*iret		int		Return code			*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		 9/98	Modified from UTEXT             	*
 * S. Jacobs/NCEP	 8/99	Added check for group type for med rng	*
 * A. Hardy/GSC          5/00   Font size is no longer hard coded	*
 ***********************************************************************/
{

	int		i, k, m, ix, iy, jx, jxoend, nx, ny, ixoff2;
        int             length, ier;
	unsigned char	txthdr[11], tchar[5000], curchr;
	unsigned char	hdr[ONEFOUR];
	float		xo, x;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Check for trying to draw something using the background
 *	color and return.
 */
	if  ( kcolr == 101 )  return;

/*
 *	If the file is not open, then open it first.
 */
	if  ( ! opnfil )  {
	    aopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	Set the location for plotting.
 */
	ix = G_NINT ( *xr );
	iy = G_NINT ( *yr );

/*
 *	Check for group type 11 for the medium range products.
 */
	if  ( kgtyp == 11 )  {
	    if  ( nchr == 0 )  {
		mxx = ix;
		myy = iy;
	    }
	    mlen[nchr] = *lens;
	    cst_ncpy ( mrchr[nchr], cchar, mlen[nchr], &ier );
	    nchr++;
	    return;
	}

/*
 *      Setting the plot parameters block: mode 1 submode 4.
 *          hdr[0] --> 0x40 is the field flag
 *          hdr[1] --> 0x03 is the length of the block
 *          hdr[2] --> 0x01 is the mode of the block
 *          hdr[3] --> 0x04 is the submode of the block
 *          hdr[4] --> Zoom Disable flag (bit 7 from high order bit)
 *                     Zoom Threshold (bits 6-0)
 *          hdr[5] --> Zoom Factor
 */
        hdr[0] = 0x40;
        hdr[1] = 0x03;
        hdr[2] = 0x01;
        hdr[3] = 0x04;
        hdr[4] = 0x00;
        hdr[5] = 0x00;

/*
 *      Write the plot parameter information to the buffer.
 */
        awrbuf ( hdr, ONEFOUR, iret );

/*
 *	Convert the text justification to offsets.
 */
	if  ( kjust == 1 )  {
	    ixoff2 = *ixoff;
	}
	else if  ( kjust == 3 )  {
	    ixoff2 = *ixoff - ( 2 * *lens ) + 2;
	}
	else  {
	    ixoff2 = *ixoff - *lens + 1;
	}

/*
 *	Compute the offsets for clipping the text.
 */
	xo = (float)ixoff2 * txszx * asize;
	x  = *xr + xo * *ispanx;
	jx = G_NINT ( x );

	jxoend = G_NINT ( jx + *lens * (txszx/2.) );

/*
 *	Check to see if the start point is outside the clipping window.
 *	Then the end point is checked to avoid wrapping to the next
 *	line.  Rotation is not taken into account.
 */
	if  ( ( *ispanx * ( ix - *icleft ) < 0 )  ||
	      ( *ispanx * ( ix - *icrght ) > 0 )  ||
	      ( *ispany * ( iy - *icbot  ) < 0 )  ||
	      ( *ispany * ( iy - *ictop  ) > 0 )  ||
	      ( *ispanx * ( jxoend - *icrght ) > 0 ) )  return;

/*
 *	Convert the GEMPAK offsets to AFOS left, right, up and down,
 *	cursor movements.
 */
	m  = 0;
	nx = ixoff2 * *ispanx;
	k = ( ( G_ABS(nx) ) + 1 ) / 2;
	if  ( nx < 0 )  {
/*
 *	    Back space
 */
	    curchr = 0x08;
	}
	else {
/*
 *	    Forward space
 */
	    curchr = 0x09;
	}
/*
 *	Add proper number of back or forward spaces.
 */
	for ( i = 0; i < k; i++, m++ )  {
	    tchar[m] = curchr;
	}

	ny = *iyoff * *ispany;
	k = ( ( G_ABS(ny) ) + 1 ) / 2;
	if  ( ny < 0 )  {
/*
 *	    Down space
 */
	    curchr = 0x0a;
	}
	else {
/*
 *	    Up space
 */
	    curchr = 0x0b;
	}
/*
 *	Add proper number of down or up spaces.
 */
	for ( i = 0; i < k; i++, m++ )  {
	    tchar[m] = curchr;
	}

/*
 *	Add the text string.
 */
	for ( i = 0; i < *lens; i++, m++ )  {
	    tchar[m] = cchar[i];
	}

/*
 *	If the length is odd, add a blank to the end.
 */
	if  ( !(m % 2) )  {
	    tchar[m] = ' ';
	    m++;
	}

	tchar[m] = CHNULL;

/*
 *	Assign values to the text header in the following format:
 *	  txthdr[0] -->  0x40 is the field flag
 *	  txthdr[1] -->  the length of the block.
 *	  txthdr[2] -->  0x05 is the mode of the block.
 *	  txthdr[3] -->  0x01 is the submode of the block.
 *	  txthdr[4] -->  The first 8 bits of the icoord (ix) value
 *	  txthdr[5] -->  The last 8 bits of the icoord value.
 *	  txthdr[6] -->  The first 8 bits of the jcoord (iy) value
 *	  txthdr[7] -->  The last 8 bits of the jcoord value.
 *	  txthdr[8] -->  The x offset
 *	  txthdr[9] -->  The y offset
 *	  txthdr[10] -->  The block/reverse block mode/char. size
 */
	txthdr[0] = 0x40;
	txthdr[1] = (LENHDR + (m/2) + 1 ) & BMASK;
	txthdr[2] = 0x05;
	txthdr[3] = 0x01;
   	txthdr[4] = ( ix >> 8 ) & BMASK;
	txthdr[5] = ix & BMASK;
   	txthdr[6] = ( iy >> 8 ) & BMASK; 
	txthdr[7] = iy & BMASK;
        txthdr[8] = nx & BMASK;
        txthdr[9] = ny & BMASK;
        txthdr[10] = ( 0x80  + nfntsz ) & BMASK;
        length = (LENHDR*2) + 1;

	awrbuf ( txthdr, length, iret );
	awrbuf ( tchar, m, iret );

}
