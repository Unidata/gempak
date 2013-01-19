#include "utfcmn.h"

#define	 LENHDR    	6

void utext ( float *xr, float *yr, char *cchar, int *lens, 
		int *ixoff, int *iyoff, float *rotat, int *ispanx, 
		int *ispany, int *icleft, int *icrght, int *icbot, 
		int *ictop, int *iret )
/************************************************************************
 * utext	                                                        *
 *                                                                      *
 * This subroutine draws hardware text to the UTF file.                 *
 *                                                                      *
 * utext ( xr, yr, cchar, lens, ixoff, iyoff, rotat                     *
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
 * E. Safford/GSC	11/96	Initial coding                  	*
 * S. Jacobs/NCEP	 7/97	Removed offset text, applied offsets	*
 *				before writing to the file		*
 * S. Jacobs/NCEP	 8/97	Changed to create offsets using up,	*
 *				down, left and right cursor movements	*
 * S. Jacobs/NCEP	 9/97	Added computation of justification	*
 * S. Jacobs/NCEP	12/97	Added check for no clipping flag	*
 * S. Jacobs/NCEP	 1/98	Added adjustment for starting location	*
 * S. Jacobs/NCEP	 7/98	Removed no clip flag; Clean up		*
 * S. Jacobs/NCEP	 3/99	Added check for group type for med rng	*
 ***********************************************************************/
{

	int		i, k, m, ix, iy, jx, jxoend, nx, ny, ixoff2, ier;
	unsigned char	txthdr[LENHDR], tchar[MXAFOS], curchr;
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
	    uopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*	Assign values to the text header in the following format:
 *	  txthdr[0] -->  The mode.  The mode will be 0xc5 if no offset
 *			    is used, or 0xc8 if there is an offset.
 *	  txthdr[1] -->  The i,j coords flag in bit 0 (high order),
 *			    the char block mode in bit 1,
 *			    the reverse block mode in bit 2,
 *			    the zoom threshold in bits 3 and 4, 
 *			    the zoom factor in bits 5-7
 *			  These flags are all 0.
 *	  txthdr[2] -->  The char size multiplier in bits 0-1, and
 *			    the first 6 bits of the icoord (ix) value 
 *	  txthdr[3] -->  The remaining 8 bits of the icoord value.
 *	  txthdr[4] -->  The first 8 bits of the jcoord (iy) value
 *	  txthdr[5] -->  The last 8 bits of the jcoord value.
 *
 *	Note that the iccord is allocated 14 total bits, while the
 *	  jcoord is allocated 16 bits.  This is the correct AFOS format.
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

	txthdr[0] = 0xc5;
	txthdr[1] = 0;
	txthdr[2] = ( (nfntsz << 6) & 0xc0 ) | ( (ix >> 8) & 0x3f );
	txthdr[3] = ix & BMASK;
	txthdr[4] = ( iy >> 8 ) & BMASK;
	txthdr[5] = iy & BMASK;

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
	if  ( m % 2 )  {
	    tchar[m] = ' ';
	    m++;
	}

	tchar[m] = CHNULL;

/*
 *	Check to see if this text string fits in the buffer.  If not
 *	terminate with an error condition of G_NAFSMX.
 */
        if  ( ( numout + m + LENHDR ) >= MXAFOS )  {
            *iret = G_NAFSMX;
            return;
        }

/*
 * 	Write the header and the string to the buffer.
 */
	uwrbuf ( txthdr, LENHDR, iret );
	uwrbuf ( tchar, m, iret );

}
