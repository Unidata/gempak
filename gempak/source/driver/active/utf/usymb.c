#include "utfcmn.h"

#define	 LENHDR    	6

void usymb ( int *isym, int *np, float code[], float x[], float y[], 
		int ixoff[], int iyoff[], int *ispanx, int *ispany, 
		int *icleft, int *icrght, int *icbot, int *ictop, int *iret )
/************************************************************************
 * usymb	                                                        *
 *                                                                      *
 * This subroutine draws symbols to the UTF output file. For the UTF	*
 * format, symbols are encoded as text with a special flag before and	*
 * after the symbol.							*
 *                                                                      *
 * usymb ( isym, np, code, x, y, ixoff, iyoff, ispanx, ispany, icleft,  *
 *					   icrght, icbot, ictop, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*isym		int		Symbol category			*
 *					  1 = Weather symbols		*
 *					  2 = Cloud type symbols	*
 *					  3 = Icing symbols		*
 *					  4 = Pressure tendency symbols	*
 *					  5 = Past weather symbols	*
 *					  6 = Sky cover symbols		*
 *					  7 = Special symbols		*
 *					  8 = Turbulence symbols	*
 *					  9 = Marker			*
 *	*np		int		Number of points		*
 *	code [np]	float		Symbol codes			*
 *	x [np]		float		X coordinates			*
 *	y [np]		float		Y coordinates			*
 *	ixoff [np]	int		X offsets			*
 *	iyoff [np]	int		Y offsets			*
 *	*ispanx		int		Direction of increasing x	*
 *	*ispany		int		Direction of increasing y	*
 *	*icleft		int		Left clipping bound		*
 *	*icrght		int		Right clipping bound		*
 *	*icbot		int		Bottom clipping bound		*
 *	*ictop		int		Top clipping bound		*
 *									*
 * Output parameters:                                                   *
 *	*iret		int		Return code			*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 8/97	Copied from utext			*
 * S. Jacobs/NCEP	 9/97	Changed wsym for combination symbols	*
 * G. Krueger/EAI	 3/98	Added 2 superstructure icing symbols	*
 * S. Jacobs/NCEP	 7/98	Modified bounds checking		*
 * G. Krueger/EAI	 7/98	Added SLASH symbol			*
 * G. Krueger/EAI	 8/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
 * G. Krueger/EAI	10/98	Added fire special symbol		*
 * G. Krueger/EAI	 1/99	Add X and LowX specials			*
 * S. Jacobs/NCEP	 3/99	Added markers				*
 * S. Jacobs/NCEP	 3/99	Added check for group type for med rng	*
 * G. Krueger/EAI	 8/99	N & SH trop storm specials		*
 ***********************************************************************/
{

	int		i, j, k, m, n, ix, iy, nx, ny, ic;
	unsigned char	txthdr[LENHDR], tchar[MXAFOS], curchr;


	unsigned char	wsym[][9]={{0x00}, {0x00}, {0x00}, {0x00},
				   {0x1c}, {0x1d}, {0x1e}, {0x1f},
				   {0x20}, {0x21}, {0x22}, {0x23},
				   {0x24}, {0x25}, {0x26}, {0x27},
				   {0x28}, {0x29}, {0x2a}, {0x2b},
				   {0x2c,0x31}, {0x2d,0x31},
				   {0x2e,0x31}, {0x2f,0x31},
				   {0x30,0x31},
				   {0x36,0x0b,0x08,0x2d,0x0a,0x31},
				   {0x36,0x0b,0x08,0x2e,0x0a,0x31},
				   {0x36,0x0b,0x08,0x4d,0x0a,0x31},
				   {0x35,0x31}, {0x29,0x31},
				   {0x37,0x09,0x39}, {0x37},
				   {0x39,0x08,0x37},
				   {0x3a,0x09,0x39}, {0x3a},
				   {0x39,0x08,0x3a},
				   {0x3b}, {0x3c}, {0x3d}, {0x3e},
				   {0x48,0x53}, {0x3f},
				   {0x40,0x09,0x39}, {0x35,0x09,0x39},
				   {0x40}, {0x35}, {0x39,0x08,0x40},
				   {0x39,0x08,0x35}, {0x41}, {0x42},
				   {0x2c}, {0x2c,0x2c}, {0x43},
				   {0x08,0x2c,0x0b,0x2c,0x0a,0x2c},
				   {0x43,0x0b,0x08,0x2c},
				   {0x08,0x2c,0x43,0x2c},
				   {0x44}, {0x45}, {0x49},
				   {0x49,0x0b,0x08,0x2c},
				   {0x2d}, {0x2d,0x2d}, {0x4f},
				   {0x08,0x2d,0x0b,0x2d,0x0a,0x2d},
				   {0x4f,0x0b,0x08,0x2d},
				   {0x08,0x2d,0x4f,0x2d},
				   {0x46}, {0x47}, {0x2f},
				   {0x2f,0x0b,0x08,0x2e},
				   {0x2e}, {0x2e,0x2e}, {0x59},
				   {0x08,0x2e,0x0b,0x2e,0x0a,0x2e},
				   {0x59,0x0b,0x08,0x2e},
				   {0x08,0x2e,0x59,0x2e},
				   {0x4a}, {0x4b}, {0x4c}, {0x4d},
				   {0x36,0x0b,0x08,0x2d},
				   {0x4e,0x0b,0x08,0x2d},
				   {0x36,0x0b,0x08,0x4f},
				   {0x36,0x0b,0x08,0x2f},
				   {0x4e,0x0b,0x08,0x2f},
				   {0x36,0x0b,0x08,0x2e},
				   {0x4e,0x0b,0x08,0x2e},
				   {0x36,0x0b,0x08,0x4d},
				   {0x4e,0x0b,0x08,0x4d},
				   {0x36,0x0b,0x08,0x4d},
				   {0x4e,0x0b,0x08,0x4d},
				   {0x29,0x51}, {0x29,0x52},
				   {0x29,0x55}, {0x29,0x57},
				   {0x29,0x0b,0x08,0x2d},
				   {0x29,0x0b,0x08,0x4d},
				   {0x58,0x0b,0x08,0x2d},
				   {0x29,0x0b,0x08,0x37},
				   {0x58,0x0b,0x08,0x4d},
				   {0x00}, {0x00}, {0x00},
				   {0x29,0x54}, {0x29,0x56},
				   {0x29,0x0b,0x08,0x2e}, {0x00},
				   {0x58,0x0b,0x08,0x2e}, {0x00}, {0x00}
				  };

	unsigned char	ctyp[] = { 0x00, 0x66, 0x67, 0x68, 0x69,
				   0x6a, 0x6b, 0x6c, 0x6d, 0x6e,
				   0x00, 0x5d, 0x5e, 0x5f, 0x60,
				   0x61, 0x62, 0x63, 0x64, 0x65,
				   0x00, 0x6f, 0x70, 0x71, 0x72,
				   0x73, 0x74, 0x75, 0x76, 0x77 };

	unsigned char	icng[] = { 0x00, 0x00, 0x00, 0x7d, 0x00,
				   0x7e, 0x00, 0x00, 0x7f, 0x00,
				   0x00 };

	unsigned char	ptnd[] = { 0x16, 0x17, 0x00, 0x1a, 0x00,
				   0x18, 0x19, 0x00, 0x1b };

	unsigned char	pwth[] = { 0x00, 0x00, 0x00, 0x37, 0x35,
				   0x2c, 0x2d, 0x2e, 0x36, 0x29 };
				  

	unsigned char	skyc[] = { 0x0e, 0x0f, 0x10, 0x13, 0x14, 0x15,
				   0x01, 0x02, 0x03, 0x04, 0x05 };

	unsigned char	spcl[] = { 0x06,
				   0x00, 0x00, 0x00, 0x34, 0x50,
				   0x00, 0x00, 0x07, 0x07, 0x5a,
				   0x5b, 0x5a, 0x5b, 0x31, 0x33,
				   0x32, 0x39, 0x38, 0x51, 0x52,
				   0x54, 0x56, 0x55, 0x57, 0x78,
				   0x79, 0x78, 0x79, 0x5c, 0x30,
				   0x2f, 0x00, 0x00, 0x00, 0x00,
				   0x00, 0x00, 0x00, 0x00, 0x00 };

	unsigned char	turb[] = { 0x00, 0x00, 0x7a, 0x00, 0x7b,
				   0x00, 0x7c, 0x00, 0x00 };

	unsigned char	mark[] = { 0x00, 0x00, 0x0e, 0x34, 0x06,
				   0x00, 0x00, 0x00, 0x00, 0x00,
				   0x00, 0x00, 0x00, 0x00, 0x00,
				   0x00, 0x00, 0x03, 0x00, 0x00,
				   0x00, 0x07 };

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	If this is for group type 11, return.
 */
	if  ( kgtyp == 11 )  return;

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
 *
 *	Plot a symbols at all NP points.
 */
	for ( n = 0; n < *np; n++ )  {
	    ix = G_NINT ( x[n] );
	    iy = G_NINT ( y[n] );
	    txthdr[0] = 0xc5;
	    txthdr[1] = 0;
	    if  ( *isym == 9 )  {
		txthdr[2] = ( (0 << 6) & 0xc0 ) | ( (ix >> 8) & 0x3f );
	    }
	    else {
		txthdr[2] = ( (2 << 6) & 0xc0 ) | ( (ix >> 8) & 0x3f );
	    }
	    txthdr[3] = ix & BMASK;
	    txthdr[4] = ( iy >> 8 ) & BMASK;
	    txthdr[5] = iy & BMASK;

/*
 *	    Check to see if the start point is outside the clipping
 *	    window.
 */
	    if  ( ( *ispanx * ( ix - *icleft ) < 0 )  ||
		  ( *ispanx * ( ix - *icrght ) > 0 )  ||
		  ( *ispany * ( iy - *icbot  ) < 0 )  ||
		  ( *ispany * ( iy - *ictop  ) > 0 ) )  return;

/*
 *	    Convert the GEMPAK offsets to AFOS left, right, up and down,
 *	    cursor movements.
 */
	    m  = 0;
	    nx = ixoff[n] * *ispanx;
	    k = ( ( G_ABS(nx) ) + 1 ) / 2;
	    if  ( nx < 0 )  {
/*
 *	    	Back space
 */
		curchr = 0x08;
	    }
	    else {
/*
 *	    	Forward space
 */
		curchr = 0x09;
	    }
/*
 *	    Add the proper number of back or forward spaces.
 */
	    for ( i = 0; i < k; i++, m++ )  {
		tchar[m] = curchr;
	    }

	    ny = iyoff[n] * *ispany;
	    k = ( ( G_ABS(ny) ) + 1 ) / 2;
	    if  ( ny < 0 )  {
/*
 *	    	Down space
 */
		curchr = 0x0a;
	    }
	    else {
/*
 *	    	Up space
 */
		curchr = 0x0b;
	    }
/*
 *	    Add the proper number of down or up spaces.
 */
	    for ( i = 0; i < k; i++, m++ )  {
		tchar[m] = curchr;
	    }

/*
 *	    Create a text string for the symbol. Turn on the special
 *	    character mode (unless slash), add the symbol, then turn off
 *	    the special character mode (unless slash).
 */
	    ic = (int) code[n];

	    if ( *isym != 7 || ic != 31 ) {
		tchar[m] = 0x12;
		m++;
	    }

	    switch ( *isym )  {
		case 1:
		    if  ( 0 <= ic && ic <= 109 )  {
			if  ( wsym[ic] == 0x00 )  {
			    return;
			}
			else {
			    j = 0;
			    while ( wsym[ic][j] )  {
				tchar[m] = wsym[ic][j];
				m++;
				j++;
			    }
			}
		    }
		    break;
		case 2:
		    if  ( 0 <= ic && ic <= 29 )  {
			if  ( ctyp[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = ctyp[ic];
			    m++;
			}
		    }
		    break;
		case 3:
		    if  ( 0 <= ic && ic <= 10 )  {
			if  ( icng[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = icng[ic];
			    m++;
			}
		    }
		    break;
		case 4:
		    ic = ic / 1000;
		    if  ( 0 <= ic && ic <= 8 )  {
			if  ( ptnd[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = ptnd[ic];
			    m++;
			}
		    }
		    break;
		case 5:
		    if  ( 0 <= ic && ic <= 9 ) {
			if  ( pwth[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = pwth[ic];
			    m++;
			}
		    }
		    break;
		case 6:
		    if  ( 0 <= ic && ic <= 10 ) {
			if  ( skyc[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = skyc[ic];
			    m++;
			}
		    }
		    break;
		case 7:
		    if  ( 0 <= ic && ic <= 40 ) {
			if  ( spcl[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = spcl[ic];
			    m++;
			}
		    }
		    break;
		case 8:
		    if  ( 0 <= ic && ic <= 8 ) {
			if  ( turb[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = turb[ic];
			    m++;
			}
		    }
		    break;
		case 9:
		    if  ( 0 <= ic && ic <= 21 ) {
			if  ( mark[ic] == 0x00 )  {
			    return;
			}
			else {
			    tchar[m] = mark[ic];
			    m++;
			}
		    }
		    break;
	    }

	    if ( *isym != 7 || ic != 31 ) {
		tchar[m] = 0x11;
		m++;
	    }

/*
 *	    If the length is odd, add a blank to the end.
 */
	    if  ( m % 2 )  {
		tchar[m] = ' ';
		m++;
	    }

	    tchar[m] = CHNULL;

/*
 *	    Check to see if this text string fits in the buffer. If not
 *	    terminate with an error condition of G_NAFSMX.
 */
	    if  ( ( numout + m + LENHDR ) >= MXAFOS )  {
		*iret = G_NAFSMX;
		return;
	    }

/*
 * 	    Write the header and the string to the buffer.
 */
	    uwrbuf ( txthdr, LENHDR, iret );
	    uwrbuf ( tchar, m, iret );

	}

}
