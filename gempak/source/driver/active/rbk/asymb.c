#include "ardcmn.h"

#define	 LENHDR    	5
#define	 HEADER        10	

void asymb ( int *isym, int *np, float code[], float x[], float y[], 
		int ixoff[], int iyoff[], int *ispanx, int *ispany, 
		int *icleft, int *icrght, int *icbot, int *ictop, int *iret )
/************************************************************************
 * asymb								*
 *									*
 * This subroutine draws symbols to the UTF output file. For the UTF	*
 * format, symbols are encoded as text with a special flag before and	*
 * after the symbol.							*
 *									*
 * asymb ( isym, np, code, x, y, ixoff, iyoff, ispanx, ispany, icleft,  *
 *					   icrght, icbot, ictop, iret )	*
 *									*
 * Input parameters:							*
 *	*isym		int		Symbol category 		*
 *					  1 = Weather symbols		*
 *					  2 = Cloud type symbols	*
 *					  3 = Icing symbols		*
 *					  4 = Pressure tendency symbols *
 *					  5 = Past weather symbols	*
 *					  6 = Sky cover symbols 	*
 *					  7 = Special symbols		*
 *					  8 = Turbulence symbols	*
 *					  9 = Markers			*
 *	*np		int		Number of points		*
 *	code [np]	float		Symbol codes			*
 *	x [np]		float		X coordinates			*
 *	y [np]		float		Y coordinates			*
 *	ixoff [np]	int		X offsets			*
 *	iyoff [np]	int		Y offsets			*
 *	*ispanx 	int		Direction of increasing x	*
 *	*ispany 	int		Direction of increasing y	*
 *	*icleft 	int		Left clipping bound		*
 *	*icrght 	int		Right clipping bound		*
 *	*icbot		int		Bottom clipping bound		*
 *	*ictop		int		Top clipping bound		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log: 								*
 * A. Hardy/GSC 	 8/98	Modified from utf's USYMB		*
 * G. Krueger/EAI	10/98	Added fire special symbol		*
 * S. Jacobs/NCEP	12/98	Fixed typo				*
 * G. Krueger/EAI	 1/99	Add X and LowX specials			*
 * S. Jacobs/NCEP	 3/99	Added markers				*
 * G. Krueger/EAI	 8/99	N & SH trop storm specials		*
 * S. Jacobs/NCEP	 8/99	Added check for group type for med rng	*
 ***********************************************************************/
{

	int		i, j, k, m, n, ix, iy, nx, ny, ic;
	unsigned char	txthdr[HEADER], tchar[5000], curchr;


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
	    aopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

	for ( n = 0; n < *np; n++ )  {
	    ix = G_NINT ( x[n] );
	    iy = G_NINT ( y[n] );

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

	}
/*
 *      Setting the plot parameters block: mode 1 submode 4.
 *          hdr[0] --> 0x40 is the field flag
 *          hdr[1] --> 0x06 is the length of the block.
 *          hdr[2] --> 0x01 is the mode of the block.
 *          hdr[3] --> 0x04 is the submode of the block.
 *          hdr[4] --> Zoom Disable flag (bit 7 from high order bit)
 *                     Zoom Threshold (bits 6-0)
 *          hdr[5] --> Zoom Factor
        hdr[0] = 0x40;
        hdr[1] = 0x03;
        hdr[2] = 0x01;
        hdr[3] = 0x04;
        hdr[4] = 0x00;
        hdr[5] = 0x00;
 */

/*	Assign values to the text header in the following format:
 *	  txthdr[0] -->  The field flag.
 *	  txthdr[1] -->  The length of the block.
 *	  txthdr[2] -->  The mode of the block.
 *	  txthdr[3] -->  The submode of the block.
 *	  txthdr[4] -->  The block mode, reverse block mode char 
 *                       size multiplier 
 *	  txthdr[5] -->  The plot process code
 *	  txthdr[6] -->  The first 8 bits of the xcoord (ix) value
 *	  txthdr[7] -->  The last 8 bits of the xcoord value.
 *	  txthdr[8] -->  The first 8 bits of the ycoord (iy) value
 *	  txthdr[9] -->  The last 8 bits of the ycoord value.
 *
 *	Plot a symbol at all NP points.
 * 
 *          Writing header and weather symbol to the buffer.
 */
            txthdr[0] = 0x40;
            txthdr[1] = (LENHDR + (m/2) ) & BMASK;
            txthdr[2] = 0x05;
            txthdr[3] = 0x02;
            txthdr[4] = 0x01;
            txthdr[5] = 0x00;
	    txthdr[6] = (ix >> 8) &  BMASK;
	    txthdr[7] = ix & BMASK;
	    txthdr[8] = ( iy >> 8 ) & BMASK;
	    txthdr[9] = iy & BMASK;

/*
 * 	    Write the header and the string to the buffer.
 */
	    awrbuf ( txthdr, (LENHDR * 2), iret );
	    awrbuf ( tchar, m, iret );
}
