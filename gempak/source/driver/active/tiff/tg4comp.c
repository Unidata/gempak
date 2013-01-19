#include "tiffcmn.h"
#include "tg4.h"

#define PIXEL(buf,ix)	((((buf)[(ix)>>3]) >> (7-((ix)&7))) & 1)

static int finddiff ( unsigned char *cp, int bs, int be, int color);
static void putcode ( const tableentry *te );
static void putspan ( int span, const tableentry *tab );

int jpos;
int spbit;

/************************************************************************
 * tg4comp.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*=====================================================================*/

void tg4comp ( int *kpos, int *iret )
/************************************************************************
 * tg4comp								*
 *									*
 * This function compresses the image using the Group 4 Fax format.	*
 *									*
 * The Group 4 compression scheme is based on code by Sam Leffler of	*
 * Silicon Graphics, Inc. The code is included in the LIBTIFF package.	*
 *									*
 * tg4comp ( kpos, iret )						*
 *									*
 * Input/Output parameters:						*
 *	*kpos		int		Byte location before/after	*
 *					   compressing the data		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99						*
 * S. Jacobs/NCEP	 2/99	Made spbit a global variable		*
 * S. Jacobs/NCEP	 5/02	Initialize entire refr & data arrays	*
 ***********************************************************************/
{

	int		i, j, m, kbyte;
	int		a0, a1, a2, b1, b2;
	int		white = 0;

	unsigned char	refr[MAXLIN], data[MAXLIN];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Set the input position counter to a working variable.
 */
	jpos = *kpos;

	kbyte = ( kbit + (8-1) ) / 8;

	spbit = 8;

/*
 *	Set the first reference line to all white pixels.
 */
	for ( i = 0; i < MAXLIN; i++ )  {
	    refr[i] = 0;
	    data[i] = 0;
	}

/*
 *	Process the remaining lines of data.
 */
	for ( j = 0; j < klin; j++ )  {
	    for ( i = 0; i < kbyte; i++ )  {
		data[i] = rasimg[j*kbyte+i];
	    }

/*
 *	    Start a0 at the beginning of the row.
 */
	    a0 = 0;

/*
 *	    a1 is the pixel that changes color from a0 on the data line.
 */
	    if  ( PIXEL(data, 0) != white )  {
		a1 = 0;
	    }
	    else   {
		a1 = finddiff ( data, 0, kbit, white );
	    }

/*
 *	    b1 is the pixel that changes color from a0 on the
 *	    reference line.
 */
	    if  ( PIXEL(refr, 0) != white )  {
		b1 = 0;
	    }
	    else   {
		b1 = finddiff ( refr, 0, kbit, white );
	    }

/*
 *	    Continue processing the entire row.
 */
	    for (;;) {

/*
 *		Find b2, the pixel that changes color from b1 on
 *		the reference line.
 */
		b2 = finddiff ( refr, b1, kbit, PIXEL(refr,b1) );

		if (b2 >= a1) {
		    m = b1 - a1;
		    if ( !(-3 <= m && m <= 3) ) {
/*
 *			Horizontal mode
 *
 *			Find a2, the pixel that changes color from
 *			a1 on the data line.
 */
			a2 = finddiff ( data, a1, kbit, PIXEL(data,a1) );
			putcode(&horizcode);
/*
 *			If the current pixel is white, put a white code
 *			followed by a black code.
 */
			if ( a0+a1 == 0 || PIXEL(data, a0) == white ) {
			    putspan(a1-a0, whitecodes);
			    putspan(a2-a1, blackcodes);
			}
			else {
/*
 *			    Otherwise, do the opposite.
 */
			    putspan(a1-a0, blackcodes);
			    putspan(a2-a1, whitecodes);
			}
			a0 = a2;
		    }
		    else {
/*
 *			Vertical mode
 */
			putcode(&vcodes[m+3]);
			a0 = a1;
		    }
		}
		else {
/*
 *		    Pass mode
 */
		    putcode(&passcode);
		    a0 = b2;
		}

		if  ( a0 >= kbit )  break;

		a1 = finddiff ( data, a0, kbit,  PIXEL(data,a0) );
		b1 = finddiff ( refr, a0, kbit, !PIXEL(data,a0) );
		b1 = finddiff ( refr, b1, kbit,  PIXEL(data,a0) );

	    }

/*
 *	    Set the current data line to the reference line for the
 *	    next line of data.
 */
	    for ( i = 0; i < kbyte; i++ )  {
		refr[i] = data[i];
	    }

	}

	putcode(&eolcode);
	putcode(&eolcode);

/*
 *	Set the output position counter.
 */
	*kpos = jpos;

}

/*=====================================================================*/

static int finddiff ( unsigned char *cp, int bs, int be, int color )
/************************************************************************
 * finddiff								*
 *									*
 * This function finds the next pixel of a given color between the 	*
 * given bit locations.							*
 *									*
 * static int finddiff ( cp, bs, be, color )				*
 *									*
 * Input parameters:							*
 *	*cp		unsigned char	Pixel data array		*
 *	bs		int		Start of bit range		*
 *	be		int		End of bit range		*
 *	color		int		Pixel color			*
 *									*
 * Output parameters:							*
 * finddiff	static int						*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99						*
 ***********************************************************************/
{

	register int	ibits = be - bs;
	register int	n, jspan;

	register const unsigned char    *tab;

/*---------------------------------------------------------------------*/

/*
 *	Set the position to the correct byte in the data array.
 */
	cp += bs >> 3;

/*
 *	Set the table to use based on the color for which to search.
 */
	if  ( color )
	    tab = oneruns;
	else
	    tab = zeroruns;

/*
 *	Find the number of pixels of the requested color that occur
 *	consecutively.
 *
 *	Check partial byte on lhs.
 */
	if  ( ibits > 0 && ( n = (bs & 7) ) )  {
	    jspan = tab[(*cp << n) & 0xff];
	    if  ( jspan > 8-n )   jspan = 8-n;
	    if  ( jspan > ibits ) jspan = ibits;
	    if  ( n+jspan < 8 )   return ( bs + jspan );
	    ibits -= jspan;
	    cp++;
	} else
	    jspan = 0;

/*
 *	Scan full bytes for all 1's or all 0's.
 */
	while ( ibits >= 8 )  {
	    n = tab[*cp];
	    jspan += n;
	    ibits -= n;
	    if  ( n < 8 )  return ( bs + jspan );
	    cp++;
	}
/*
 *	Check partial byte on rhs.
 */
	if  ( ibits > 0 )  {
	    n = tab[*cp];
	    if  ( n > ibits )
		jspan += ibits;
	    else
		jspan += n;
	}
	return ( bs + jspan );

}

/*=====================================================================*/

static void putcode ( const tableentry *te )
/************************************************************************
 * putcode								*
 *									*
 * This function adds a given code value to the compressed data array.	*
 *									*
 * static void putcode ( te )						*
 *									*
 * Input parameters:							*
 *	*te	const tableentry	Structure of code value and len	*
 * Output parameters:							*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99						*
 ***********************************************************************/
{

	unsigned int	ibits;
	unsigned int	length;

	static short	spdata;

	static const int mask[9] =
	    { 0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff };

/*---------------------------------------------------------------------*/

	ibits  = te->code;
	length = te->length;

	while ( (int)length > spbit ) {
	    spdata |= ibits >> (length - spbit);
	    length -= spbit;

	    group4[jpos] = spdata & 0xff;
	    jpos++;

	    spdata = 0;
	    spbit  = 8;
	}

	spdata |= ( ibits & mask[length] ) << ( spbit - length );
	spbit -= length;

	if  ( spbit == 0 )  {
	    spdata |= ibits >> (length - spbit);
	    length -= spbit;

	    group4[jpos] = spdata & 0xff;
	    jpos++;

	    spdata = 0;
	    spbit  = 8;
	}

}

/*=====================================================================*/

static void putspan ( int span, const tableentry *tab )
/************************************************************************
 * putspan								*
 *									*
 * This function computes the codes for spans of pixels of the same	*
 * color.								*
 *									*
 * static void putspan ( span, tab )					*
 *									*
 * Input parameters:							*
 *	span	int			Number of pixels in span	*
 *	*tab	const tableentry	Table of code structures	*
 * Output parameters:							*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

/*
 *	While the number of consecutive pixels of the same color
 *	is greater than 2624, add the make-up code for 2560.
 */
	while ( span >= 2624 )  {
	    const tableentry *te = &tab[63 + (2560>>6)];
	    putcode ( te );
	    span -= te->runlen;
	}

/*
 *	If the number of consecutive pixels of the same color
 *	is greater than 64, add the appropriate make-up code.
 */
	if  ( span >= 64 )  {
	    const tableentry *te = &tab[63 + (span>>6)];
	    putcode ( te );
	    span -= te->runlen;
	}

/*
 *	Add the appropriate terminal code.
 */
	putcode ( &tab[span] );
}
