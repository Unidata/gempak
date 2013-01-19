#include "utfcmn.h"

#define LENHDR	8

void uline ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * uline								*
 *									*
 * This subroutine draws lines to the UTF file.				*
 *									*
 * uline ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	ix [np]		int		X coordinates			*
 *	iy [np]		int		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Added code from UDLINE			*
 ***********************************************************************/
{

	int		i, idx, idy, knt, nbt, ixval, iyval;
	unsigned char	hdr[LENHDR], parr[MXPTS];

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
	if ( ! opnfil ) {
	    uopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	Clear the array of data points.
 */
	for ( i = 0; i < MXPTS; i++ )  {
	    parr[i] = 0;
	}

/*
 *	Create the array of data points. The vectors are represented
 *	by an initial I,J pair (in the header) and deltas from the
 *	previous location for the remaining points.
 */
	knt = 0;
	nbt = 0;
	for ( i = 0; i < *np-1; i++ )  {

/*
 *	    Calculate the delta X and Y.
 */
	    idx = ix[i+1] - ix[i];
	    idy = iy[i+1] - iy[i];

/*
 *	    If both delta values are less than 63, the short format
 *	    may be used. Otherwise the long format must be used.
 */
	    if  ( ( G_ABS(idx) < 63 ) && ( G_ABS(idy) < 63 ) )  {

/*
 *		If the X delta is less than 0, the value is the 2's
 *		complement. Otherwise, use the value computed.
 */
		if  ( idx < 0 )  {
		    ixval = ~(G_ABS(idx)) + 1;
		    parr[nbt] = 0x80 | 0x40 | (ixval & 0x3f);
		}
		else {
		    ixval = idx;
		    parr[nbt] = 0x80 | ( ixval & 0x3f );
		}
		nbt++;

/*
 *		If the Y delta is less than 0, the value is the 2's
 *		complement. Otherwise, use the value computed.
 */
		if  ( blank )  parr[nbt] = 0x80;

		if  ( idy < 0 )  {
		    iyval = ~(G_ABS(idy)) + 1;
		    parr[nbt] = parr[nbt] | 0x40 | (iyval & 0x3f);
		}
		else {
		    iyval = idy;
		    parr[nbt] = parr[nbt] | ( iyval & 0x3f );
		}
		nbt++;

/*
 *		Increment the counter for the number of 2-byte words.
 */
		knt += 1;
	    }
	    else {

/*
 *		If the X delta is less than 0, the value is the 2's
 *		complement. Otherwise, use the value computed.
 */
		if  ( idx < 0 )  {
		    ixval = ~(G_ABS(idx)) + 1;
		    parr[nbt] = 0x10 | ( (ixval>>8) & 0x0f );
		}
		else {
		    ixval = idx;
		    parr[nbt] = (ixval>>8) & 0x0f;
		}
		nbt++;
		parr[nbt] = ixval & BMASK;
		nbt++;

/*
 *		If the Y delta is less than 0, the value is the 2's
 *		complement. Otherwise, use the value computed.
 */
		if  ( blank )  parr[nbt] = 0x20;

		if  ( idy < 0 )  {
		    iyval = ~(G_ABS(idy)) + 1;
		    parr[nbt] = parr[nbt] | 0x10 | ( (iyval>>8) & 0x0f );
		}
		else {
		    iyval = idy;
		    parr[nbt] = parr[nbt] | ( (iyval>>8) & 0x0f );
		}
		nbt++;
		parr[nbt] = iyval & BMASK;
		nbt++;

/*
 *		Increment the counter for the number of 2-byte words.
 */
		knt += 2;
	    }
	}

/*
 *	Load the header for the line.   
 *	    hdr[0] --> 0xc3 which is the relative vector mode
 *	    hdr[1] --> Nothing in bits 0-1 (from high order)
 *			Zoom Disable flag (bit 2)
 *			Zoom Threshold (bits 3-4)
 *			Zoom Factor (bits 5-7)
 *	    hdr[2&3]-> I coordinate for the line's origin
 *	    hdr[4&5]-> J coordinate for the line's origin
 *	    hdr[6&7]-> Number of 2 byte words in the line
 */

	hdr[0] = 0xc3;
	hdr[1] = 0x00;
	hdr[2] = (ix[0] >> 8) & BMASK;
	hdr[3] = ix[0] & BMASK;
	hdr[4] = (iy[0] >> 8) & BMASK;
	hdr[5] = iy[0] & BMASK;
	hdr[6] = (knt >> 8) & BMASK;
	hdr[7] = knt & BMASK;

/*
 *	Check to see if this line fits in the buffer. If not,
 *	return with an error.
 */
	if  ( ( numout + nbt + LENHDR ) >= MXAFOS )  {
	    *iret = G_NAFSMX;
	    return;
	}

/*
 *	Write the vector header and data to the buffer.
 */
	uwrbuf ( hdr, LENHDR, iret );
	uwrbuf ( parr, nbt, iret );

}
