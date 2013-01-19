#include "ardcmn.h"

#define LENHDR	8
#define ONEFOUR 6

void aline ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * aline								*
 *									*
 * This subroutine draws lines to the UTF file.				*
 *									*
 * aline ( np, ix, iy, iret )						*
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
 * A. Hardy/GSC         8/98            Modified from utf's ULINE       *
 * A. Hardy/GSC         9/98            Changed length byte values      *
 ***********************************************************************/
{

	int		i, idx, idy, knt, nbt, ixval, iyval, length;
        int             len2;
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
	    aopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	Clear the array of data points.
 */
	for ( i = 0; i < MXPTS; i++ )  {
	    parr[i] = 0;
	}
/*
 *      Setting the plot parameters block: mode 1 submode 4.
 *	    hdr[0] --> 0x40 is the field flag
 *	    hdr[1] --> 0x06 is the length of the block.
 *	    hdr[2] --> 0x01 is the mode of the block.
 *	    hdr[3] --> 0x04 is the submode of the block.
 *	    hdr[4] --> Zoom Disable flag (bit 7 from high order bit)
 *		       Zoom Threshold (bits 6-0)
 *	    hdr[5] --> Zoom Factor 
 */
	hdr[0] = 0x40;
	hdr[1] = 0x03;
	hdr[2] = 0x01;
	hdr[3] = 0x04;
	hdr[4] = 0x00;
	hdr[5] = 0x00;
/*
 *	Write the plot parameter inforation to the buffer.
 */
	awrbuf ( hdr, ONEFOUR, iret );

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
	length = knt;

/*
 *	Load the header for the line.   
 *	    hdr[0] -> Flag field
 *	    hdr[1] -> Length of block
 *	    hdr[2] -> 0x04 is the mode for the block   
 *	    hdr[3] -> 0x05 is the submode for the block
 *	    hdr[4&5] -> I coordinate for the line's origin
 *	    hdr[6&7] -> J coordinate for the line's origin
 */
        len2   = ( length + 4 ) | 0x4000;
        hdr[0] = ( len2 >> 8 ) & BMASK;
	hdr[1] = len2  & BMASK;
	hdr[2] = 0x04;
	hdr[3] = 0x05;
	hdr[4] = (ix[0] >> 8) & BMASK;
	hdr[5] = ix[0] & BMASK;
	hdr[6] = (iy[0] >> 8) & BMASK;
	hdr[7] = iy[0] & BMASK;

/*
 *	Write the vector header and data to the buffer.
 */
	awrbuf ( hdr, LENHDR, iret );
	awrbuf ( parr, nbt, iret );

}
