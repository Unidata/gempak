#include "geminc.h"
#include "gemprm.h"

void utf_pvctr ( int shift_x, int shift_y, unsigned char *ptr, int *add,
		 int *iret )
/************************************************************************
 * utf_pvctr								*
 *									*
 * This function plots relative vectors (C3) from a UTF file.		*
 *									*
 * utf_pvctr ( shift_x, shift_y, ptr, add, iret )			*
 *									*
 * Input parameters:							*
 *	shift_x		int		X shift factor			*
 *	shift_y		int		Y shift factor			*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*add		int		Size of record in bytes		*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96	Copied from utf_vectr			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
    int			xsign, ysign, yoff, xoff, blank, zf;
    int			ier, i, j, k, len, xpnt[8000], ypnt[8000], x, y;
    int			incr, jncr, zd, zt, ipnt, jpnt, bytadd;
    unsigned int	word, word2;
    float		rx[8000], ry[8000];
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
**  Initialize vector arrays to zero.
*/
    for ( k = 0; k < 5000; k++ ) {
	xpnt[k] = 0;
	ypnt[k] = 0;
	rx[k]   = 0.0F;
	ry[k]   = 0.0F;
    }

/*
**  Decode the header of the C3 record.
*/
    utf_dvctr( shift_x, shift_y, ptr, &zd, &zt, &zf, &ipnt, &jpnt,
	       &len, &bytadd, &ier );
    *add = bytadd;
    ptr += 8;

/*
**  Fill the first element of vector arrays with I, J coordinate.
*/
    if  ( ipnt < 5000 && ipnt > 0 )
	xpnt[0] = ipnt;
    else
	xpnt[0] = 0;

    if  ( jpnt < 5000 && jpnt > 0 )
	ypnt[0] = jpnt;
    else
	ypnt[0] = 0;

    rx[0] = (float) xpnt[0];
    ry[0] = (float) ypnt[0];

    j = 1;
    incr = 0;
    jncr = 0;

/*
**  Loop over number of words, decoding each word of data.
*/
    for ( i = 0; i < len; i += jncr, ptr += incr ) {

	word = (unsigned int)(*ptr << 8) + *(ptr + 1);
	x = xpnt[j-1];
	y = ypnt[j-1];
/*
**	Determine which type of word, either delta X and Y together
**	or separate, then decode the word and compute the vector.
*/
	if  ( ( word & 0x8000 ) != 0 ) {
	    jncr = 1;
	    incr = 2;
/*
**	    Delta X and delta Y are in the same word.
*/
	    blank = word & 0x80;
	    xsign = word & 0x4000;
	    ysign = word & 0x40;
	    xoff = (word & 0x3f00) >> 8;
	    yoff = (word & 0x3f);
	    if  ( xsign != 0 )
		xoff = (xoff - 0x3f) - 1;
	    if  ( ysign != 0 )
		yoff = (yoff - 0x3f) - 1;
	    if  ( blank == 0 )  {
		xpnt[j] = x + (xoff << shift_x) * zf;
		ypnt[j] = y + (yoff << shift_y) * zf;
		rx[j] = (float) xpnt[j];
		ry[j] = (float) ypnt[j];
		j++;
	    }
/*
**	    If a blank vector flag is set in the word, pass rx and ry
**	    into GLINE, then reset array counter before decoding next
**	    vector.
*/
	    else {
		gline( sys_G, &j, rx, ry, &ier, strlen(sys_G) );
		j = 0;
		xpnt[j] = x + (xoff << shift_x) * zf;
		ypnt[j] = y + (yoff << shift_y) * zf;
		rx[j] = (float) xpnt[j];
		ry[j] = (float) ypnt[j];
		j++;
	    }

	}
	else {
	    jncr = 2;
	    incr = 4;
/*
**	    Delta X and delta Y are in separate words.
*/
	    word2 = (unsigned int) (*(ptr + 2) << 8) + *(ptr + 3);
	    blank = word2 & 0x2000;
	    xsign = word & 0x1000;
	    ysign = word2 & 0x1000;
	    xoff = word & 0xfff;
	    yoff = word2 & 0xfff;
	    if  ( xsign != 0 )
		xoff = xoff - 0xfff - 1;
	    if  ( ysign != 0 )
		yoff = yoff - 0xfff - 1;
	    if  ( blank == 0 ) {
/*
**		Compute the X and Y vector.
*/
		xpnt[j] = x + (xoff << shift_x) * zf;
		ypnt[j] = y + (yoff << shift_y) * zf;
		rx[j] = (float) xpnt[j];
		ry[j] = (float) ypnt[j];
		j++;
	    }
	    else {
/*
**	    If a blank vector flag is set in the word, pass rx and ry
**	    into GLINE, then reset array counter before decoding next
**	    vector.
*/
		gline( sys_G, &j, rx, ry, &ier, strlen(sys_G) );
		j = 0;
		xpnt[j] = x + (xoff << shift_x) * zf;
		ypnt[j] = y + (yoff << shift_x) * zf;
		rx[j] = (float) xpnt[j];
		ry[j] = (float) ypnt[j];
		j++;
	    }
	}
    }

/*
**  Pass vector arrays to be plotted into GLINE.
*/
    gline( sys_G, &j, rx, ry, &ier, strlen(sys_G) );
    *add = len * 2 + 8;

}
