#include "nccmn.h"

void mfill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * mfill								*
 *									*
 * This subroutine draws a filled polygon to the metafile.		*
 *									*
 * mfill ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	ix[]		int		X coordinates			*
 *	iy[]		int		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92	Add Header Information			*
 * C. Lin/EAI		 4/93	Add Internal Buffer			*
 * A. Chang/EAI		 1/94	Modified to buffered I/O		*
 * A. Chang/EAI		 4/94						*
 * A. Chang/EAI		 5/94	Eliminated FILL_STYLE code		*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * M. Linda/GSC		 2/97	buffer[2048] to buffer[LLMXPT]		*
 * S. Jacobs/NCEP	 3/98	Allow other fill types than just solid	*
 * J. Wu/SAIC	 	 6/05	remove reference to LLMXPT		*
 ***********************************************************************/
{
	short	*buffer;
	short	colbuf[2];
	short	flsbuf[2];
	short	nbyte;
	int	numbyte, i, k, ibyt, ier;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;
/*
 *	Check for too many points.  Number of bytes must fit into short.
 */
	numbyte = (*np) * 2 * NUM_BTSZ;

	if ( numbyte > MAX_TWOBT ) {
	    *iret = G_NMDATA;
	    return;
	}

        G_MALLOC ( buffer, short, ((*np) * 2 + 2), "MFILL" );

/*
 *	Open the file, if necessary.
 */
	if  ( ! opnfil ) {
	    mopen ( &ier );
	    if  ( ier != G_NORMAL ) {
		*iret = ier;
		return;
	    }
	}

/*
 *	Set fill color.
 */
	if  ( fcolor_req != fcolor_set ) {
	    fcolor_set = fcolor_req;

	    nbyte = 1 * NUM_BTSZ;
	    colbuf[0] = FILL_COLR | nbyte;
	    colbuf[1] = (short) fcolor_set;
	    colbuf[1] <<= 8;

	    fwrite ( colbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Set fill style.
 */
	if  ( fstyle_req != fstyle_set ) {
	    fstyle_set = fstyle_req;

	    nbyte = 1 * NUM_BTSZ;
	    flsbuf[0] = FILL_STYLE | nbyte;
	    flsbuf[1] = (short) fstyle_set;

	    fwrite ( flsbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Draw a filled polygon.
 */
	if  ( numbyte < 31 ) {
	    buffer[0] = POLYGON | (short) numbyte;
	    ibyt = 1;
	}
	else {
	    buffer[0] = POLYGON | LONG_FORM;
	    buffer[1] = (short) numbyte;
	    ibyt = 2;
	}

	k = ibyt;
	for ( i = 0; i < (*np); i++ ) {
	    buffer[k] = (short) ( ix[i] );
	    k++;

	    buffer[k] = (short) ( iy[i] );
	    k++;
	}

	if  ( k > 0 ) {
	    fwrite ( buffer, NUM_BTSZ, k, meta_fp );
	}

	byte_count += ( numbyte + ibyt * NUM_BTSZ );

        G_FREE ( buffer, short );
}
