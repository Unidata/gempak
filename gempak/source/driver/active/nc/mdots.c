#include "nccmn.h"

void mdots ( int *ix, int *iy, int *irad, int *iret )
/************************************************************************
 * mdots								*
 *									*
 * This subroutine draws circles to the metafile.			*
 *									*
 * mdots ( ix, iy, irad, iret )                                         *
 *                                                                      *
 * Input parameters:							*
 *	*ix		int		X coordinate 			*
 *	*iy		int		Y coordinate 			*
 *	*irad		int		Radius of the circle		*
 *                                                                      *
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92		Add Header Information		*
 * C. Lin/EAI		 4/93		Add Internal Buffer		*
 * A. Chang/EAI		 1/94		Modified to buffered I/O	*
 * A. Chang/EAI		 5/94		Checked opnfil			*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 * A. Hardy/GSC         11/98           Renamed from mcirc              *
 ***********************************************************************/
{
	short	buffer[4];
	short	colbuf[2];
	short	widbuf[2];
	short	nbyte;
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

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
 *      Set line width.
 */

	if  ( lwidth_req != lwidth_set ) { 
	    lwidth_set = lwidth_req;

	    nbyte = 1 * NUM_BTSZ;
	    widbuf[0] = LINE_WIDTH | nbyte;
	    widbuf[1] = (short) lwidth_set;

	    fwrite ( widbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Draw a circle.
 */
	nbyte = 3 * NUM_BTSZ;

	buffer[0] = CIRCLE | nbyte;
	buffer[1] = (short) (  *ix );
	buffer[2] = (short) (  *iy );
	buffer[3] = (short) ( *irad );

	fwrite ( buffer, NUM_BTSZ, 4, meta_fp );

	byte_count += NUM_BTSZ * 4;

}
