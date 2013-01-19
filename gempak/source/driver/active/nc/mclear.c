#include "nccmn.h"

void mclear ( int *iret )
/************************************************************************
 * mclear								*
 *									*
 * This subroutine ends the current frame and begins a new frame.	*
 *									*
 * mclear ( iret )							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log:									*
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92		Add Header Information		*
 * C. Lin/EAI		 4/93		Add Internal Buffer		*
 * A. Chang/EAI          1/94           Modified to buffered I/O        *
 * A. Chang/EAI          4/94           			        *
 * A. Chang/EAI          5/94           Checked blank frame		*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 * C. Lin/EAI           10/94           Reset text alignment       	*
 ***********************************************************************/
{
	short	buffer[3];
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Open new frame.
 */
	if ( opnfil ) {

	    buffer[0] = END_PIC;
	    meta_frame[frame_count].iebyte = byte_count + NUM_BTSZ;

	    frame_count++;
/*
 *	    Check for number of frames less than the maximum.
 */
	    if ( frame_count < maxfrm ) {

		buffer[1] = BEG_PIC;
		buffer[2] = BEG_PICBODY;

		fwrite ( buffer, NUM_BTSZ, 3, meta_fp );

		byte_count += NUM_BTSZ * 3;

		meta_frame[frame_count].isbyte = byte_count;

		lcolor_set = G_RESET;
		lwidth_set = G_RESET;
		fcolor_set = G_RESET;
		fstyle_set = G_RESET;
		txfont_set = G_RESET;
		txsize_set = G_RESET;
		txalgn_set = G_RESET;

	    }
	    else {

		mclose ( &ier );
		*iret = G_NMAXFR;

	    }

	}
}
