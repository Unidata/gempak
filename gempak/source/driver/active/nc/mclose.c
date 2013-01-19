#include "nccmn.h"

void mclose ( int *iret )
/************************************************************************
 * mclose                                                               *
 *                                                                      *
 * This subroutine closes the metafile.	  	                        *
 *                                                                      *
 * mclose  ( iret )                                                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92		Add Header Information		*
 * C. Lin/EAI		 4/93		Add Internal Buffer		*
 * A. Chang/EAI          1/94           Modified to buffered I/O        *
 * A. Chang/EAI          4/94           			        *
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 8/94		Fixed call to mwfrhd		*
 ***********************************************************************/
{
	short	buffer[2];
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if ( opnfil ) {

/*
 *	    End of frame. Set the end byte for the frame header.
 */
	    buffer[0] = END_PIC;

	    byte_count += NUM_BTSZ;
	    meta_frame[frame_count].iebyte = byte_count;

/*
 *	    End of file. Write the ending codes to the file.
 */
	    buffer[1] = END_MF;
	    fwrite ( buffer, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ;

/*
 *	    Set the file pointer to the beginning of the file, and
 *	    write the file and frame headers to the file.
 */
	    fseek ( meta_fp, (off_t)(FIL_HDRSZ), SEEK_SET );

	    mwfrhd ( &ier );

/*
 *	    Close the file and reset the open file flag.
 */
	    fclose ( meta_fp );

	    opnfil = G_FALSE;
	}

}
