#include "nccmn.h"

void mrfrhd ( int *iret )
/************************************************************************
 * mrfrhd								*
 *									*
 * This subroutine reads the frame headers from the metafile.		*
 *									*
 * mrfrhd ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if ( ( fread ( &meta_frame[frame_count],
			       sizeof ( nc_frame_header ),
			       maxfrm, meta_fp )
	     ) != (size_t)maxfrm ) {
	    *iret = G_NOMETA;
	    return;
	}

/*
 *	    Set the number of frames used.
 */
	while ( ( meta_frame[frame_count].label  != NULL ) &&
		( meta_frame[frame_count].isbyte !=    0 ) ) {
	    frame_count++;
	}

}
