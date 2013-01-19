#include "nccmn.h"

void mwfrhd ( int *iret )
/************************************************************************
 * mwfrhd								*
 *									*
 * This subroutine writes the frame headers to the metafile.		*
 *									*
 * mwfrhd ( iret )							*
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

/*
 *	Write the frame headers to the file.
 */
	fwrite ( meta_frame, FRM_HDRSZ, maxfrm, meta_fp );

/*
 *	Set the byte count.
 */
	byte_count += FRM_HDRSZ * maxfrm;

}
