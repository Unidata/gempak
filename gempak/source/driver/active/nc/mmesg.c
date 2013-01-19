#include "nccmn.h"

void mmesg ( char *messag, int *ilen, int *iret )
/************************************************************************
 * mmesg								*
 *									*
 * This subroutine writes a frame title the metafile.			*
 *									*
 * mmesg  ( messag, ilen, iret )					*
 *									*
 * Input parameters:							*
 *	*messag		char		Title of the frame		*
 *	*ilen		int		Length of the title		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * C. Lin/EAI		11/92						*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Open the file, if necessary.
 */
	if ( ! opnfil ) {
	    mopen ( &ier ) ;
	    if  ( ier != G_NORMAL ) {
		*iret = G_NMDATA;
		return;
	    }
	}

	if ( *ilen < FRM_LBLSZ )
	    messag[(*ilen)] = '\0';
	else
	    messag[FRM_LBLSZ-1] = '\0';

	strcpy ( meta_frame[frame_count].label, messag );

}
