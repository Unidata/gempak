#include    "gemsys.h"

void cendmq ( int *mbchan, int *iret )
/************************************************************************
 * cendmq								*
 *									*
 * This subroutine closes a message queue when it is no longer needed.	*
 *									*
 * cendmq ( mbchan, iret )						*
 *									*
 * Input parameters:							*
 *	*mbchan		int		Message queue number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * J. Nielsen/TAMU	 2/92						*
 * M. desJardins/NMC	 3/92	Changed name				*
 * S. Jacobs/EAI	 9/93	Added debug statements			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Jacobs/NCEP	 9/97	Initialized buf				*
 ***********************************************************************/
{
   int      idebug, ichan;
   struct   msqid_ds  *buf;

/*---------------------------------------------------------------------*/
    *iret = 0;
    buf = NULL;
    ichan = *mbchan;

    idebug = GEM_DEBUG;
    if  ( idebug == 1 ) {
	printf ( "CENDMQ: mbchan = %d\n", *mbchan );
    }

/*
 *  Terminate message queue.
 */
    if  ( msgctl ( ichan, IPC_RMID, buf ) == -1 )
	*iret = -1;

/*
 *  Close the debug output files.
 */
    if ( idebug == 1 ) {
	if ( fdes[0] != NULL )
	    fclose ( fdes[0] );

	if ( fdes[1] != NULL )
	    fclose ( fdes[1] );
    }

}
