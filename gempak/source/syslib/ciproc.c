#include    "gemsys.h"

void ciproc  ( int *mproc, int *mbchan, int *iret )
/************************************************************************
 * ciproc								*
 *									*
 * This subroutine is called by subprocesses to create a communications	*
 * channel between the subprocess and the calling process.  The		*
 * subprocess is created using GSPROC.					*
 *									*
 * ciproc  ( mproc, mbchan, iret )					*
 *									*
 * Input parameters:							*
 *	*mproc		int		Subprocess type			*
 *					  0 = gplt			*
 *					  1 = device driver		*
 *									*
 * Output parameters:							*
 *	*mbchan		int		Message queue number		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = system error		*
 **									*
 * Log:									*
 * G. Chatters/RDS	 4/82						*
 * M. desJardins/GSFC	 4/85						*
 * M. desJardins/NMC	 7/91	UNIX version; added mproc		*
 * M. desJardins/NMC    10/91   Use error lun rather than input lun	*
 * S. Jacobs/EAI	 9/93	Added debug output files		*
 * L. Williams/EAI	 7/94	Reformat header				*
 ***********************************************************************/
{
    int     ierr;
    int     idebug, key;

    int     iproc, smchan, requeue;

/*---------------------------------------------------------------------*/
    iproc = *mproc;

/*
 *  Write debug error message if requested.
 */
    idebug = GEM_DEBUG;
    if  ( idebug == 1 ) {
	if  ( iproc == 0 ) {
            if  ( fdes[0] == NULL )
                fdes[0] = fopen ( "appl_gplt.mqout", "a+" );
	}
        else {
            if  ( fdes[1] == NULL )
                fdes[1] = fopen ( "gplt_dev.mqout", "a+" );
	}

	printf ( "CIPROC - Printing to output file - %d.\n", iproc );
	fprintf ( fdes[iproc],
		  "CIPROC: isatty=%d, getppid=%d, mproc=%d\n", 
		  isatty(2), getppid(), iproc );
    }

/*
 * Get message queue information from terminal or parent
 */
    requeue = 1;
    cigetq ( &iproc, &requeue, &key, mbchan, &smchan, &ierr );

/*
 *  Return an error if there is no message queue.
 */
    if  ( *mbchan == -1 )
        *iret = -1;
      else
        *iret = 0;

}
