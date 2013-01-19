#include    "gemsys.h"

void ccheck ( int *mproc, int *mbchan, int *iwact, int *iret )
/************************************************************************
 * ccheck								*
 *									*
 * This subroutine checks to see if a subprocess is attached to the	*
 * message queue specified by MPROC.					*
 *									*
 * ccheck  ( mproc, mbchan, iwact, iret )				*
 *									*
 * Input parameters:							*
 *	*mproc		int		Process type			*
 *					  0 = gplt			*
 *					  1 = device driver		*
 *									*
 * Output parameters:							*
 *	*mbchan		int		Message queue number		*
 *	*iwact		int		Subprocess existence flag	*
 *					  0 = subprocess not found	*
 *					  1 = subprocess found		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * M. desJardins/NMC	 7/91	Original code				*
 * M. desJardins/NMC    10/91   Use error lun rather than input lun	*
 * S. Jacobs/EAI	 9/93	Added debug output files		*
 * L. Williams/EAI	 7/94	Reformat header				*
 ***********************************************************************/
{
    int    idata [128], nword, itype, iwait, knt, ier, ierr;
    int    idebug, ier2, jsatty, key;

    int    iproc;

/*---------------------------------------------------------------------*/

    *iret = 0;

    iproc = *mproc;

/*
 *  Determine whether to write debug messages.
 *
 *  The debug output will go to files specified by mproc.
 */

    idebug = GEM_DEBUG;
    if  ( idebug == 1 ) {
        if  ( iproc == 0 ) {
            if  ( fdes[0] == NULL )
                fdes[0] = fopen ( "appl_gplt.mqout", "a+" );
	}
        else {
            if  ( fdes[1] == NULL ) {
                fdes[1] = fopen ( "gplt_dev.mqout", "a+" );
	    }
	}

	printf ( "CCHECK - Printing to output file - %d.\n", iproc );
	fprintf ( fdes[iproc], "CCHECK: mproc=%d\n", iproc );
    }

/*
 *  Determine the correct message queue to use.
 */
    cgetmq ( iproc, idebug, &jsatty, &key, mbchan, &ier2 );

/*
 *  First send a message to the queue.
 */

    idata [0] = 2;
    idata [1] = -999;
    nword = 2;
    itype = 1;
    gsend  ( &itype, mbchan, idata, &nword, &ier ); 

/*
 *  Now wait for the subprocess connected to this mailbox to answer.
 *    -- check for reply
 *    -- if no reply, sleep for 1 sec, check for reply
 *    -- if no reply, sleep for 1 more sec, check for reply
 *    -- if no reply, assume process not there
 */
    itype = 2;  
    iwait = 1;
    knt   = 0;
    while  ( knt < NCHECK )
      {
        grecv ( &itype, &iwait, mbchan, idata, &ier );
        if  ( ier == -1 )
          {
            knt = knt + 1;
            if  ( knt < NCHECK )  sleep (1);
          }
          else
            if  ( idata [1] == -999 )  knt = NCHECK;
      }

/*
 *  If process is not active, read mailbox to clear it.
 */

    if ( ier == -1 )
      {
        itype  = 1;
        do
            grecv ( &itype, &iwait, mbchan, idata, &ierr );
        while ( ierr != -1 );
      }

/*
 *  Set return code depending on whether process answered.
 */

    if  ( ier == -1 )
        *iwact = 0;
      else
        *iwact = 1;

}
