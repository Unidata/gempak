#include    "gemsys.h"
#include    "time.h"

void ccheck ( int *mproc, int *mbchan, int *iwact, int *iret )
/************************************************************************
 * ccheck								*
 *									*
 * This subroutine checks to see if a subprocess is attached to the	*
 * message queue specified by MPROC.	 				*
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
 *					 -1 = blocked subprocess found  *
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

    int    iproc, semid, slowCheck, smchan, initmq;
    struct sembuf sb = {0, -1, IPC_NOWAIT};  /* poll for allocation */
    struct timespec fast;
    struct timespec slow;
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
    initmq = 1;
    cgetmq ( iproc, idebug, initmq, &jsatty, &key, mbchan, &smchan, &ier2 );

/*
 *  Check for lock on the message queue.
 */
    slowCheck = 0;
    *iwact = 1;
    if ( (semid = semget ( key, 1, IPC_NOWAIT )) == -1 )
      {
	if (errno == ENOENT){  /* no semaphore lock for queue */
	  *iret = 0;
	  *iwact = 0;
	}
	else{
	  perror("semget in ccheck");
	  *iret = -1;
	  *iwact = 0;
	}
      }
    if (semid != smchan)
      {
	printf("WARNING: ccheck semaphore ID and smchan do not match\n");
	*iwact = 0;
      }
    if (*iwact == 1)
      {
	if ( (ier = semop ( smchan, &sb, 1 )) == -1 )
	  {
	    if (errno == EAGAIN)
	      {
		*iwact = 1;
		/* Check that the process attached to the message
		   queue is ready to receive packets */
		slowCheck = 1;
	      }
	    else
	      {
		printf ("semop error 1 in ccheck\n");
		perror("semop");
		*iret = -1;
	      }
	  }
	else
	  {
	    sb.sem_op = 1;
	    if ( (ier = semop ( smchan, &sb, 1 )) == -1 )
	      {
		printf ("semop error 2 in ccheck\n");
		perror("semop");
		*iret = -1;
	      }
	  }
      }

/*
 * Now check for communication if a lock is set.  If this test fails,
 * then there is a dead child blocking the end of the message queue.
 * If this is the case, then a reset should be attempted.
 */
    if (slowCheck == 1)
      {
	idata [0] = 2;
	idata [1] = -999;
	nword = 2;
	itype = 1;
	gsend  ( &itype, mbchan, idata, &nword, &ier ); 

	itype = 2;
	fast.tv_sec = 0;
	fast.tv_nsec = 59999999;
	slow.tv_sec = 1;
	slow.tv_nsec = 0;
	iwait = 1;
	knt   = 0;
	
	while  ( knt < NCHECK ) /* run fast checks and one slow (1sec) check last */
	  {
	    grecv ( &itype, &iwait, mbchan, idata, &ier );
	    if  ( ier == -1 )
	      {
		knt = knt + 1;
		if  ( knt < NCHECK-1 )
		  {
		    ierr = nanosleep ( &fast, NULL );
		  }
		else
		  {
		    ierr = nanosleep ( &slow, NULL );
		}
	      }
	    else
	      if  ( idata [1] == -999 )  knt = NCHECK;
	  }
	if ( ier == -1 )
	  {
	    itype  = 1;
	    do
	      grecv ( &itype, &iwait, mbchan, idata, &ierr );
	    while ( ierr != -1 );
	  }
	if  ( ier == -1 )
	  *iwact = -1;
	else
	  *iwact = 1;
      }
	
/*
 * Remove semaphores for queue blocked by dead child.
 */
    if (*iwact == -1)
      {
	csfree ( &iproc, &ier2, &semid, &ierr );
      }
}
