#include    "gemsys.h"

void cgetmq ( int mproc, int idebug, int initmq, int *jsatty, int *key,
	      int *mbchan, int *smchan, int *iret )
/************************************************************************
 * cgetmq                                                               *
 *                                                                      *
 * This subroutine connects to a message queue.                         *
 *                                                                      *
 * cgetmq ( mproc, idebug, jsatty, key, mbchan, smchan, iret )          *
 *                                                                      *
 * Input parameters:                                                    *
 *      mproc           int             Process type                    *
 *      idebug          int             Debug flag                      *
 *                                        1 = write debug message       *
 *                                      <>1 = don't write               *
 *	initmq		int		Initialize memory queues	*
 *					  0 = don't initialize		*
 *					  1 = initialize		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *jsatty         int             Terminal flag                   *
 *                                        1 = key from terminal id      *
 *                                        0 = key from process id       *
 *	*key		int		Message queue key		*
 *      *mbchan         int             Message queue number            *
 *	*smchan		int		Semaphore ID			*
 *      *iret           int             Return code                     *
 **									*
 * Log:									*
 * M. desJardins/NMC	 3/92	Consolidated code in one subroutine	*
 * S. Jacobs/EAI	 9/93	IBM specific code			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Chiswell/UCAR	 3/04	Added perror				*
 ***********************************************************************/
{
  int    mq, nkey, semid;
  char   id, device [20];

/*---------------------------------------------------------------------*/
  *iret = 0;
  
  if ( idebug == 1 ) {
    printf ( "CGETMQ - Printing to output file - %d.\n", mproc );
    fprintf ( fdes[mproc],
	      "isatty=%d, ttyname=%s, getpid=%d, getppid=%d\n",
	      isatty(2), ttyname(2), getpid(), getppid() );
  }
  
/*
 *  Get key to set up message queue.  The key is made from either the
 *  terminal name or from a process id.
 */
  if ( isatty (2) ) {

/*
 *	Get the terminal and use as a file name in ftok to create a
 *	unique key for this terminal. This is used for interactive
 *	session, including scripts executed from the command line.
 *
 *	This section is machine specific. Most machines will use this
 *	algorithm. There is a different algorithm for IBM machines.
 */
    sprintf ( device, "%s", ttyname (2) );
    
    if  ( mproc == 0 )
      id = 'G';
    else
      id = 'D';
    
    nkey  =  ftok ( device, id );
    
    *jsatty = 1;
  }
  else {

/*
 *	Use the process id or the parent process id as the key. This 
 *	is used for batch type sessions such as executing a script
 *	from the CRON.
 */
    if  ( mproc == 0 )
      nkey = GEMPAK_MQ_G + getppid ();
    else

      nkey = GEMPAK_MQ_D + getpid  ();
    
    *jsatty = 0;
  }
  *key = nkey;

/*
 *  Get id of message queue for this key.  This queue id is returned
 *  as the channel number.
 */
  if (initmq > 0) 
    {
      if ( (mq = msgget ( nkey, 0666 | IPC_CREAT )) == -1 )
	perror("msgget");
    }
  else
    {
      if ( (mq = msgget ( nkey, 0 )) == -1 )
	{
	  if (errno == ENOENT)
	    {
	    }
	  else
	    {
	      perror("msgget");
	    }
	}
    }
  *mbchan = mq;

/*
 * Get id of the semaphores that will be used to lock the memory queue
 * once a child process is initiated.  Semaphores will be created here
 * if they do not already exist.
 */

  semid   = semget ( nkey, 1, 0 );
  *smchan = semid;
  if (semid == -1)
    {
      if (errno == ENOENT)
	{
	}
      else
	{
	  perror( "semget" );
	}
    }

/*
 *  Write information if requested.
 */
  if ( idebug == 1 )
    {
      if  ( *jsatty ) {
	printf ( "CGETMQ - Printing to output file - %d.\n", mproc );
	fprintf ( fdes[mproc],
		  "terminal = %s; key = %i; mbchan = %i\n",
		  device, nkey, mq );
      }
      else {
	printf ( "CGETMQ - Printing to output file - %d.\n", mproc );
	fprintf ( fdes[mproc],
		  "process id = %i; key = %i; mbchan = %i\n",
		  getpid(), nkey, mq );
      }
    }
}
