#include "gemsys.h"

void cigetq ( int *mproc, int *requeue, key_t *key, int *mbchan, int *smchan, 
	      int *iret )
/************************************************************************
 * cigetq                                                               *
 *                                                                      *
 * This function allows the child process to access the message queue   *
 * and semaphore queue identifiers.					*
 *									*
 * cigetq ( mproc, requeue, key, mbchan, smchan, iret )			*
 *									*
 * Input paramters:							*
 *	mproc		int		Process type			*
 *					  0 = gplt process		*
 *					  1 = device process		*
 *	requeue		int		Re-post data to queue		*
 *					  0 = false			*
 *					  1 = true			*
 *									*
 * Output paramters:							*
 *	key		key_t		Generated key			*
 *	mbchan		int		Message queue ID		*
 *	smchan		int		Semaphore ID (-1 if none)	*
 *	iret		int		Return code			*
 **									*
 * Log:									*
 * R. McTaggart-Cowan/SUNY	01/05	Initial implementation		*
 ***********************************************************************/
{
  int ier;
  int idebug, jsatty, msqid, iproc, semid, initmq, nword;
  int keytmp, itype = 1, msgflg = 01666, iwait, idata [128];

/*---------------------------------------------------------------------*/
  iproc = *mproc;
  idebug = GEM_DEBUG;

/*
 *  If the terminal does not determine the key for the message queue,
 *  it will be passed from the main program.
 */

  if  ( isatty (2) )
    {
      initmq = 0;
      cgetmq ( iproc, idebug, initmq, &jsatty, key, mbchan, smchan, &ier ); 
    }
  else
    {
    if  ( iproc == 0 )
      {
	keytmp = GEMPAK_MQ_T + getppid ();
	msqid = msgget ( keytmp, msgflg );
	itype  = 1;
	iwait  = 0;
	crecv  ( &itype, &iwait, &msqid, idata, & ier );
	*key    = idata [0];
	/* Re-post the message if requested */
	if (*requeue == 1)
	  {
	    nword = 1;
	    csend ( &itype, &msqid, idata, &nword, &ier );
	  }
	else
	  {
	    cendmq ( &msqid, &ier );
	  }
      }
    else
      {
	*key    = GEMPAK_MQ_D + getppid ();
      }
    
/*
 * Access message queues and return ID
 */
    msqid  = msgget ( *key, msgflg );
    *mbchan = msqid;

/*
 * Access semaphores and return ID
 */
    semid  = semget ( *key, 1, 0 );
    if (semid == -1 )
      {
        perror ("semget cigetq");
      }
    *smchan = semid;

    }
}
