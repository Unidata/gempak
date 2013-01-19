#include	"gemsys.h"

void csgrab  ( int *mproc, int *grab, int *semid, int *iret )
/************************************************************************
 * csgrab								*
 *									*
 * This subprogram grabs semaphore locks associated with the requrested *
 * IPC message queue.							*
 *									*
 * csgrab  ( mbchan, grab, iret )					*
 *									*
 * Input parameters:							*
 *	*mproc		int		Subprocess type			*
 *					  0 = gplt			*
 *					  1 = device driver		*
 *									*
 * Output parameter:							*
 *	*grab		int		Semaphore lock acquire status   *
 *					  0 = acquire unsuccessful	*
 *					  1 = acquire successful	*
 *	*semid		int		Semaphore ID obtained		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * R. McTaggart-Cowan	01/05		Original code			*
 ***********************************************************************/
{
  int smchan, mbchan, ier, iproc, requeue;
  key_t key;
  struct sembuf sb = {0, -1, 0};  /* set lock */
/*---------------------------------------------------------------------*/

  *iret = 0;
  iproc = *mproc;

/*
 * Get the semaphore identifier.
 */
  requeue = 0;
  cigetq ( &iproc, &requeue, &key, &mbchan, &smchan, &ier );

/*
 *  Attempt to set semaphore for the queue
 */
  *grab = 1;
  if ( (*semid = semget ( key, 1, 0 )) == -1)
    {
      perror("semget");
      *grab = 0;
      *iret = -1;
    }
  if (*semid != smchan)
    {
      printf("WARNING: csgrab semaphore ID does not match smchan\n");
      *grab = 0;
      *iret = -1;
    }
  if ( (ier = semop ( smchan, &sb, 1 )) == -1 )
    {
      if (errno == EAGAIN)
	{
	  *grab = 0;
	  *iret = 0;
	}
      perror("semop");
      *grab = 0;
      *iret = -1;
    }

}
