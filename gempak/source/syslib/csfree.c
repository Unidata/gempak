#include	"gemsys.h"

void csfree  ( int *mproc, int *free, int *semid, int *iret )
/************************************************************************
 * csfree								*
 *									*
 * This subprogram frees semaphore locks associated with the requrested *
 * IPC message queue.							*
 *									*
 * csfree  ( mproc, free, iret )					*
 *									*
 * Input parameters:							*
 *	*mproc		int		Subprocess type			*
 *					  0 = gplt			*
 *					  1 = device driver		*
 *	*semid		int		Semaphore ID to free		*
 *									*
 * Output parameter:							*
 *	*free		int		Semaphore lock release status   *
 *					  0 = release unsuccessful	*
 *					  1 = release successful	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * R. McTaggart-Cowan	01/05		Original code			*
 ***********************************************************************/
{
  union gemsemun arg;
    
/*---------------------------------------------------------------------*/

  *iret = 0;

/*
 *  Terminate semaphore lock corresponding to IPC memory queue mproc.
 */
  *free = 1;
  if ( semctl ( *semid, 0, IPC_RMID, arg ) == -1 )
    {
      if (errno == ENOENT)
	{
	  *iret = 0;
	  *free = 1;
	}
      else
	{
	  perror("semctl");
	  *iret = -1;
	  *free = 0;
	}
    }

}

