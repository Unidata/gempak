#include "geminc.h"
#include "gemprm.h"
#include "time.h"

void cssleep  ( int *nanosec, int *iret )
/************************************************************************
 * cssleep								*
 *									*
 * This subroutine is used by UNIX systems to wait for a variable	*
 * number of nanoseconds.						*
 *									*
 * cssleep ( nsec, iret )						*
 *									*
 * Input parameters:							*
 *	*nsec		int		Number of nanoseconds		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * R. McTaggart-Cowan/SUNY	01/05	Created from csleep	        *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

#ifdef NO_NANOSLEEP
	{
  	int microsec;

	microsec = *nanosec/1000000 + 1;
	*iret = usleep ( microsec );
	}
#else
	{
  	struct timespec rqtp;

	rqtp.tv_sec = 0;
	rqtp.tv_nsec = (long)(*nanosec);

	*iret = nanosleep ( &rqtp, NULL );
	if ( *iret == -1 )
	  {
	  if ( errno != EINTR ) 
	     perror("nanosleep");
	  else
	     *iret = 0;
	  }
	}
#endif

}
