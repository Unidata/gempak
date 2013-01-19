#include "gemsys.h"

int cnsleep ( const struct timespec *intvl, struct timespec *rtn )
/************************************************************************
 * cnsleep								*
 *									*
 * This function is used by systems without a nanosleep builtin.  It is	*
 * compiled to 'nanosleep' (interface as given in time.h) using the	*
 * NO_NANOSLEEP definition at build time.				*
 *									*
 * cnsleep ( intvl, rtn )						*
 *									*
 * Input parameters:							*
 *	*intvl		struct timespec		Sleep interval		*
 *									*
 * Output parameters:							*
 *	*rtn		const timespec		Return time		*
 **									*
 * Log:									*
 * R. McTaggart-Cowan/SUNY	01/05		Initial implementation	*
 ***********************************************************************/
{
  int millisecond, iret;
/*---------------------------------------------------------------------*/

  iret = 0;

/*
 * Use sleep if seconds are requested, otherwise use usleep
 */

  if (intvl->tv_sec > 0)
    {
      iret = sleep ( intvl->tv_sec );
    }
  else
    {
      millisecond = intvl->tv_nsec / 1000000 + 1;
      iret = usleep ( millisecond );
    }
  return ( iret );
}
