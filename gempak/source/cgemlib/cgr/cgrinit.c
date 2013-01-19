#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

int	ncgrPoints;
int	maxcgrPoints=MAX_CGR_PTS;
POINT	**cgrPoints;
int	nBytesAlloc, nBytesFree;


void	cgr_init ( int *iret )
/************************************************************************
 * cgr_init								*
 *                                                                      *
 * This function performs the necessary operations to startup CGR       *
 * processing.                                                          *
 *                                                                      *
 * cgr_init ( int *iret )                                               *
 *                                                                      *
 * Input parameters:                                                    *
 * none                                                                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int     Return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( cgrPoints == (POINT**)NULL )  {

        cgrPoints = NEW(POINT*,maxcgrPoints);

        ncgrPoints = 0;

    }

}

