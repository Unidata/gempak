#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

void	cgr_done ( int *iret )
/************************************************************************
 *                                                                      *
 * cgr_done								*
 *                                                                      *
 * This function performs the necessary operations to end CGR		*
 * processing.								*
 *                                                                      *
 * cgr_done ( int *iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * none									*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int     Return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
int	ier;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    polyp_freepts ( &ier );

}

