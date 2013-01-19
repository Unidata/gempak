#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

int clo_bqtag ( char *info )
/************************************************************************
 * clo_bqtag                                                            *
 *                                                                      *
 * This function returns a logical value, G_TRUE if the "info" variable	*
 * satisfies the criteria set for "tBndName"; G_FALSE otherwise.	*
 *                                                                      *
 * int clo_bqtag ( info )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *info		char	Info string from bound structure	*
 *                                                                      *
 * Output parameters:                                                   *
 *      clo_bqtag	int	Logical G_TRUE or G_FALSE		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      6/01   	                                *
 ***********************************************************************/
{
int	ier;
char	data[80];
/*---------------------------------------------------------------------*/

    if ( strlen(tBndName) == (size_t)0 )  return ( G_TRUE );

    cst_gtag ( tBndName, info, "NOT_FOUND", data, &ier );

    if ( strcmp ( tBndData, data ) == 0 )  return ( G_TRUE );

    return ( G_FALSE );

}
