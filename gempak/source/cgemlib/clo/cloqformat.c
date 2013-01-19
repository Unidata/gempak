#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

int clo_qformat ( char *name )
/************************************************************************
 * clo_qformat                                                    	*
 *                                                                      *
 * This function returns an integer indicating the format (station or	*
 * bound) for the given name.						*
 *                                                                      *
 * int clo_qformat ( name )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*name		char		Name of CLO parameter		*
 *									*
 * Output parameters:                                                   *
 *	clo_qformat	int		Return value			*
 *					= -1 - invalid name		*
 *					= 0 - station 			*
 *					= 1 - bound 			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 8/00	Create					*
 ***********************************************************************/
{
int	which;
/*---------------------------------------------------------------------*/

	which = clo_which( name );

	if ( which < 0 )  return ( -1 );

	return ( clo.loc[which].format );

}
