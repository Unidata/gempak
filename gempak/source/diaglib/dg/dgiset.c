#include "dg.h"

void dg_iset ( const char *ckey, const int *nval, const int *ival,
               int *iret )
/************************************************************************
 * dg_iset                                                              *
 *                                                                      *
 * This subroutine sets, based on the key provided, the values of 	*
 * integer type global variables.					*
 *                                                                      *
 * dg_iset ( ckey, nval, ival, iret )                                   *
 *                                                                      *
 * Input parameter:							*
 *	*ckey		const char	Variable key			*
 *	*nval		const int	Number of values		*
 *	*ival		const int	Values				*
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 3/06						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( strcmp ( ckey, "LDLEVL1" ) == 0 ) {
	_dginpt.ldlevl1 = *ival;
    } else if ( strcmp ( ckey, "LDLEVL2" ) == 0 ) {
	_dginpt.ldlevl2 = *ival;
    }

    return;
}
