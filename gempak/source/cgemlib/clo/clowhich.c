#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

int clo_which ( char *type )
/************************************************************************
 * clo_which                                                    	*
 *                                                                      *
 * This function returns an integer pointer into the master CLO		*
 * structure indicating which entry matches the input CLO name.		*
 *                                                                      *
 * int clo_which ( type )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*type		char		Name of CLO parameter		*
 *									*
 * Output parameters:                                                   *
 *	clo_which	int		Return value			*
 *					=  < 0 - no match		*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 1/00	Create					*
 ***********************************************************************/
{
int	which;
/*---------------------------------------------------------------------*/

	which = 0;

	while ( which < clo.nloc ) {
	    if ( strcmp( type, clo.loc[which].name ) == 0 )  
		return ( which );
	    which++;
	}

	return ( -1 );

}
