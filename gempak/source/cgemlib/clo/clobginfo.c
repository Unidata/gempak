#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bginfo ( char *name, int ihot, char *info, int *iret )
/************************************************************************
 * clo_bginfo								*
 *									*
 * This function returns info information currently indexed		*
 * by the bounds hotlist.						*
 *									*
 * clo_bginfo ( name, ihot, info, iret )				*
 *									*
 * Input parameters:							*
 *	*name		char	Bound name				*
 *	ihot		int	Maximum allowed in the returned arrays	*
 *									*
 * Output parameters:							*
 *	*info		char	Metainformation of bound		*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 ***********************************************************************/
{
int	which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    info[0] = '\0';

    if ( ihot > nhot )  {
	*iret = -1;
	return;
    }

    which = clo_which ( name );

    strcpy ( info, clo.loc[which].bnd.bound[hotlist[ihot]].info );

}
