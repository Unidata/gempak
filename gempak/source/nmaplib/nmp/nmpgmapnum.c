#include "nmpcmn.h"

void nmp_gmapnum ( int *mapnum, int *iret )
/************************************************************************
 * nmp_gmapnum                                                          *
 *                                                                      *
 * This function gets the number of predefined areas. nmp_init must be	*
 * called before calling this function.					*
 *									*
 * void nmp_gmapnum ( mapnum, iret )					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*mapnum		int		The number of map areas		*
 *      *iret           int             Return code                     *
 *                                       = 0  - OK                      *
 *                                       = -1 - table not read          *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    *mapnum = num_maps;
    if (num_maps <= 0) {
	*iret = -1;
	*mapnum = IMISSD;
    }

}
/*=====================================================================*/	    


