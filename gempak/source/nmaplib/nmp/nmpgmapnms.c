#include "nmpcmn.h"

void nmp_gmapnms ( nmpstr_t mapnms[], int *iret )
/************************************************************************
 * nmp_gmapnms                                                          *
 *                                                                      *
 * This function gets an array of names to all the predefined areas.   	*
 *                                                                      *       
 * void nmp_gmapnms ( mapnms, iret )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      mapnms[]	nmpstr_t	Array of map areas      	* 
 *      *iret           int             Return code             	* 
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 * M. Li/GSC		12/00	Added nmpstr_t				*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if (num_maps <= 0) {
	*iret = -1;
    } 
    else {
    	for (ii = 0; ii < num_maps; ii++) {
           strcpy ( mapnms[ii], map_tbl[ii].name );
        }
    }

}
            
/*=====================================================================*/
