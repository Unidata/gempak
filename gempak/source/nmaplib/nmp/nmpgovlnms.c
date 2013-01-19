#include "nmpcmn.h"

void nmp_govlnms ( nmpovlstr_t ovlnms[], int *iret )
/************************************************************************
 * nmp_govlnms                                                          *
 *                                                                      *
 * This function gets an array of all overlay names.			* 
 *                                                                      *       
 * void nmp_govlnms ( ovlnms, iret )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      ovlnms[]	nmpovlstr_t             Array of overlay names	*
 *      *iret           	int             Return code             * 
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 * M. Li/GSC		12/00	Added nmpovlstr_t			*
 ***********************************************************************/
{
int     ii;

/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( overlay[0].novl <= 0 ) {
        *iret = -2;
    }
    else {
        for (ii = 0; ii < overlay[0].novl; ii++) {
           strcpy ( ovlnms[ii], overlay[0].mapovl[ii].title);
        }
    }

}
            
/*=====================================================================*/
