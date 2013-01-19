#include "nmpcmn.h"

void nmp_govlflg ( int lp, Boolean ovlflg[], int *iret )
/************************************************************************
 * nmp_govlflg                                                          *
 *                                                                      *
 * This function gets an array of overlay flag settings for a loop.	* 
 *                                                                      *
 * void nmp_govlflg ( lp, ovlflg, iret )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp			int		loop number		*
 *									*
 * Output parameters:                                                   *
 *      ovlflg[]		Boolean         Array of overlay flag 	*
 *      *iret           	int             Return code             * 
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 * M. Li/GSC		01/01	Added a check for lp out of range       *
 ***********************************************************************/
{
int     ii;

/*---------------------------------------------------------------------*/
    *iret = 0;
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }

    if ( overlay[lp].novl <= 0 ) {
        *iret = -2;
    }
    else {
        for (ii = 0; ii < overlay[lp].novl; ii++) {
            ovlflg[ii]  = overlay[lp].mapovl[ii].active;
        }
    }

}
            
/*=====================================================================*/
