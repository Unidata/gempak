#include "nmpcmn.h"

void nmp_govlattr ( int ovl, int lp, int *itype, 
					nmpovlstr_t ovlattr, int *iret )
/************************************************************************
 * nmp_govlattr                                                         *
 *                                                                      *
 * This function gets the current attribute values for a given overlay	*
 * and loop.								*
 *                                                                      *
 * void nmp_govlattr ( ovl, lp, itype, ovlattr, iret )                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	ovl		int		overlay number			*
 *      lp              int             loop number             	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*itype		int		overlay type			*
 *      ovlattr		nmpovlstr_t	attribute values		*
 *      *iret           int             Return code             	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 * M. Li/GSC		12/00	introduced nmpovlstr_t			*
 * M. Li/GSC		01/01	Added check for invalid input		*
 * M. Li/GSC		03/01	Added itype				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check for invalid loop index
     */
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }


    if ( overlay[lp].novl <= 0 ) {
        *iret = -2;
	return;
    }

    /*
     * Check for invalid overlay number.
     */
    if (ovl < 0 || ovl >= overlay[lp].novl ) {
        *iret = -12;
        return;
    }

    *itype = overlay[lp].mapovl[ovl].ityp;
    strcpy ( ovlattr, overlay[lp].mapovl[ovl].attr);

}

/*=====================================================================*/
