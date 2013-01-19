#include "nmpcmn.h"

void nmp_sovlflg ( int lp, int ovl, Boolean flg, int *iret )
/************************************************************************
 * nmp_sovlflg                                                          *
 *                                                                      *
 * This function sets the overlay flag for a specific loop and overlay  *
 *                                                                      *
 * void nmp_sovlflg ( lp, ovl, flg, iret )                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *      lp                      int             loop number             *
 *	ovl			int		overlay index		*
 *	flg			Boolean		overlay flag		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret                   int             Return code             *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            03/01   Created                                 *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    *iret = 0;
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }

    if ( overlay[lp].novl <= 0 ) {
        *iret = -2;
	return;
    }

    overlay[lp].mapovl[ovl].active = flg;

}

/*=====================================================================*/
