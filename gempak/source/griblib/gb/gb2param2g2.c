#include "gb2def.h"

void gb2_param2g2( char *param, char *wmovartbl, char *lclvartbl,
                   int wmover, int lclver, char *wmocntr, 
                   G2Vinfo *tblentry, int *iret )
/************************************************************************
 * gb2_param2g2								*
 *									*
 * This routine returns the Grib2 parameter table entry that matches    *
 * a given parameter string.
 *									*
 * If either wmovartbl or lclvartbl are NULL, the default tables are    *
 * read.                                                                *
 *									*
 * gb2_param2g2 ( param, wmovartbl, lclvartbl, wmover, lclver, wmocntr, *
 *                tblentry, iret )                                      *
 *									*
 * Input parameters:							*
 *      *param          char            12 character parameter name.    *
 *      *wmovartbl      char            WMO parameter table             *
 *      *lclvartbl      char            Local parameter table           *
 *	wmover  	int             WMO table version number        *
 *	lclver  	int             Local table version number      *
 *      *wmocntr        char            Originating center abbreviation *
 *									*
 * Output parameters:							*
 *	*tblentry       G2Vinfo		GRIB2 parameter table entry     *
 *	*iret		int		return code			*
 *                                          1 = No gempak param name    *
 *                                              defined for this grid   *
 **									*
 * Log:									*
 * S. Gilbert/NCEP      12/04                                           *
 ***********************************************************************/
{
    int     ret, ier;

    G2vars_t  *g2vartbl;

/*---------------------------------------------------------------------*/

    *iret = 0;
    ier = -1;

    /* 
     *  Get Parameter into from Paramter table(s).
     */

    if ( wmover != 255 ) {      /* try WMO parameter table  */
        /*
         *  get WMO parameter Table
         */
        gb2_gtwmovartbl ( wmovartbl, wmover, &g2vartbl, &ier);
        if ( ier == 0 ) {
            /*
             *  Check for desired parameter in table
             */
            gb2_skparam( param, g2vartbl, tblentry, &ier);
        }
    }

    /*
     *  Try local parameter table next...
     */

    if ( ier != 0 ) {
        /*
         *  get Local parameter Table
         */
        gb2_gtlclvartbl ( lclvartbl, wmocntr, lclver, &g2vartbl, &ier);
        if ( ier == 0 ) {
            /*
             *  Check for desired parameter in table
             */
            gb2_skparam( param, g2vartbl, tblentry, &ier);
        }
    }

    if ( ier != 0 ) {
        if ( ier == -32 ) {
           er_wmsg("GB",&ier,param,&ret,2,strlen(param));
        }
        *iret=1;
        return;
    }


}
