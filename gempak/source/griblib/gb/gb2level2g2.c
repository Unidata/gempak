#include "gb2def.h"

void gb2_level2g2( int vcord, char *wmolvltbl, char *lcllvltbl,
                   int wmover, int lclver, char *wmocntr, 
                   G2level *tblentry, int *iret )
/************************************************************************
 * gb2_level2g2								*
 *									*
 * This routine returns the Grib2 vertical coordinate table entry that  *
 * matches the given vertical coordinate.                               *
 *									*
 * If either wmolvltbl or lcllvltbl are NULL, the default tables are    *
 * read.                                                                *
 *									*
 * gb2_level2g2 ( vcord, wmolvltbl, lcllvltbl, wmover, lclver, wmocntr, *
 *                tblentry, iret )                                      *
 *									*
 * Input parameters:							*
 *      vcord           int             4 byte vert. coord. abbrev.     *
 *      *wmolvltbl      char            WMO vert. coord. table          *
 *      *lcllvltbl      char            Local vert. coord. table        *
 *	wmover  	int             WMO table version number        *
 *	lclver  	int             Local table version number      *
 *      *wmocntr        char            Originating center abbreviation *
 *									*
 * Output parameters:							*
 *	*tblentry       G2level		GRIB2 vert. coord. table entry  *
 *	*iret		int		return code			*
 *                                          1 = No gempak vert. coord.  *
 *                                              defined for this grid   *
 **									*
 * Log:									*
 * S. Gilbert/NCEP      08/05                                           *
 ***********************************************************************/
{
    int     ret, ier;
    char    vcabbrev[5];

    G2lvls  *g2lvltbl;

/*---------------------------------------------------------------------*/

    *iret = 0;
    ier = -1;

    /* 
     *  Get Level info from Vertical Coordinate table(s).
     */

    if ( wmover != 255 ) {      /* try WMO vert. coord. table  */
        /*
         *  get WMO vert. coord. Table
         */
        gb2_gtwmolvltbl ( wmolvltbl, wmover, &g2lvltbl, &ier);
        if ( ier == 0 ) {
            /*
             *  Check for desired Level in table
             */
            gb2_skvcord( vcord, g2lvltbl, tblentry, &ier);
        }
    }

    /*
     *  Try local vertical coordinate table next...
     */

    if ( ier != 0 ) {
        /*
         *  get Local vert. coord. Table
         */
        gb2_gtlcllvltbl ( lcllvltbl, wmocntr, lclver, &g2lvltbl, &ier);
        if ( ier == 0 ) {
            /*
             *  Check for desired level in table
             */
            gb2_skvcord( vcord, g2lvltbl, tblentry, &ier);
        }
    }

    if ( ier != 0 ) {
        if ( ier == -30 ) {
           vcabbrev[4]='\0';
           lv_ccrd( &vcord, vcabbrev, &ret, 4 );
           er_wmsg("GB",&ier,vcabbrev,&ret,2,4);
        }
        *iret=1;
        return;
    }


}
