#include "gb2def.h"

void gb2_stat( gribfield *gfld, char *param )
/************************************************************************
 * gb2_stat                                                             *
 *									*
 * This function adds statistical info to the GEMPAK parameter string.  *
 *									*
 * gb2_stat( gfld, param )						*
 *									*
 * Input parameters:							*
 *	*gfld		struct gribfield	Decoded GRIB2 structure *
 *									*
 * Input/Output parameters:						*
 *      *param          char            GEMPAK Parameter string         *
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Copied from gb2_prob			*
 ***********************************************************************/
{
    int	stat_type;
    char cpds[32];
    int	ext_flag, num;

/*---------------------------------------------------------------------*/

    ext_flag = 0;
    cpds[0] = '\0';

    switch ( gfld->ipdtnum ) {

        case  15 :		/* Statistical fcst */
            ext_flag = 1;
            stat_type = gfld->ipdtmpl[15];
            switch (stat_type) {
                case 0 :
                    strcpy ( cpds, "AVG" );
                    break;
                case 2 :
                    strcpy ( cpds, "MAX" );
                    break;
                case 3 :
                    strcpy ( cpds, "MIN" );
                    break;
            }
            break;
    }
    if ( ext_flag == 1 ) {
	if ( strlen(param) + strlen(cpds) > 12 ) {
	    num = 12 - strlen(param);
	    strncat ( param, cpds, (size_t)num );
	}
	else {
	    strcat ( param, cpds );
	}
    }
    return;
}
