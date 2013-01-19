#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_wfld ( FILE *fp, shp_record *onerec, int *iret )
/************************************************************************
 * shp_wfld                                                             *
 *                                                                      *
 * This function writes one shape record's fields.          		*
 *                                                                      *
 * shp_wfld ( fp, onerec, iret )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Output file pointer             *
 *      *onerec         shp_record      One shape record                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                       -1 = Error                     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    int ifld;
/*---------------------------------------------------------------------*/
    *iret = 0;

    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        fprintf ( fp, "Field %s:  %s\n", onerec->fields[ifld].name,
                                         onerec->fields[ifld].data );
    }
}
