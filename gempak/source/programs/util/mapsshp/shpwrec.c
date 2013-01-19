#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_wrec ( FILE *fp, shp_record *onerec, int *iret )
/************************************************************************
 * shp_wrec                                                             *
 *                                                                      *
 * This function writes one shape record's data.                        *
 *                                                                      *
 * shp_wrec ( fp, onerec, iret )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Output file pointer             *
 *      *onerec         shp_record      One shape record		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_part *curprt;
    int iprt;
/*---------------------------------------------------------------------*/
    *iret = 0;

    for ( curprt = onerec->shpart, iprt = 0; iprt < onerec->numprt;
        iprt++, curprt = curprt->nxtprt ) {

        shp_wprt ( fp, curprt, iret );
    }
}
