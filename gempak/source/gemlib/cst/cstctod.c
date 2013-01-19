#include "geminc.h"
#include "gemprm.h"

void cst_ctod ( char *str, double *dblptr, int *iret )
/************************************************************************
 * cst_ctod                                                             *
 *                                                                      *
 * This subroutine converts string latitude and longitude in format     *
 * of degrees:minutes or decimal degrees into double                    *
 *                                                                      *
 * cst_ctod ( str, dblptr, iret )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *      *str            char            Pointer to string lat/lon       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *dblptr         double          Pointer to double value         *
 *      *iret           int             Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = conversion error          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         1/02                                            *
 ***********************************************************************/
{
    long int longint;
    char *dlmptr = NULL;

/*---------------------------------------------------------------------*/

    if ( str == NULL || dblptr == NULL || iret == NULL ) {
	*iret = -1;
        return;
    }

    *iret = 0;

    if( (dlmptr = strchr(str, ':')) ) {
/*
 * Input in degrees:minutes format.
 * Check input format correctness.  For minutes, it has to be exactly
 * 2 digits except when minutes is zero.  For degrees, because latitude
 * and longitude have different valid range, it can not be given
 * a unique check.  Check shoud be done outside of the function.
 */
        if( (strlen(dlmptr+1) != (size_t)2) && (strtol(dlmptr+1, (char **)NULL, 10) != 0) ) {
            *iret = -1;
            *dblptr = (double)RMISSD;
        }
        else {
            longint = strtol(str, &dlmptr, 10) + ((str[0] == '-') ? -1 : 1) 
                      * strtol(dlmptr+1, (char **)NULL, 10)/60;
	    *dblptr = (double)longint;
            if(dlmptr && *dlmptr != ':') {
/*
 * Input string has garbage
 */
                *iret = -1;
                *dblptr = (double)RMISSD;
            }
        }
    }
    else {
/*
 * Input in decimal degrees format
 */
        *dblptr = strtod(str, &dlmptr); 
        
        if(dlmptr && *dlmptr != '\0') {
/*
 * Input string has garbage
 */
            *iret = -1;
            *dblptr = (double)RMISSD;
        }
    }
}
