#include "nmpcmn.h"

void nmp_gltln ( int lp, char *ltln_str, int *iret )
/************************************************************************
 * nmp_gltln                                                       	*
 *                                                                      *
 * This function gets LAT/LON string.                                   *
 *                                                                      *
 * void nmp_gltln ( lp, ltln_str, iret )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp       	int           	loop number                             *
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *ltln_str	char           	LAT/LON string                          *
 *  *iret	int		return code				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      	NONE                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		02/01	created					*
 ***********************************************************************/
{
int     	ii, condition, intattr[5], itype, intnum, ier;
nmpovlstr_t 	ovlattr;
/*---------------------------------------------------------------------*/
    *iret = 0;

    strcpy(ltln_str, "0");

    for ( ii = 0; ii < overlay[lp].novl; ii++ ) {
        if ( overlay[lp].mapovl[ii].ityp == 0 ) {

            /*
	     * get the attribute
	     */
	    nmp_govlattr(ii, lp, &itype, ovlattr, &ier);
	    if (ier == 0 ) {
		cst_ilst(ovlattr, ' ', 1, 5, intattr, &intnum, &ier);
	    }
	    else {
		*iret = ier;
	    }

            condition = overlay[lp].mapovl[ii].active;

            if ( condition ) {
                sprintf(ltln_str, "%d/%d/%d/%d;%d/%5.2f;%5.2f",
			intattr[0], intattr[1], intattr[2], intattr[4],
			intattr[4], (float)intattr[3], (float)intattr[3]);
            }

            break;
        }
    }

}

/*=====================================================================*/
