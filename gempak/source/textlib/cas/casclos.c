#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_clos ( FILE *fptr, int *iret )
/************************************************************************
 * cas_clos								*
 *                                                                      *
 * This function closes the specified file.				*
 *                                                                      *
 * cas_clos ( fptr, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fptr 			FILE		File pointer		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           	int             Return code             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * M. Li/SAIC		05/05	Initialized gtstr			*
 ***********************************************************************/
{
    int		level, ier, ier1;
    char        grp[4], gtstr[40];

/*---------------------------------------------------------------------*/
    *iret   = 0;
    level   = 0;
    strcpy ( grp, "CFL");
    strcpy ( gtstr, "");


    cfl_clos ( fptr, &ier );    

    if ( ier != 0 ) {
        er_lmsg ( &level, grp, &ier, gtstr, &ier1,
	           strlen (grp), strlen(gtstr) );
        *iret = -2;
     }
			    

}
