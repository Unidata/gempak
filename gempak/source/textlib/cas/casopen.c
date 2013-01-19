#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

FILE *cas_open ( char *ofname, Boolean readflg, int *iret )
/************************************************************************
 * cas_open								*
 *                                                                      *
 * This function opens a specified file for reading or writing.		*
 *                                                                      *
 * FILE *cas_open ( ofname, readflg, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ofname 		char		Output file name	*
 *      readflg			Boolean		Open read or write flag *
 *					           True = read		*
 *					           False = write	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           	int             Return code             *
 *	*cas_open		FILE		file id			*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * M. Li/SAIC		05/05	Replaced gtstr with ofname		*
 ***********************************************************************/
{
    int		level, ier, ier1;
    char        grp[4];
    FILE        *fpout;

/*---------------------------------------------------------------------*/
    *iret   = 0;
    level   = 0;
    strcpy ( grp, "CFL");

    if ( readflg ) {
        fpout = cfl_ropn ( ofname, NULL, &ier );
    }
    else {
        fpout = cfl_wopn ( ofname, &ier );
    }


    if ( ier != 0 ) {
	er_lmsg ( &level, grp, &ier, ofname, &ier1,
	          strlen (grp), strlen(ofname) );
        *iret = -1;
    }

    return fpout;
}
