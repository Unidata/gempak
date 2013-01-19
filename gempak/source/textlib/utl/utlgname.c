#include "geminc.h"
#include "gemprm.h"

void utl_gname ( char *acstn1, char *acnam1, char *acst1, int *iret )
/************************************************************************
 * utl_gname                                                            *
 *                                                                      *
 * This function returns an anchor point's city name and state id.  	*
 *                                                                      *
 * utl_gname ( acstn1, acnam1, acst1, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*acstn1		char		3-char Anchor point station id	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*acnam1		char		Anchor point station name	*
 *	*acst1		char		Anchor point state id		*
 *      *iret           int		Return Code                     *
 *					  -8 = Bad anchor pt stn id	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03		Copied from VFGNAME		*
 ***********************************************************************/
{
    char    stninfo[2048];
    char    hunt[1], type[7], state[3], county[30];
    int     ier, nstns, maxlen, isrch, len;
    size_t  lend, lenp;
/*---------------------------------------------------------------------*/
    *iret     = 0;
    ier       = 0;
    isrch     = 1;
    lend      = 3;
    lenp      = 32;
    hunt[0]   = '\0';
    acst1[0]  = '\0';
    acnam1[0] = '\0';
    maxlen    = sizeof(stninfo);
    strcpy (type, "ANCHOR");

    cst_lstr ( acstn1, &len, &ier );
    if ( len != 3 ) {
	*iret = -8;
	return;
    }

    /*
     * Find the anchor point's city name and state id.
     */

    clo_init ( &ier );
    clo_findstn ( type, acstn1, hunt, isrch, maxlen, &nstns, stninfo, &ier ); 

    if ( ier == 0 ) {
        cst_gtag ( "NAME", stninfo, " ", county, &ier );
        cst_gtag ( "ST", stninfo, " ", state, &ier );
        cst_rnan (county, county, &ier);

        len = G_MIN ( lenp, strlen (county) );
        cst_ncpy ( acnam1, county, len, &ier );

        len = G_MIN ( lend, strlen (state) );
        cst_ncpy ( acst1, state, len, &ier);
    }
    else {
	*iret = -8;
	return;
    }
}
