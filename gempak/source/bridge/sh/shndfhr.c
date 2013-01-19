#include "geminc.h"
#include "gemprm.h"
#include "shncmn.h"
#include "../dc/dccmn.h"

#define LOG_ERROR(spname) dc_wclg( 2, "DC", 2, "Bad return from " #spname, &ier )

void shn_dfhr ( int *idst, float *rlat, float *rlon, int *idiff, int *iret )
/************************************************************************
 * shn_dfhr								*
 *									*
 * Given a location defined by a latitude/longitude pair, this function	*
 * determines the offset (in hours) between GMT and the local time at	*
 * that location.							*
 *									*
 * shn_dfhr ( *idst, *rlat, *rlon, *idiff, *iret )			*
 *									*
 * Input parameters:                                                    *
 *	*idst		int		Flag denoting whether the	*
 *					current	report date-time occurs	*
 *					during Daylight	Savings Time:	*
 *					   0 = No			*
 *					   1 = Yes			*
 *	*rlat		float		Latitude of location		*
 *	*rlon		float		Longitude of location		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*idiff		int		Difference (in hours) determined*
 *					by subtracting GMT from the	*
 *					local time at the given location*
 *	*iret		int		Return code:			*
 *					   0 = normal return		*
 *					  -1 = a problem occurred	*
 **                                                                     *
 *  Log:                                                                *
 *  J. Ator/NCEP         04/05                                          *
 ***********************************************************************/
{
	char cbndtp[8] = "TZ_BNDS";
	char cwrkst[80];
	char info[128];

	int ier;

	*iret = -1;

	clo_tqbnd ( cbndtp, *rlat, *rlon, cwrkst, &ier );
	if ( ier != 0 ) {
	    LOG_ERROR(clo_tqbnd);
	    return;
	}

	clo_bginfo ( cbndtp, 0, info, &ier ); 
	if ( ier != 0 ) {
	    LOG_ERROR(clo_bginfo);
	    return;
	}

	if ( *idst ) {
	    cst_gtag ( "GMT_DST_OFFSET", (const char *) info, " ", cwrkst, &ier );
	}
	else {
	    cst_gtag ( "GMT_OFFSET"    , (const char *) info, " ", cwrkst, &ier );
	}
	if ( ier != 0 ) {
	    LOG_ERROR(cst_gtag);
	    return;
	}

	cst_numb ( cwrkst, idiff, &ier );
	if ( ier != 0 ) {
	    LOG_ERROR(cst_numb);
	    return;
	}

	*iret = 0;
	return;
}
