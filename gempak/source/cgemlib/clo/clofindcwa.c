#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

void clo_findcwa ( char *locnam, char *cwa, int maxnum, 
		   int *nfips, int*  fips, int *iret )
/************************************************************************
 * clo_findcwa								*
 *									*
 * This function finds all the counties that belong to a certain CWA.	*
 * The county fips codes are returned in an array.			*
 *									*
 *									*
 * clo_findcwa (locnam, cwa, maxnum, nfips, fips, iret) 		*
 *									*
 * Input parameters:							*
 *      locnam          char*       	county table name		*
 *	cwa		char*		cwa name			*
 *	maxnum		int		max. num. that can be returned	*
 *									*
 * Output parameters:							*
 *	nfips		int*		actual num. that are returned	*
 *	fips		int*		county fips codes array		*
 *	iret		int*		Return code			*
 *					=  0 - normal			*
 *					= -1 - unable to match CWA	*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/06	initial coding				*
 ***********************************************************************/
{
  int	which, ii, ihi, ilo, iptr, start_idx, ier;
Stn_t	*stn;
/*---------------------------------------------------------------------*/
    *iret  = 0;
    *nfips = 0;

    /*
     *  Sort locnam stations by CWA name(column 10).
     */
    clo_sortstn( locnam, STN_COL10, &ier );

    which = clo_which ( locnam );

    stn = &(clo.loc[which].stn);

    /*
     *  Perform binary search to find a county that carries the same
     *  CWA name.
     */
    ihi = stn->nstn - 1;
    ilo = 0;
    iptr = (ihi+ilo) / 2;
    while ( strcasecmp(cwa, stn->station[iptr].col10) != 0 && ihi > ilo )  {

        if ( strcasecmp(cwa, stn->station[iptr].col10) < 0 )  ihi = iptr-1;
	if ( strcasecmp(cwa, stn->station[iptr].col10) > 0 )  ilo = iptr+1;
    	iptr = (ihi+ilo) / 2;
    }

    if ( strcasecmp(cwa, stn->station[iptr].col10) != 0 )  {

        /*
         *  Matching CWA not found
         */
	*iret = -1;
        return;
    }

    /*
     *  Matching CWA found. Find the starting index now.
     */
    while ( strcasecmp(cwa, stn->station[iptr].col10) == 0 ) {
        iptr--;
	if ( iptr == -1 )  break;
    }
    start_idx = iptr + 1;
    
    /*
     * copy all the county fips codes in need into an array.
     */
    for ( ii = start_idx; ii < stn->nstn; ii++ ) {
        if ( strcasecmp(stn->station[ii].col10, cwa) == 0 &&
	     *nfips < maxnum )  {

	    fips[*nfips] = stn->station[ii].nm;
	    (*nfips)++;
        }
        else {

	    break;
        }

    }

}
