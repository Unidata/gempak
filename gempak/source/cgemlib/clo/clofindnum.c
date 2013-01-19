#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

void clo_findnum ( char *locnam, int num, int maxlen, int *nret, 
						char *info, int *iret )
/************************************************************************
 * clo_findnum								*
 *									*
 * This function finds station information based on a station number.	*
 *									*
 * The returned info string contains the station information as a 	*
 * string in the <TAG>value format.  Valid TAGs are:			*
 * "STID", "STNM", "NAME", "ST", "CO", "LAT", "LON", "ELV", "PRI" and	*
 * "COL10"								*
 *									*
 * clo_findnum (locnam, num, maxlen, nret, info, iret) 			*
 *									*
 * Input parameters:							*
 *      *locnam         char       	Data location name		*
 *	num		int		Station name or substring	*
 *	maxlen		int		Max length of info string	*
 *									*
 * Output parameters:							*
 *	*nret		int		Number of stations returned	*
 *	*info		char		String w/ station information	*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 *					= -2 - unable to match station	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/01	Created					*
 ***********************************************************************/
{
int	which, ihi, ilo, stptr, iptr, ier;
char	tstr[128];

Stn_t	*stn;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *nret = 0;

    /*
     *  Sort locnam stations by second column (station number)
     */
    clo_sortstn( locnam, STN_NM, &ier );

    which = clo_which ( locnam );

    stn = &(clo.loc[which].stn);

    /*
     *  Perform binary search for station number
     */
    ihi = stn->nstn - 1;
    ilo = 0;
    stptr = (ihi+ilo) / 2;
    iptr = stptr;
    while ( num != stn->station[iptr].nm && ihi > ilo )  {

	if ( num < stn->station[iptr].nm )  ihi = stptr-1;
	if ( num > stn->station[iptr].nm )  ilo = stptr+1;
    	stptr = (ihi+ilo) / 2;
	iptr = stptr;
    }

    if ( num != stn->station[iptr].nm )  {
        /*
         *  Station number not found
         */
        info[0] = '\0';
	*iret = -2;
    }
    else  {
        /*
         *  Station number found - return station info
         */
        *nret = 1;
        sprintf( tstr, 
	  "<STID>%s<STNM>%d<NAME>%s<ST>%s<CO>%s<LAT>%6.2f<LON>%6.2f<ELV>%d<PRI>%d<COL10>%s", 
	    stn->station[iptr].id, 
	    stn->station[iptr].nm, 
	    stn->station[iptr].desc, 
	    stn->station[iptr].state, 
	    stn->station[iptr].cntry, 
	    stn->station[iptr].lat, 
	    stn->station[iptr].lon,
	    stn->station[iptr].elv,
	    stn->station[iptr].pri,
	    stn->station[iptr].col10 );

        strcpy ( info, tstr );

    }

}
