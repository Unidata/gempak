#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_ccfind ( int fips, char *ccwfo, char *ccname, int *ncfips, 
						int *cfips, int *iret )
/************************************************************************
 * ctb_ccfind								*
 *									*
 * This routine will return the number and values of clustered FIPS	*
 * given an input FIPS code.						*
 *									*
 * ctb_ccfind ( fips, ccwfo, ccname, ncfips, cfips, iret )		*
 *									*
 * Input parameters:							*
 *	fips		int	FIPS code				*
 *									*
 * Output parameters:							*
 *	*ccwfo		char	WFO 					*
 *	*ccname		char	Cluster name				*
 *	*ncfips		int	Number of FIPS codes returned in cfips	*
 *	*cfips		int	Array of FIPS codes clustered w/ input	*
 *	*iret		int	Return code				*
 *				= 0 - normal				*
 *				= 1 - FIPS code not found		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/01						*
 ***********************************************************************/
{
int	ii, jj, kk, ier;
static Clustcnty_t	cc;
static	int	ccread=0;

/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( ccread == 0 )  {

        ctb_ccrd ( CNTYCLUST_TBL, "stns", &cc, &ier );
	ccread = 1;

    }

    for ( ii = 0; ii < cc.nclust; ii++ )  {

	for ( jj = 0; jj < cc.clust[ii].ncc; jj++ )  {

	    if ( cc.clust[ii].cc[jj] == fips )  {

		strcpy ( ccwfo, cc.clust[ii].ccwfo );
		strcpy ( ccname, cc.clust[ii].ccname );
		*ncfips = cc.clust[ii].ncc;

		for ( kk = 0; kk < cc.clust[ii].ncc; kk++ )  {
		    cfips[kk] = cc.clust[ii].cc[kk];
		}

	        return;

	    }

	}

    }

    ccwfo[0] = '\0';
    ccname[0] = '\0';
    *ncfips = 0;
    *iret = 1;

}
