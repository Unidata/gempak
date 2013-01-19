#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_permccfind ( int fips, char *pcwfo, char *pcname, int *ncfips, 
						int *cfips, int *iret )
/************************************************************************
 * ctb_permccfind							*
 *									*
 * This routine will return the number and values of permanent clustered*
 * FIPS given an input FIPS code. Only the first FIPS code on each line *
 * will be compared to the input code. When a match is found. that line *
 * of information is returned.						*
 *									*
 * ctb_permccfind ( fips, ccwfo, pcname, ncfips, cfips, iret )		*
 *									*
 * Input parameters:							*
 *	fips		int	FIPS code				*
 *									*
 * Output parameters:							*
 *	*pcwfo		char	WFO 					*
 *	*pcname		char	Permanent cluster name			*
 *	*ncfips		int	Number of FIPS codes returned in cfips	*
 *	*cfips		int	Array of FIPS codes clustered w/ input	*
 *	*iret		int	Return code				*
 *				= 0 - normal				*
 *				= 1 - FIPS code not found		*
 **									*
 * Log:									*
 * A. Hardy/NCEP	10/04	Modified from ctb_ccfind		*
 * A. Hardy/NCEP	10/04	Modified to use 1st code on each line	*
 ***********************************************************************/
{
int			ii, kk, ier;
static Permclust_t	pc;
static	int		pcread=0;

/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( pcread == 0 )  {

        ctb_permccrd ( PERMCLUST_TBL, "stns", &pc, &ier );
	pcread = 1;

    }

    for ( ii = 0; ii < pc.nclust; ii++ )  {

        if ( pc.clust[ii].pc[0] == fips )  {

	    strcpy ( pcwfo, pc.clust[ii].pcwfo );
	    strcpy ( pcname, pc.clust[ii].pcname );
	    *ncfips = pc.clust[ii].npc;

	    for ( kk = 0; kk < pc.clust[ii].npc; kk++ )  {
	        cfips[kk] = pc.clust[ii].pc[kk];
	    }

	    return;

	}
    }

    pcwfo[0] = '\0';
    pcname[0] = '\0';
    *ncfips = 0;
    *iret = 1;

}
