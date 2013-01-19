#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t		clo;

void clo_tgid ( char *name, int maxids, int maxchar, int *nids, char *id, 
								int *iret )
/************************************************************************
 * clo_tgid								*
 *									*
 * This function returns all parameter information currently indexed    *
 * by the current hotlist.  Note length of id string array.		*
 * Attm, this function applies only to standard station tables.		*
 *									*
 * clo_tgid  ( name, maxids, maxchar, nids, id, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char	Data name				*
 *	maxids		int	Maximum ids   allowed in the ret arrays	*
 *	maxchar		int	Maximum chars allowed in the ret arrays	*
 *									*
 * Output parameters:							*
 *	*nids		int	# of indices in hotlist			*
 *	*id		char	Station id string			*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/97	Created					*
 * F. J. Yen/NCEP	 9/98	Added type SFSTN (surface station)	*
 * D.W.Plummer/NCEP	12/98	Added type CITY and changed type of 	*
 *				output id from char[][8] to *char	*
 * D.W.Plummer/NCEP	 1/99	LOC structure name changes; add 	*
 *				universal LOC hotlist			*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL types	*
 * D.W.Plummer/NCEP	 8/99	Updated for CLO library changes		*
 * D.W.Plummer/NCEP	10/00	Bug fix in for loop test		*
 ***********************************************************************/
{
int	i, which, mx1;
char	*stnid;
/*---------------------------------------------------------------------*/
    id[0] = '\0';
    *nids = 0;
    *iret = 0;

    mx1 = maxchar - 1;

    which = clo_which ( name );

    switch ( clo.loc[which].format )  {

        case 0:			/*  Standard station table format	*/

	    for ( i = 0; i < nhot; i++ )  {

		stnid = clo.loc[which].stn.station[hotlist[i]].id;

	        if ( i < maxids &&
		  (int)(strlen(id)+strlen(stnid)) < mx1 )  {
	            strcat( id, stnid );
	            strcat( id, ";" );
	        }

            }
    	    *nids = G_MIN( nhot, maxids );

	    break;

    }

}
