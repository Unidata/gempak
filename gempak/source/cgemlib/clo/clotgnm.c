#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t		clo;

void clo_tgnm ( char *name, int maxnms, int maxchar, int *n_nms, char *nm, 
								int *iret )
/************************************************************************
 * clo_tgnm								*
 *									*
 * This function returns all parameter information currently indexed    *
 * by the current hotlist.  Note length of nm string array.		*
 * Attm, this function applies only to standard station tables.		*
 *									*
 * clo_tgnm  ( name, maxnms, maxchar, n_nms, nm, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char	Data name				*
 *	maxnms		int	Maximum ids   allowed in the ret arrays	*
 *	maxchar		int	Maximum chars allowed in the ret arrays	*
 *									*
 * Output parameters:							*
 *	*n_nms		int	# of indices in hotlist			*
 *	*nm		char	Station id string			*
 *	*iret		int	Return code				*
 **									*
 * Log:	          							*
 * H. Zeng/EAI          03/00   Copied from clotgid.c                   *
 * D.W.Plummer/NCEP	 8/00	Updated for CLO library changes		*
 * D.W.Plummer/NCEP	 7/01	bug fix for number of items returned	*
 ***********************************************************************/
{
int	ii, which, mx1;
char	*stn_nm;
/*---------------------------------------------------------------------*/
    nm[0] = '\0';
    *n_nms = 0;
    *iret = 0;

    mx1 = maxchar - 1;

    which = clo_which ( name );

    switch ( clo.loc[which].format )  {

        case 0:			/*  Standard station table format	*/

	    for ( ii = 0; ii < nhot; ii++ )  {

	        stn_nm = clo.loc[which].stn.station[hotlist[ii]].desc;

	        if ( ii < maxnms &&
			(int)(strlen(nm)+strlen(stn_nm)) < mx1 )  {
	            strcat( nm, stn_nm );
	            if ( ii < nhot-1 )  strcat( nm, ";" );
	        }

            }
    	    *n_nms = G_MIN( nhot, maxnms );

	    break;

    }

}

