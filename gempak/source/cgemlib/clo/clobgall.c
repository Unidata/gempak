#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bgall ( char *name, int maxbnd, int *nbnd, char *bndname, 
	float *clat, float *clon, int *nparts, char *info, int *iret )
/************************************************************************
 * clo_bgall								*
 *									*
 * This function returns all parameter information currently indexed	*
 * by the bounds hotlist.						*
 * Note that if maxbnd > 1, returned strings have information for each	*
 * bound separated by the "|" character.				*
 *									*
 * clo_bgall ( name, maxbnd, 						*
 * 	       nbnd, bndname, clat, clon, nparts, info, iret )		*
 *									*
 * Input parameters:							*
 *	*name		char	Bound name				*
 *	maxbnd		int	Maximum allowed in the returned arrays	*
 *									*
 * Output parameters:							*
 *	*nbnd		int	# of indices in hotlist			*
 *	*bndname	char	Bound name				*
 *	*clat		float	Central Latitude			*
 *	*clon		float	Central Longitude			*
 *	*nparts		int	Number of parts				*
 *	*info		char	Metainformation of bound		*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 ***********************************************************************/
{
int	ii, which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( name );

    bndname[0] = '\0';
    info[0] = '\0';

    for ( ii = 0; ii < nhot; ii++ )  {

	if ( ii <= maxbnd )  {

	    strcat ( bndname, clo.loc[which].bnd.bound[hotlist[ii]].name );
	    strcat ( bndname, "|" );
	    clat[ii] = clo.loc[which].bnd.bound[hotlist[ii]].cenlat;
	    clon[ii] = clo.loc[which].bnd.bound[hotlist[ii]].cenlon;
	    nparts[ii] = clo.loc[which].bnd.bound[hotlist[ii]].nparts;
	    strcat ( info, clo.loc[which].bnd.bound[hotlist[ii]].info );
	    strcat ( info, "|" );

	}

    }

    *nbnd = G_MIN( nhot, maxbnd );

}
