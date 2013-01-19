#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

static	int	_icol = -1;


void clo_sortbnd ( char *locnam, int icol, int *iret )
/************************************************************************
 * clo_sortbnd								*
 *									*
 * This function sorts bounds structures.				*
 *									*
 * clo_sortbnd (locnam, icol, iret) 					*
 *									*
 * Input parameters:							*
 *      *locnam         char       	Data location name		*
 *	icol		int		Column number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= 0 - normal			*
 *					= >0 - >maxret stations availbl	*
 *					= -2 - unable to match station	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 ***********************************************************************/
{
int	which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( locnam );

    if ( which == whichBnd && icol == sortBnd )  return;

    /*
     *  Sort the structure using icol as the determinant.
     *  Added (int(*)(const void*, const void*)) cast to satisfy qsort
     */

    _icol = icol;

    qsort( clo.loc[which].bnd.bound, (size_t)clo.loc[which].bnd.nbnd, 
	   sizeof(struct binfo_t), (int(*)(const void*, const void*))bnd_sort );

    whichBnd = which;
    sortBnd = icol;

}

/*=====================================================================*/

int bnd_sort ( struct binfo_t *bnd1, struct binfo_t *bnd2 )
/************************************************************************
 * bnd_sort                                                     	*
 *                                                                      *
 * This function compares strings within a bnd structure.		*
 *                                                                      *
 * int bnd_sort  ( bnd1, bnd2 )                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 * *bnd1	struct binfo_t		struct binfo_t element       	*
 * *bnd2	struct binfo_t		struct binfo_t element       	*
 *                                                                      *
 * Output parameters:                                                   *
 * bnd_sort	int          		Return code (ala strcmp)        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      7/00                                           *
 * D.W.Plummer/NCEP     09/03	Added check for equal max lons		*
 ***********************************************************************/
{
    int ival;
/*---------------------------------------------------------------------*/

    switch ( _icol )  {

	case	BND_NAME:	/* Boundary name			*/

    	    ival = strcmp( bnd1->name, bnd2->name );

	    break;

	case	BND_STREC:	/* Starting record			*/

    	    ival = ( bnd1->strec < bnd2->strec ) ? -1 : 1 ;

	    break;

	case	BND_CLAT:	/* Centroid latitude (S -> N)		*/

    	    ival = ( bnd1->cenlat < bnd2->cenlat ) ? -1 : 1 ;

	    break;

	case	BND_CLON:	/* Centroid longitude (E -> W)		*/

    	    ival = ( bnd1->cenlon > bnd2->cenlon ) ? -1 : 1 ;

	    break;

	case	BND_MNLAT:	/* Minimum latitude (S -> N)		*/

    	    ival = ( bnd1->minlat < bnd2->minlat ) ? -1 : 1 ;

	    break;

	case	BND_MXLON:	/* Maximum longitude (E -> W)		*/

	    if ( bnd1->maxlon >  bnd2->maxlon )  ival = -1;
	    if ( bnd1->maxlon <  bnd2->maxlon )  ival =  1;
	    if ( G_DIFF(bnd1->maxlon, bnd2->maxlon) )  {
		/*
		 * If max lons are equal, use min lats
		 */
		if (  bnd1->minlat <  bnd2->minlat )  ival = -1;
		if (  bnd1->minlat >  bnd2->minlat )  ival =  1;
		if (  G_DIFF(bnd1->minlat, bnd2->minlat ) )
		    /*
		     * If min lats are equal, use name
		     */
		    ival = strcmp( bnd1->name, bnd2->name );
	    }

	    break;

	default:		/* DEFAULT -- do nothing		*/

	    break;

    }

    return (ival);
}
