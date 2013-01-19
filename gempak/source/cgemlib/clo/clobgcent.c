#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bgcent ( float *clat, float *clon, int *iret )
/************************************************************************
 * clo_bgcent                                                           *
 *                                                                      *
 * This function returns the bound centroid.				*
 *                                                                      *
 * clo_bgcent ( clat, clon, iret )            				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *clat           float   Centroid latitude                       *
 *      *clon           float   Centroid longitude                      *
 *      *iret           int     Return code                             *
 *                              = 0  - normal                           *
 *                              = -1 - tag not found 			*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      9/01   Created                                 *
 ***********************************************************************/
{
Bnd_t   *bptr;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Set a pointer to the proper bounds structure.
     */
    bptr = &(clo.loc[whichBnd].bnd);

    while ( boundBnd < bptr->nbnd )  {

	if ( clo_bqtag( bptr->bound[boundBnd].info ) )  {

    	    *clat = bptr->bound[boundBnd].cenlat;
    	    *clon = bptr->bound[boundBnd].cenlon;
	    boundBnd++;
	    return;

	}

	boundBnd++;

    }

    *iret = -1;

}
