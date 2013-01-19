#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bgrange ( float *minlat, float *minlon, float *maxlat,
		   float *maxlon, int *iret )
/************************************************************************
 * clo_bgrange                                                          *
 *                                                                      *
 * This function returns the range for the current bound part.		*
 *                                                                      *
 * clo_bgrange ( minlat, minlon, maxlat, maxlon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      NONE								*
 *                                                                      *
 * Output parameters:                                                   *
 *      *minlat	float	Minimum latitude				*
 *      *minlon	float	Minimum longitude				*
 *      *maxlat	float	Maximum latitude				*
 *      *maxlon	float	Maximum longitude				*
 *      *iret           int     Return code                             *
 *                              = 0  - normal                           *
 *                              = -1 - no bound part available		*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/02   Created                                 *
 ***********************************************************************/
{
Bnd_t   *bptr;

/*---------------------------------------------------------------------*/

    *iret = -1;
    *minlat = RMISSD;
    *minlon = RMISSD;
    *maxlat = RMISSD;
    *maxlon = RMISSD;

    if ( boundBnd >= 0 && bndsptBnd >= 0 )  {

        /*
         *  Set a pointer to the proper bounds structure.
         */
        bptr = &(clo.loc[whichBnd].bnd);

	if ( boundBnd < bptr->nbnd )  {

	  if ( bndsptBnd < bptr->bound[boundBnd].nparts )  {

	    *minlat = bptr->bound[boundBnd].bndspt[bndsptBnd].minlat;
	    *minlon = bptr->bound[boundBnd].bndspt[bndsptBnd].minlon;
	    *maxlat = bptr->bound[boundBnd].bndspt[bndsptBnd].maxlat;
	    *maxlon = bptr->bound[boundBnd].bndspt[bndsptBnd].maxlon;

            *iret = 0;

	  }

	}

    }

}
