#include "geminc.h"
#include "gemprm.h"

void cpcg_rdbnd ( char *bndtyp, int *nbnd, int npts[], 
		float blat[][LLMXPT], float blon[][LLMXPT], int *iret ) 
/************************************************************************
 * cpcg_rdbnd								*
 *                                                                      *
 * This function reads a bound area and returns the lat/lon. 		* 
 *                                                                      *
 * cpcg_rdbnd(bndtyp, nbnd, npts, blat, blon, iret)			* 
 *                                                                      *
 * Input parameters:                                                    *
 *      *bndtyp         char            Bounds type			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*nbnd			int	number of bounds areas		*
 *	npts[nbnd]		int	number of points in bound area 	*
 *	blat[nbnd][LLMXPT]	float	latitude of bounds		*
 *	blon[nbnd][LLMXPT]	float	longitude of bound		*
 *      *iret           	int     Return code                     *
 *					-4 = no bounds found		*
 *									*	
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/01                                           *
 ***********************************************************************/
{
int     ii, mxpts, npt, minpts, nBnd, ier;
float   filter, rlat, rlon, dlat, dlon, slat[LLMXPT], slon[LLMXPT];
/*---------------------------------------------------------------------*/
    *iret = 0;

    rlat = 10.0;
    rlon = -180;
    dlat = 75.0;
    dlon = -55.0;

    clo_init(&ier);

    /*
     * set bounds type and area
     */
    clo_bstype(bndtyp, &ier);
    clo_bsarea(&rlat, &rlon, &dlat, &dlon, &ier);

    ier = 0;
    nBnd = 0;
    minpts = 0;
    filter = 0.0;
    mxpts = LLMXPT;
    while ( ier == 0 ) {
        clo_bgnext(&minpts, &mxpts, &filter, &npt, slat, slon, &ier);

        if (ier == 0) {
            for (ii = 0; ii < npt; ii++) {
	        blat[nBnd][ii] = slat[ii];
	        blon[nBnd][ii] = slon[ii];
            }

	    npts[nBnd] = npt;
	    nBnd++;
        }
    }

    *nbnd = nBnd;
    if (nBnd <= 0) *iret = -4;

}
