#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"


void crg_gbnd ( char sys_in[], char sys_out[], int nsp, 
		float sx[], float sy[], float *llx, float *lly, 
		float *urx, float *ury, float *ccx, float *ccy )
/************************************************************************
 * crg_gbnd								*
 *									*
 * This function finds the bounding box and its center for a set of	*
 * points.  Both the source points and the returned points can be in	*
 * any coordinate system.  If in map coordinates, x = lat, y = lon.	*
 *									*
 * crg_gbnd (sys_in, sys_out, nsp, sx, sy, llx, lly, urx, ury, ccx, ccy)*
 *									*
 * Input parameters:							*
 *	sys_in[]	char	source coordinate system		*
 *	sys_out[]	char	output coordinate system		*
 *	nsp		int	number of points			*
 *	sx[]		float	source x coordinates			*
 *	sy[]		float	source y coordinates			*
 *									*
 * Output parameters:							*
 *	*llx		float	lower left x coordinate			*
 *	*lly		float	lower left y coordinate			*
 *	*urx		float	upper right x coordinate		*
 *	*ury		float	upper right y coordinate		*
 *	*ccx		float	center x coordinate			*
 *	*ccy		float	center y coordinate			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		03/00	reworked from crg_setltln		*
 * A. Hardy/GSC         11/00   renamed output coord. system declaration*
 ***********************************************************************/
{
    int         ii, ier, nop = 3;
    float	*sdx, *sdy, odx[3], ody[3], ox[3], oy[3];
/*---------------------------------------------------------------------*/

    G_CALLOC(sdx, float, nsp, "crg_gbnd:  sdx");
    G_CALLOC(sdy, float, nsp, "crg_gbnd:  sdy");

/*
 * convert points to device coordinate system
 */
    if ((strcmp (sys_in, sys_D)) == 0) {
	for (ii = 0; ii < nsp; ii++) {
	    sdx[ii] = sx[ii];
	    sdy[ii] = sy[ii];
	}
    }
    else {
	gtrans (sys_in, sys_D, &nsp, sx, sy, sdx, sdy, &ier,
		strlen (sys_in), strlen (sys_D));
    }

    /*
     * find extremes
     */
    for (ii = 0; ii < nsp; ii++) {
	if (ii == 0 || sdx[ii] < odx[0])  odx[0] = sdx[ii];
	if (ii == 0 || sdx[ii] > odx[1])  odx[1] = sdx[ii];
	if (ii == 0 || sdy[ii] < ody[0])  ody[0] = sdy[ii];
	if (ii == 0 || sdy[ii] > ody[1])  ody[1] = sdy[ii];
    }

    /*
     * find center
     */
    odx[2] = (odx[0] + odx[1]) / 2.0F;
    ody[2] = (ody[0] + ody[1]) / 2.0F;

    /*
     * convert points to output coordinate system
     */
    if ((strcmp (sys_D, sys_out)) == 0) {
	*llx = odx[0];
	*lly = ody[0];
	*urx = odx[1];
	*ury = ody[1];
	*ccx = odx[2];
	*ccy = ody[2];
    }
    else {
	gtrans (sys_D, sys_out, &nop, odx, ody, ox, oy, &ier,
		strlen (sys_D), strlen (sys_out));

	*llx = ox[0];
	*lly = oy[0];
	*urx = ox[1];
	*ury = oy[1];
	*ccx = ox[2];
	*ccy = oy[2];
    }
    G_FREE(sdx, float);
    G_FREE(sdy, float);
}
