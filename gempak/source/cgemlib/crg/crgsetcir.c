#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_setcir ( VG_DBStruct *el, int joffset, int elnum,
		 float minlat, float minlon, float maxlat,
		 float maxlon, int *iret )
/************************************************************************
 * crg_setcir                                                           *
 *                                                                      *
 * This function sets the range for a circle element.			*
 *                                                                      *
 * crg_setcir ( el, joffset, elnum, minlat, minlon, maxlat, maxlon,	*
 *					iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing circle	*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *	minlat		float		Minimum latitude		*
 *	minlon		float		Minimum longitude		*
 *	maxlat		float		Maximum latitude		*
 *	maxlon		float		Maximum longitude		*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		12/98		Modified from crg_setsym        *
 * G. Krueger/EAI	 5/99		Corrected for coord rotation	*
 * G. Krueger/EAI	 1/00		Fixed circles at dateline/poles	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 ***********************************************************************/
{
    float       rx[4], ry[4];
    float	srx[4], sry[4];
    float	llx, lly, urx, ury;
    int 	ier, np, ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Convert points in element to device...
 */

    np = 4;
    rx[0] = minlat;
    ry[0] = minlon;
    rx[1] = maxlat;
    ry[1] = maxlon;
    rx[2] = minlat;
    ry[2] = maxlon;
    rx[3] = maxlat;
    ry[3] = minlon;
/*
 *	If the whole globe fits within the bounds, this is a circle that
 *	encompasses a pole.  In that case, we explicitly specify four
 *	equidistant points around the equator so that the circle will
 *	be fully contained within the resulting box.
 */
    if ( ( ( G_DIFFT(minlat,      0.0F, GDIFFD) && G_DIFFT(maxlat,  90.0F, GDIFFD) ) ||
	   ( G_DIFFT(minlat,  (-90.0F), GDIFFD) && G_DIFFT(maxlat,   0.0F, GDIFFD) ) ) &&
	   ( G_DIFFT(minlon, (-180.0F), GDIFFD) && G_DIFFT(maxlon, 180.0F, GDIFFD) ) ) {
	rx[0] = 0.F;
	ry[0] = -90.F;
	rx[1] = 0.F;
	ry[1] = 0.F;
	rx[2] = 0.F;
	ry[2] = 90.F;
	rx[3] = 0.F;
	ry[3] = 180.F;
    }

    gtrans(sys_M, sys_D, &np, rx, ry, srx, sry, &ier,
			strlen(sys_M), strlen(sys_D));
 
/* 
 *  Set range points based on the dimension of the element 
 */

    llx = srx[0];
    lly = sry[0];
    urx = srx[0];
    ury = sry[0];
    for (ii = 1; ii < 4; ii++) {
	if ( srx[ii] < llx ) llx = srx[ii];
	if ( sry[ii] < lly ) lly = sry[ii];
	if ( srx[ii] > urx ) urx = srx[ii];
	if ( sry[ii] > ury ) ury = sry[ii];
    }
    lly -= (float)EXTRA_SM;
    llx -= (float)EXTRA_SM;
    urx += (float)EXTRA_SM;
    ury += (float)EXTRA_SM;

/*
 *  Store the device coordinates in the range array.
 */

    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);

}
