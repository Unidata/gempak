#include "crgcmn.h"


void crg_grfrsh ( float in_llx, float in_lly, float in_urx, float in_ury, 
	 	float *llx, float *lly, float *urx, float *ury, int *iret )
/************************************************************************
 * crg_grfrsh                                                           *
 *                                                                      *
 * This function scans the range record using the input range  		*
 * information for intersecting range areas and returns the minimum	*
 * area that includes the input area and all intersecting areas.	* 
 *                                                                      *
 * crg_grfrsh ( in_llx, in_lly, in_urx, in_ury, 			*
 *		 	llx, lly, urx, ury, iret)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	in_llx		float		Lower left x			*
 *	in_lly		float		Lower left y			*
 *	in_urx		float		Upper right x			*
 *	in_ury		float		Upper right y			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *llx            float           Lower left x                    *
 *      *lly            float           Lower left y                    *
 *      *urx            float           Upper right x                   *
 *      *ury            float           Upper right y                   *
 *      *iret           int             Return code (always 0)          *
 * 									* 
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/98	initial coding                          *
 * T. Piper/GSC		10/98	Prolog update				*
 * J. Wu/SAIC		01/02	add layer param to crg_get()		*
 * E. Safford/SAIC	04/02	add check on ier from crg_get		*
 * J. Wu/SAIC		07/04	add filter param to crg_get()		*
 ***********************************************************************/
{
int	location, ii, ier, el_layer;	
float	fllx, flly, furx, fury;
filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *llx = in_llx;
    *lly = in_lly;
    *urx = in_urx;
    *ury = in_ury;

    for (ii=0; ii < MAX_EDITABLE_ELEMS; ii++) {
	crg_goffset (ii, &location, &ier);

	if (location >= 0) {
	    crg_get (ii, &el_layer, filter, &fllx, &flly, &furx, &fury, &ier);

	    if ( ier < 0 ) continue;

	    if ( cgr_ntrsct (in_llx, in_lly, in_urx, in_ury, 
				    fllx, flly, furx, fury, &ier) ) {
		if (*llx > fllx)
		    *llx = fllx;
		if (*lly > flly)
		    *lly = flly;
		if (*urx < furx)
		    *urx = furx;
	        if (*ury < fury)
		    *ury = fury;
   	    }

	}
    }

}
