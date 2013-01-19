#include "crgcmn.h"

void crg_gtyp ( int elnum, char *vg_class, char *vg_type, int *iret )
/************************************************************************
 * crg_gtyp								*
 *									*
 * This function returns the vg_class and vg_type for the specified	*
 * element.								*
 *									*
 * crg_gtyp ( elnum, vg_class, vg_type, iret)				*
 *									*
 * Input parameters:							*
 *	elnum		int	Element number				*
 *									*
 * Output parameters:							*
 *	*vg_class	char	vg_class				*
 *	*vg_type	char	vg_type					*
 *	*iret		int	Return code                     	*
 *				 -2 = elnum out of bounds		*
 *				 -5 = elnum is deleted (ioffset < 0)	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/98	Created					*
 * S. Law/GSC		05/98	Added vg_class				*
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/SAIC	04/02	init vg_class, vg_type, add error checks*
 ***********************************************************************/
{
    *iret     = 0;
    *vg_class = 0;
    *vg_type  = 0;

    if ((elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0)) {
	if ( range[elnum].ioffset >= 0 ) {
            *vg_class = range[elnum].vg_class;
            *vg_type  = range[elnum].vg_type;
	}
	else {
	    *iret = -5;			/* elnum is deleted */
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }
}
