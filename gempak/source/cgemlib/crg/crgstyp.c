#include "crgcmn.h"

void crg_styp ( int elnum, char vg_class, char vg_type, int *iret )
/************************************************************************
 * crg_styp								*
 *									*
 * This function sets the vg_class and vg_type for the specified	*
 * element.								*
 *									*
 * crg_styp ( elnum, vg_class, vg_type, iret) 				*
 *									*
 * Input parameters:							*
 *	elnum		int	Element number				*
 *	vg_class	char	vg_class				*
 *	vg_type		char	vg_type					*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				 -2 = elnum out of bounds		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/98						*
 * S. Law/GSC		05/98	Added vg_class				*
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/SAIC	04/02	return -2 if out of range		*
 ***********************************************************************/
{
    *iret = 0;

    if ((elnum < MAX_EDITABLE_ELEMS) && (elnum >= 0)) {
        range[elnum].vg_class = vg_class;
        range[elnum].vg_type = vg_type;
    }
    else {
	*iret = -2;			/* elnum is out of bounds */
    }
}
