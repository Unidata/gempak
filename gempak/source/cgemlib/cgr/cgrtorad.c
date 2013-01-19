#include "geminc.h" 
#include "gemprm.h"

void cgr_to_rad ( double deg, double *rd )
/************************************************************************
 * cgr_to_rad								*
 *									*
 * This function will convert a degree value to a radians		*
 * value.  Radians are needed for trigonometric functions.		*
 *									*
 * cgr_to_rad ( deg, rd )						*
 *									*
 * Input parameters:							*
 *	deg		double		Degrees value			*
 *									*
 * Output parameters:							*
 *	 *rd		double		Radians value			*
 *									*
 **									*
 * E. Wehner/EAI	10/96	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * F. J. Yen/NCEP	 5/99	Changed cgr_to_rad to a void function	*
 * E. Safford/GSC	10/00	removed cgr.h include			*
 ***********************************************************************/
{
    *rd = (((double)(deg))*M_PI)/180.0;
    return;
}

