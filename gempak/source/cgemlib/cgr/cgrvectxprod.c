#include "geminc.h"
#include "gemprm.h"

void cgr_vectxprod ( double *va, double *vb, double *vc, int *iret )
/************************************************************************
 * cgr_vectxprod							*
 *									*
 * This function computes the cross product of two three dimensional	*
 * vectors.								*
 *									*
 * void cgr_vectxprod ( va, vb, vc, iret )				*
 *									*
 * Input parameters:							*
 *	*va		double		Vector A			*
 *	*vb		double		Vector B			*
 *									*
 * Output parameters:							*
 *	*vc		double		Cross product vector		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * R. Tian/SAIC		07/05		Created				*
 * D.W.Plummer/NCEP	12/06		Made all arguments doubles	*
 ***********************************************************************/
{
    *iret = 0;

    vc[0] = va[1] * vb[2] - va[2] * vb[1];
    vc[1] = va[2] * vb[0] - va[0] * vb[2];
    vc[2] = va[0] * vb[1] - va[1] * vb[0];

    return;
}
