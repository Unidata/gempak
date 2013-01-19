#include "dv.h"

void dv_vr  ( int *iret )
/************************************************************************
 * dv_vr								*
 *									*
 * This subroutine returns the v component of a vector in grid		*
 * relative coordinates.						*
 *									*
 *     VR  ( V ) = v							*
 *									*
 * dv_vr  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 5/88	Added new stack functions		*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 4/89	Added grid relative functions		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int	zero = 0;
	int		ier, numu, numv;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector from the stack.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Return the v component.  Make a name of the form 'V'//v and 
         *	update the header; update the stack.
         */
	dg_updh  ( "V", &numv, &numv, &zero, iret );
	dg_puts  ( &numv, iret );
	dg_esub  ( &numv, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
