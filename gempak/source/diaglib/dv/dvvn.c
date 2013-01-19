#include "dv.h"

void dv_vn  ( int *iret )
/************************************************************************
 * dv_vn								*
 *									*
 * This subroutine returns the v component of a vector in north		*
 * relative coordinates.						*
 *									*
 *     VN  ( V ) = v							*
 *									*
 * dv_vn  ( iret )							*
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
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero = 0;
        int             ier, kxd, kyd, ksub1, ksub2;
	int		numu, numv, nunor, nvnor;
	float		*gru, *grv, *grunor, *grvnor;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector from the stack.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Get a new vector to translate from grid relative to north
         *	relative components.
         */
	dg_nxtv  ( &nunor, &nvnor, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Internal grid are always grid relative.  Translate to north rel.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nunor, &grunor, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvnor, &grvnor, &kxd, &kyd, &ksub1, &ksub2, iret );

	dg_nrel  ( gru, grv, grunor, grvnor, &ier );

        /*
         *	Return the v component.  Make a name of the form 'V'//v and 
         *	update the header; update the stack.
         */
	dg_updh  ( "V", &nvnor, &numv, &zero, iret );
	dg_puts  ( &nvnor, iret );
	dg_esub  ( &nvnor, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
