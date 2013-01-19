#include "dv.h"

void dv_mag  ( int *iret )
/************************************************************************
 * dv_mag								*
 *									*
 * This subroutine computes the magnitude of a vector:			*
 *									*
 *     MAG ( V ) = PD_SPED ( u, v )					*
 *									*
 * MAG generates a scalar grid.						*
 *									*
 * dv_mag  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translatino from Fortran                *
 ************************************************************************/
{
        int             kxd, kyd, kxyd, ksub1, ksub2, ier, zero=0;
	int		numu, numv, numout;
	float		*gru, *grv, *grout;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector into the grid table, define a new grid, and
         *	compute the magnitude.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	dg_nxts  ( &numout, iret );
	if  ( *iret != 0 ) return;

        dg_getg( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	kxyd = kxd * kyd;
	pd_sped  ( gru, grv, &kxyd, grout, &ier );

        /*
         *	Make a name of the form 'MAG'//u and update the header;
         *	update the stack.
         */
	dg_updh  ( "MAG", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
