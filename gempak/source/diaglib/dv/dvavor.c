#include "dv.h"

void dv_avor ( int *iret )
/************************************************************************
 * dv_avor								*
 *									*
 * This subroutine computes the absolute vorticity of a vector:		*
 *									*
 *     AVOR ( V ) = VOR ( V ) + CORL					*
 *									*
 * AVOR generates a scalar grid.					*
 *									*
 * dv_avor ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * G. Huffman/GSC	9/88	New stack functions			*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/NMC        11/90    Pass grid number to DF_CORL		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translated from Fortran                 *
 ************************************************************************/
{
        int             num, numu, numv, numcor, ier, iret1, iret2;
	char	        gfunc[13], parm[13], time1[21], time2[21];
	int		level1, level2, ivcord, zero=0;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector grid and put it back on the stack (grid number
         *	used name generation).
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	dg_putv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute the relative vorticity, leaving it on the stack.
         */
	dv_vor  ( iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute the coriolis grid and put it on the stack.
         */
	dg_nxts  ( &numcor, &iret1 );
	df_corl  ( &numcor, &iret2 );
        dg_puts  ( &numcor, iret  );
        *iret = *iret + iret1 + iret2;
	if  ( *iret != 0 ) return;

        /*
         *	Add the relative vorticity and coriolis force; read the
         *	answer grid number.
         */
	df_add  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops ( gfunc, &num, time1, time2, &level1, &level2, 
                          &ivcord, parm, iret );

        /*
         *	Make a name of the form 'AVOR'//u and update the header;
         *	the stack is current.
         */
	dg_updh  ( "AVOR", &num, &numu, &zero, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
