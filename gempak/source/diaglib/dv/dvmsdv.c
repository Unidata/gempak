#include "dv.h"

void dv_msdv  ( int *iret )
/************************************************************************
 * dv_msdv								*
 *									*
 * This subroutine computes mass-scalar flux divergence:		*
 *									*
 *     MSDV ( S, V ) = DIV ( [ MASS * LAV (S) * LAV (u),		*
 *                             MASS * LAV (S) * LAV (v) ] )		*
 *									*
 * S and V must be grid parameters for LAV to work correctly.  MASS is	*
 * given V's in-line parameters without regard to the values in S's in-	*
 * line parameters.   MSDV generates a scalar grid.			*
 *									*
 * dv_msdv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		ns, nlavs, nu, nv, nmass, nmsdv;

	char    	gnam[13], gvect[13], gdum[13], pdum[13];
     	char		gfun[13], time1[21], time2[21];
	int		level1, level2, ivcord, zero=0, idlun, nval, ier;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	S is on top of the stack.  Read the scalar name, compute the
         *	LAV (S) and get the result.
         */
	dg_tops  ( gfun, &ns, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;
	df_lav  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nlavs, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Now proceed as at the beginning of DV_MDIV . . .
         *	Read the vector name on top of the stack and replace it with
         *	PRES (so that MASS will pick up in-line parameters).
         */
	dg_topv  ( gvect, &nu, &nv, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;
	dg_rpls  ( "PRES", &zero, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute the mass / unit volume, and read the number of the grid.
         */
	df_mass  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops  ( gdum, &nmass, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Replace the top of the stack with the vector name, compute the
         *	average wind vector within the layer, and leave the result.
         */
	dg_rplv  ( gvect, &zero, &zero, iret );
	if  ( *iret != 0 ) return;
	dv_vlav  ( iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the mass on top of the stack, put the LAV (S) on the stack
         *	and multiply (extra steps compared to DV_MDIV), compute the
         *	scalar divergence, and read the grid number of the result.
         */
	dg_puts  ( &nmass, iret );
	if  ( *iret != 0 ) return;

	dg_puts  ( &nlavs, iret );
	if  ( *iret != 0 ) return;
	df_mul  ( iret );
	if  ( *iret != 0 ) return;

	dv_sdiv  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops  ( gdum, &nmsdv, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Make a name of the form 'MSDV'//S//u and update header; the
         *	stack is current.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret);

	dg_mnam ( "MSDV", gfun, gvect, gnam, &ier );
	dg_upsg ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                         gnam, &nmsdv, iret );
	dg_esub  ( &nmsdv, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
