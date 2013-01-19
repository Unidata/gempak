#include "dv.h"

void dv_mdiv ( int *iret )
/************************************************************************
 * dv_mdiv								*
 *									*
 * This subroutine computes layer-average mass divergence:		*
 *									*
 *     MDIV ( V ) = DIV ( [ MASS * LAV (u), MASS * LAV (v) ] )		*
 *									*
 * V must be a grid parameter for LAV to work correctly.  		*
 * MDIV generates a scalar grid.					*
 *									*
 * dv_mdiv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		nu, nv, nmass, nmdiv;

	char	        gnam[13], gvect[13], gdum[13], pdum[13];
        char            time1[21], time2[21];
	int		level1, level2, ivcord, zero=0, ier, idlun, nval;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the vector name on top of the stack and replace it with
         *	PRES (so that MASS will pick up in-line parameters).
         */
	dg_topv  ( gvect, &nu, &nv, time1, time2, &level1, 
                           &level2, &ivcord, pdum, iret );
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
         *	Put the mass on top of the stack, compute the mass divergence,
         *	and read the grid number of the result.
         */
	dg_puts  ( &nmass, iret );
	if  ( *iret != 0 ) return;
	dv_sdiv  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops  ( gdum, &nmdiv, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Make a name of the form 'MDIV'//u and update header; the
         *	stack is current.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret );

	dg_mnam  ( "MDIV", gvect, "", gnam, &ier );
	dg_upsg  ( time1, time2, &level1, &level2, &ivcord, 
                               &idlun, gnam, &nmdiv, iret );
	dg_esub  ( &nmdiv, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
