#include "dv.h"

void dv_def ( int *iret )
/************************************************************************
 * dv_def								*
 *									*
 * This subroutine computes the total deformation of a vector:		*
 *									*
 *     DEF ( V ) = ( STR (V) ** 2 + SHR (V) ** 2 ) ** .5		*
 *									*
 * DEF generates a scalar grid.						*
 *									*
 * dv_def ( iret )							*
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
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC            4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEC	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, kxd, kyd, ksub1, ksub2, ier, zero=0;
	int		numu, numv, nstr, nshr, numout;
        float           *grstr, *grshr, *grout;
        float           dshr, dstr;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector grid.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the vector on the stack, compute the stretching deformation,
         *	and get the result.
         */
	dg_putv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	dv_str  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nstr, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the vector on the stack, compute the shearing deformation,
         *	and get the result.
         */
	dg_putv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	dv_shr  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nshr, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a number for the deformation grid and compute DEF
         */
	dg_nxts  ( &numout, iret );
	if  ( *iret != 0 )  return;
        
        dg_getg ( &nshr, &grshr, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nstr, &grstr, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    dshr  =  grshr[i];
	    dstr  =  grstr[i];
	    if  ( ERMISS (dshr) || ERMISS (dstr) )
		grout[i] = RMISSD;
	    else                                 
		grout[i] = (float) sqrt (dshr*dshr + dstr*dstr);
	    
	}

        /*
         *	Make a name of the form 'DEF'//u and update header;
         *	update stack.
         */
	dg_updh  ( "DEF", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
