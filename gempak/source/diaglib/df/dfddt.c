#include "df.h"

void df_ddt ( int *iret )
/************************************************************************
 * df_ddt								*
 *									*
 * This subroutine computes the time derivative:			*
 *									*
 *     DDT (S) = [ S (time1) - S (time2) ] / (time1 - time2)		*
 *									*
 * where the time difference is in seconds.				*
 *									*
 * df_ddt ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	 7/88	Added new stack subroutines		*
 * G. Huffman/GSC	 8/88	Revised name generation; error messages	*
 * K. Brill/GSC		 8/89	Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
 * K. Brill/GSC         11/89   Call TG_DIFF instead of TI_DIFF		*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    char gp[13], time1[21], time2[21], parm[13], gfunc[13];
    int ntdf, kxd, kyd, ksub1, ksub2, fidx, cidx; 
    int level1, level2, ivcord, imins, zero, ier;
    float *gntdf;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Compute the scalar difference over time.
     */
    df_tdf ( iret );
    if ( *iret != 0 ) return;

    /*
     * Get the pointer to the time difference; save the scalar name.
     */
    dg_tops ( gfunc, &ntdf, time1, time2, &level1, &level2, &ivcord,
	      parm, iret );
    if ( *iret != 0 ) return;

    /*
     * Convert the date/time range into seconds.
     */
    tg_diff ( time1, time2, &imins, &ier, strlen(time1), strlen(time2) );

    /*
     * Divide the scalar difference by the number of seconds.
     */
    dg_getg ( &ntdf, &gntdf, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
        cidx = fidx - 1;
	if ( imins == 0 || ERMISS ( gntdf[cidx] ) ) {
	    gntdf[cidx] = RMISSD;
	} else {
            gntdf[cidx] /= ( imins * 60 );
	}
    }

    /*
     * Make a name of the form 'DDT'//S and update header;
     * the stack is current.  DG_UPDH is not used here because
     * the scalar name was buried in the TDF parameter name.
     */
    strcpy ( gp, "DDT" );
    strcat ( gp, &parm[3] );
    dg_upsg ( time1, time2, &level1, &level2, &ivcord, &zero, gp,
	      &ntdf, iret );
    dg_esub ( &ntdf, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
