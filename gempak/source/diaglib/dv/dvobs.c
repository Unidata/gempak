#include "dv.h"

void dv_obs  ( int *iret )
/************************************************************************
 * dv_obs								*
 *									*
 * This subroutine gets the observed wind with conversion, if needed.	*
 *									*
 * dv_obs  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -9 = calling sequence error	*
 *					-10 = internal grid list full	*
 *					-11 = grid must be a vector	*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 9/88	Break out DG_GOBS separately from DG_OBS*
 * M. desJardins/NMC	 3/92	Make WND and WIND same as OBS		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		ier, zero=0;

	char		time1[21], time2[21], gvect[13], parm[13];
	int		level1, level2, ivcord;
        int             ignumu, ignumv;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get information on grid to find.
         */
	dg_topv  ( gvect, &ignumu, &ignumv, time1, time2, &level1,
                          &level2, &ivcord, parm,  iret );
	if  ( *iret != 0 )  return;

        /*
         *	Check that correct subroutine has been called.
         */
	if  ( ( strncmp(gvect, "OBS", 3) != 0 ) && 
              ( strncmp(gvect, "WND", 3) != 0 ) &&
     	      ( strncmp(gvect, "WIND", 4) != 0 ) )  {
	    *iret = -9;
            dg_cset ( "ERRST", gvect, &ier);
	    return;
	}

        /*
         *	Actually get the grids and replace the vector on the stack.
         */
	dg_gobs  ( time1, time2, &level1, &level2, &ivcord, 
                          &ignumu, &ignumv, iret );
	if  ( *iret != 0 )  return;
	dg_rplv  ( " ", &ignumu, &ignumv, iret );
	dg_esub  ( &ignumu, &ignumv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
