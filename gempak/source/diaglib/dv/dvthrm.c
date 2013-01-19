#include "dv.h"

void dv_thrm  ( int *iret )
/************************************************************************
 * dv_thrm								*
 *									*
 * This subroutine computes the thermal wind.				*
 *									*
 *     THRM ( S ) = [ u (GEO(S)) (level1) - u (GEO(S)) (level2),	*
 *                    v (GEO(S)) (level1) - v (GEO(S)) (level2) ]	*
 *									*
 * THRM generates a vector field.					*
 *									*
 * dv_thrm  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					 -9 = ... calling sequence error*
 *					-11 = ... must be a vector	*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * G. Huffman/GSC	 9/88	GEMPAK4 version				*
 * L. Sager/NMC          4/93	Change to UTHRM and VTHRM		*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		ier, tmp, idlun, nval;
        int             num1, num2, nu, nv;

	char	        grid[13], parm[13], time1[21], time2[21];
	int		level1, level2, ignum, ivcord;
        char            errst[60];

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the information from the top of the stack.
         */
	dg_tops  ( grid, &ignum, time1, time2, &level1, &level2, 
                          &ivcord, parm, iret );
	if  ( *iret != 0 )  return;

        /*
         *	If the name of the vector to be computed is "THRM", replace the
         *	top of the stack with the proper dependent "height" field.
         *	Otherwise, assume that the user has done it.
         */
	if  ( strncmp(grid, "THRM", 4) == 0 )  {
	    if  ( ivcord == 0 )
		dg_rpls  ( "ZMSL", &zero, &ier );
	    else if  ( ivcord == 1 )
		dg_rpls  ( "HGHT", &zero, &ier );
	    else if  ( ivcord == 2 )
		dg_rpls  ( "PSYM", &zero, &ier );
	    else if  ( ivcord == 3 )
		dg_rpls  ( "PRES", &zero, &ier );
	    else {
		*iret = -24;
                tmp = -1;
		dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
                dg_cset ( "ERRST", errst, &ier );
		return;
	    }

        /*
         *	Assume that the user did it.
         */
	}

        /*
         *	Read in the scalar grid at the two levels.
         */
	dg_getl  ( &num1, &num2, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the lower scalar field on the stack and compute the
         *	geostrophic wind.  Leave it for subtraction.
         */
	dg_puts  ( &num2, iret );
	if  ( *iret != 0 )  return;
	dv_geo  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the upper scalar field on the stack and compute the
         *	geostrophic wind.  Leave it for subtraction.
         */
	dg_puts  ( &num1, iret );
	if  ( *iret != 0 )  return;
	dv_geo  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Subtract lower from upper (the order left above), and read
         *	the grid file numbers of the answer.
         */
	dv_vsub  ( iret );
	if  ( *iret != 0 )  return;
	dg_topv  ( grid, &nu, &nv, time1, time2, &level1, &level2, 
                          &ivcord, parm,  iret );
	if  ( *iret != 0 )  return;

        /*
         *	Update both grid headers.  Use wind type as parameter name.
         *	The stack is current.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret);
	dg_upvg  ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                          "UTHRM", &nu, &nv, &ier );
	dg_esub  ( &nu, &nv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
