#include "dv.h"

void dv_age  ( int *iret )
/************************************************************************
 * dv_age								*
 *									*
 * This subroutine computes the ageostrophic wind:			*
 *									*
 *     AGE ( S ) = [ u (OBS) - u (GEO(S)), v (OBS) - v (GEO(S)) ]	*
 *									*
 * AGE generates a vector grid.						*
 *									*
 * dv_age  ( iret )							*
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
 * K. Brill/GSC          8/89	Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * L. Sager/NMC          4/93	Change to UAGE and VAGE			* 
 * L. Sager/NMC		 5/93 	Add WND to top of stack			*
 * M. desJardins/NMC	 7/93	Update grid names			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK IRET & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translated from Fortran                 *
 ************************************************************************/
{
        int             ignum, ier, dummy=0, miss=-1;
	int		level1, level2, ivcord, idlun;
        int             i, nval, kxd, kyd, ksub1, ksub2;
        int             ngeou, ngeov, nobsu, nobsv;
        float           *grngeou, *grngeov, *grnobsu, *grnobsv;
	char	        grid[13], parm[13], time1[21], time2[21];
        char            errst[1024];
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the information from the top of the stack.
         */
	dg_tops ( grid, &ignum, time1, time2, &level1, &level2, 
                          &ivcord, parm, iret );
	if  ( *iret != 0 )  return;

        /*
         *	If the name of the vector to be computed is "AGE", replace the
         *	top of the stack with the proper dependent "height" field.
         *	Otherwise, assume that the user has done it.
         */
	if  ( strncmp(grid, "AGE",3) == 0 )  {
	    if  ( ivcord == 0 )  
		dg_rpls ( "ZMSL", &dummy, &ier );
	    else if ( ivcord == 1 )
		dg_rpls ( "HGHT", &dummy, &ier );
	    else if ( ivcord == 2 )
		dg_rpls ( "PSYM", &dummy, &ier );
	    else if ( ivcord == 3 )
		dg_rpls ( "PRES", &dummy, &ier );
	    else {
		*iret = -24;
		dg_merr  ( "", "", "", &miss, &miss, &ivcord, errst, &ier );
                dg_cset ( "ERRST", errst, &ier);
		return;
	    }

        /*
         *	Else, assume that the user did it.
         */
	}

        /*
         *	Compute the geostrophic wind and get the grid numbers.
         */
	dv_geo  ( iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &ngeou, &ngeov, iret );
	if  ( *iret != 0 )  return;

        /*
         *      Put wind on the stack
         */
	dg_puts ( &ngeou, iret);
	dg_rplv ( "WND", &dummy, &dummy, &ier);
	dg_getv ( &nobsu, &nobsv, iret);
	if  ( *iret != 0 )  return;

        /*
         *	Retrieve the observed wind.
         */ 
/*       CALL DG_GOBS  ( time, level, ivcord, nobsu, nobsv, iret )    */
	if  ( *iret != 0 )  return;

        dg_getg ( &nobsu, &grnobsu, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &nobsv, &grnobsv, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &ngeou, &grngeou, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &ngeov, &grngeov, &kxd, &kyd, &ksub1, &ksub2, iret);

	for ( i = ksub1 - 1; i < ksub2; i++ ) {

            /*
             *	    Compute the u-component difference.
             */
	    if  ( ERMISS ( grnobsu[i] )  || ERMISS ( grngeou[i] ) )
		grngeou[i] = RMISSD;
	    else
		grngeou[i] = grnobsu[i] - grngeou[i];

            /*
             *	    Compute the v-component difference.
             */
	    if  ( ERMISS ( grnobsv[i] ) || ERMISS ( grngeov[i] ) )
		grngeov[i] = RMISSD;
	    else
		grngeov[i] = grnobsv[i] - grngeov[i];
	    
	}

        /*
         *	Update both grid headers.  Use wind type as parameter name.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret );

	dg_upvg ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                         "AGE", &ngeou, &ngeov, &ier );

        /*
         *	Update stack.
         */
	dg_putv  ( &ngeou, &ngeov, iret );
	dg_esub  ( &ngeou, &ngeov, &dummy, &dummy, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
