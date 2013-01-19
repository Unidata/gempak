#include "dg.h"

void dg_getl ( int *ignum1, int *ignum2, int *iret )
/************************************************************************
 * dg_getl								*
 *									*
 * This subroutine gets the next operand on the stack for a layer	*
 * computation.  The operand must be a scalar; otherwise, an error 	*
 * is returned.  The internal grid numbers of the grids are returned.	*
 *									*
 * dg_getl ( ignum1, ignum2, iret )					*
 *									*
 * Output parameters:							*
 *	*ignum1		int		Grid number for level1		*
 *	*ignum2		int		Grid number for level2		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list is full*
 *					-12 = must be a scalar		*
 *					-13 = must be from grid file	*
 *					-16 = map projection invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 5/88						*
 * M. desJardins/GSFC	 7/88	Added %, ^, & 				*
 * M. desJardins/GSFC	 7/88	Adapted from DG_GETS			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/NMC		 4/93   Called DG_FNDS 				* 
 * T. Lee/GSC		 5/96	Added input parameter check		*	
 * K. Brill/HPG		12/01	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], gfunc[14], parm[14];
    int ignum, level1, level2, ivcord, l2, minus1, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;
    zero = 0;
    dg_ssub ( iret );

    /*
     * Get information from the top of the stack.
     */
    dg_tops ( gfunc, &ignum, time1, time2, &level1, &level2, &ivcord, parm,
              iret );
    if ( *iret != 0 ) return;

    /*
     * Check to see if grid has already been computed.
     * If so, this is an error.
     */
    if ( gfunc[0] == '\0' ) {
	*iret = -13;
	strcpy ( _dgerr.errst, parm );
	return;
    /*
     * Otherwise, get the grids from the grid file.
     */
    } else if ( ( strcmp ( gfunc, "OBS" ) == 0 ) ||
                ( strcmp ( gfunc, "WND" ) == 0 ) ||
		( strcmp ( gfunc, "GEO" ) == 0 ) ||
		( strcmp ( gfunc, "AGE" ) == 0 ) ||
		( strcmp ( gfunc, "ISAL" ) == 0 ) ||
		( strcmp ( gfunc, "THRM" ) == 0 ) ) {
	*iret = -12;
	strcpy ( _dgerr.errst, gfunc );
	return;
    } else {
	/*
	 * Check that there are two levels.
	 */
	if ( ( level1 == -1 ) || ( level2 == -1 ) || ( level1 == level2 ) ) {
	    *iret = -17;
	    dg_merr ( "", "", "", &level1, &level2,  &minus1, _dgerr.errst,
	              &ier );
	    return;

	/*
	 * Read the grids from the file.
	 */
	} else {
	    /*
	     * Get grid at level 1.
	     */
	    l2 = level2;
	    level2 = -1;
	    strcpy ( gfunc, _dgstck.stack[_dgstck.itop] );
	    strcpy ( parm,  _dgstck.stack[_dgstck.itop] );
	    dg_fnds ( time1, time2, &level1, &level2, &ivcord, gfunc, ignum1,
		      iret );
	    if ( *ignum1 == 0 ) {
		strcpy ( gfunc, parm );
		dg_rgrd ( time1, time2, &level1, &level2, &ivcord, gfunc,
		          ignum1, iret );
	    }
	    if ( *iret != 0 ) return;

	    /*
	     * Get grid at level 2.
	     */
	    level1 = l2;
	    strcpy ( gfunc, parm );
	    dg_fnds ( time1, time2, &level1, &level2, &ivcord, gfunc, ignum2,
		      iret );
	    if ( *ignum2 == 0 ) {
		strcpy ( gfunc, parm );
		dg_rgrd ( time1, time2, &level1, &level2, &ivcord, gfunc,
			  ignum2, iret );
	    }
	    if ( *iret != 0 ) return;
	}
    }

    /*
     * Decrement the stack pointer.
     */
    _dgstck.itop--;

    dg_esub ( ignum1, ignum2, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
