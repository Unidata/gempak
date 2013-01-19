#include "dg.h"

void dg_gett ( int *ignum1, int *ignum2, int *iret )
/************************************************************************
 * dg_gett								*
 *									*
 * This subroutine gets the next operand on the stack for a time	*
 * computation.  The operand must be a scalar; otherwise, an error 	*
 * is returned.  The internal grid numbers of the grids are returned.	*
 *									*
 * dg_gett ( ignum1, ignum2, iret )					*
 *									*
 * Output parameters:							*
 *	*ignum1		int		Grid number for time1		*
 *	*ignum2		int		Grid number for time2		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 *					-12 = grid must be a scalar	*
 *					-13 = grid must be in file	*
 *					-16 = map proj is invalid	*
 *					-18 = TIME must be a range	*
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
 * M. desJardins/NMC	 3/92	Documentation				*
 * K. Brill/HPC		12/01	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], gfunc[14], parm[14], t2[21];
    int ignum, level1, level2, ivcord, zero, minus1, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    minus1 = -1;
    dg_ssub ( iret );

    /*
     * Get information about the grid.
     */
    dg_tops ( gfunc, &ignum, time1, time2, &level1, &level2, &ivcord, parm,
              iret );

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
    } else {
	/*
	 * Check that there are two times.
	 */
	if ( ( time1[0] == '\0' ) || ( time2[0] == '\0' ) ||
	     ( strcmp ( time1, time2 ) == 0 ) ) {
	    *iret = -18;
	    dg_merr  ( "", time1, time2, &minus1, &minus1, &minus1,
	               _dgerr.errst, &ier );
	    return;
	/*
	 * Read the grids from the file.
	 */
	} else {
	    /*
	     * Get grid at time 1.
	     */
	    strcpy ( t2, time2 );
	    time2[0] = '\0';
	    dg_rgrd ( time1, time2, &level1, &level2, &ivcord,
	        _dgstck.stack[_dgstck.itop], ignum1, iret );
	    if ( *iret != 0 ) return;

	    /*
	     * Get grid at time 2.
	     */
	    strcpy ( time1, t2 );
	    dg_rgrd ( time1, time2, &level1, &level2, &ivcord,
	        _dgstck.stack[_dgstck.itop], ignum2, iret );
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
