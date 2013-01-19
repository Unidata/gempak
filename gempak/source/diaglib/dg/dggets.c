#include "dg.h"

void dg_gets ( int *ignum, int *iret )
/************************************************************************
 * dg_gets								*
 *									*
 * This subroutine gets the next operand on the stack.  The operand	*
 * must be a scalar; otherwise, an error is returned.  The internal	*
 * grid number of the grid is returned.					*
 *									*
 * dg_gets ( ignum, iret )						*
 *									*
 * Output parameters:							*
 *	*ignum		int		Grid number			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 *					-12 = grid must be a scalar	*
 *					-13 = grid must be in file	*
 *					-16 = map proj is invalid	*
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
 * M. desJardins/GSFC	 9/88	Fixed MASS, STAB			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 4/89	Eliminate vectors entered with /	*
 * M. desJardins/NMC	 3/92	Check for WND, WIND			*
 * K. Brill/NMC		 5/93	Remove call to ST_LSTR			*
 * M. desJardins/NMC	 8/93	Eliminate duplicate names		*
 * K. Brill/HPC		12/01	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], gfunc[14], parm[14];
    int level1, level2, ivcord, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    dg_ssub ( iret );

    /*
     * Get information about the grid.
     */
    dg_tops ( gfunc, ignum, time1, time2, &level1, &level2, &ivcord,
              parm, iret );
    if ( *iret != 0 ) return;

    /*
     * If grid has not been computed, make sure it's not a misplaced
     * vector name, then get it from the grid file.
     */
    if ( gfunc[0] != '\0' ) {
	if ( ( strcmp ( gfunc, "OBS" ) == 0 ) ||
	     ( strcmp ( gfunc, "WND" ) == 0 ) ||
	     ( strcmp ( gfunc, "GEO" ) == 0 ) ||
	     ( strcmp ( gfunc, "AGE" ) == 0 ) ||
	     ( strcmp ( gfunc, "ISAL" ) == 0 ) ||
	     ( strcmp ( gfunc, "THRM" ) == 0 ) ) {
	    *iret = -12;
	    strcpy ( _dgerr.errst, gfunc );
	    return;
	}

	/*
	 * Read the grid from the file.
	 */
	dg_rgrd ( time1, time2, &level1, &level2, &ivcord, gfunc,
	          ignum, iret );

	/*
	 * If MASS is to be computed, put pressure on stack, so that 
	 * layer to be used can be found.
	 */
	if ( ( *iret != 0 ) && ( strcmp ( gfunc, "MASS" ) == 0 ) ) {
	    strcpy ( _dgstck.stack[_dgstck.itop], "PRES" );
	    df_mass ( iret );
	    if ( *iret == 0 ) *ignum = _dgstck.istack[_dgstck.itop];
	} else if ( ( *iret != 0 ) && ( strcmp ( gfunc, "STAB" ) == 0 ) ) {
	    strcpy ( _dgstck.stack[_dgstck.itop], "TMPC" );
	    df_stab ( iret );
	    if ( *iret == 0 ) *ignum = _dgstck.istack[_dgstck.itop];
	}
    }

    /*
     * Decrement the stack pointer.
     */
    _dgstck.itop--;

    dg_esub ( ignum, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
