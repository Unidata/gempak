#include "dg.h"

void dg_getv ( int *ignumu, int *ignumv, int *iret )
/************************************************************************
 * dg_getv								*
 *									*
 * This subroutine gets the next operand on the stack.  The operand	*
 * must be a vector; otherwise, an error is returned.  The internal	*
 * grid numbers of the u and v components are returned.			*
 *									*
 * dg_getv ( ignumu, ignumv, iret )					*
 *									*
 * Output parameters:							*
 *	*ignumu		int		Grid number for u-component	*
 *	*ignumv		int		Grid number for v-component	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = calling sequence error	*
 *					-10 = internal grid list full	*
 *					-11 = grid must be a vector	*
 *					-12 = grid must be a scalar	*
 *					-13 = grid must be in file	*
 *					-16 = map proj is invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-18 = TIME must be a range	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 *					-25 = Vector cannot be computed	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 5/88						*
 * G. Huffman/GSC	 9/88	Added retrieval of direct parm names	*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 4/89	Eliminated direct parm names		*
 * M. desJardins/NMC	 3/92	WND, WIND = OBS				*
 * K. Brill/NMC		 4/93	Checked only for WND (set in DG_TOPV);	*
 *				Called DG_GOBS directly; Read any vector*
 *				ABC as components UABC, VABC		*
 * M. desJardins/NMC	 8/93	Eliminated duplicate names		*
 * T. Lee/GSC		 5/96	Added input parameter check		*
 * T. Lee/GSC		 9/96	Removed input check; Changed error msg	*
 * K. Brill/HPC		12/01	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
C************************************************************************/
{
    char time1[21], time2[21], gvect[14], parm[14];
    int level1, level2, ignum, ivcord, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    dg_ssub ( iret );

    /*
     * Get grid information.
     */
    dg_topv ( gvect, ignumu, ignumv, time1, time2, &level1, &level2,
              &ivcord, parm,  iret );
    if ( *iret != 0 ) return;

    /*
     * Check to see that the grid doesn't already exist in the internal
     * grid list.
     */
    if ( strcmp ( gvect, "WND" ) != 0 &&
         _dgstck.stack[_dgstck.itop][0] != '\0' ) {
	/*
	 * Try to read the vector components from the file.
	 */
	strcpy ( parm, "U" );
	strcat ( parm, gvect );
	dg_rgrd ( time1, time2, &level1, &level2, &ivcord, parm,
	          ignumu, &ier );
	if ( ier == 0 ) {
	    strcpy ( parm, "V" );
	    strcat ( parm, gvect );
	    dg_rgrd ( time1, time2, &level1, &level2, &ivcord, parm,
	              ignumv, &ier );
	    if ( ier == 0 ) {
		_dgstck.stack[_dgstck.itop][0] = '\0';
	    } else {
 	        dg_frig ( ignumv, &ier );
	    }
	} else {
 	    dg_frig ( ignumu, &ier );
	}
    }

    if ( _dgstck.stack[_dgstck.itop][0] != '\0' ) {
	/*
	 * Check for various wind vectors to compute.
	 */
	if ( strcmp ( gvect, "WND" ) == 0 ) {
	    dg_gobs ( time1, time2, &level1, &level2, &ivcord, ignumu,
	              ignumv, iret );
	    if ( *iret == 0 ) {
		dg_rplv  ( "", ignumu, ignumv, iret );
	    }
	} else if ( strcmp ( gvect, "GEO" ) == 0 ) {
	    dv_geo ( iret );
	} else if ( strcmp ( gvect, "AGE" ) == 0 ) {
	    dv_age ( iret );
	} else if ( strcmp ( gvect, "THRM" ) == 0 ) {
	    dv_thrm ( iret );
	} else if ( strcmp ( gvect, "ISAL" ) == 0 ) {
	    dv_isal ( iret );
	} else {
	    *iret  = -25;
	    strcpy ( _dgerr.errst, gvect );
	}

	/*
	 * If the function was computed, get the grid numbers from
	 * the top of the stack.
	 */
	if ( *iret == 0 ) {
	    ignum  = _dgstck.istack[_dgstck.itop];
	    *ignumu = ignum / 100;
	    *ignumv = ignum % 100;
	}
    }

    /*
     * If the function was computed or already in DGG, decrement the
     * stack pointer.
     */
    _dgstck.itop--;

    dg_esub ( ignumu, ignumv, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
