#include "dg.h"

void dg_topv  ( char *gvect, int *ignumu, int *ignumv, char *time1,
                char *time2, int *level1, int *level2, int * ivcord,
		char *parm, int *iret )
/************************************************************************
 * dg_topv								*
 *									*
 * This subroutine gets information about the grid on the top of the	*
 * stack.  If the grid has not been computed, GVECT will be returned	*
 * with the TIME, LEVEL, and IVCORD to be used in computing the grid.	*
 * If the grid has already been computed, IGNUMU and IGNUMV  will be	*
 * returned with the grid information and GVECT will be blank.		*
 *									*
 * dg_topv ( gvect, ignumu, ignumv, time1, time2, level1, level2,	*
 *           ivcord, parm, iret )					*
 *									*
 * Output parameters:							*
 *	*gvec		char		Grid to be computed		*
 *	*ignumu		int		Internal grid number of u comp	*
 *	*ignumv		int		Internal grid number of v comp	*
 *	*time1		char		Date/time			*
 *	*time2		char		Date/time			*
 *	*level1		int		Level				*
 *	*level2		int		Level				*
 *	*ivcord		int		Vertical coordinate		*
 *	*parm		char		Parameter name			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = Internal grid list is full*
 *					-11 = grid must be a vector	*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 8/88	Adapted from DG_UHDR			*
 * M. desJardins/GSFC	 4/89	Changed handling of time, errors	*
 * K. Brill		 9/89   Multiple files; check for internal grid	*
 * K. Brill              1/90   Moved i=i+1 in DO loop search for inter-*
 *                              nal grids so that v comp is correct	*
 * M. desJardins/NMC	 3/92	Edited long line			*
 * K. Brill/NMC		 4/93	CALL DG_FNDV				*
 * M. desJardins/NMC	 8/93	Used MMFILE for maximum number of files	*
 * T. Lee/GSC		 5/96	Checked for internal grid overflow	*
 * K. Brill/HPC		 1/02	Change iret = -10 to 2nd iret = -11	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ignum, gidx, ifile, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check that there is something on the stack.
     */
    if ( _dgstck.itop < 0 ) {
	*iret = -21;
	return;
    /*
     * Check for grid already computed.
     */
    } else if ( _dgstck.stack[_dgstck.itop][0] == '\0' ) {
	/*
	 * Get information about grid.
	 */
	ignum = _dgstck.istack[_dgstck.itop];
	gvect[0] = '\0';
	*ignumu = ignum / 100;
	*ignumv = ignum % 100;
	if ( ( *ignumu <= 0 ) || ( *ignumu >= 100 ) || ( *ignumv <= 0 ) ) {
	    *iret = -11;
	    if ( ( *ignumu > 0) && ( *ignumu < 100) ) {
		strcpy ( _dgerr.errst, _dggrid.gparmd[(*ignumu)-1] );
	    } else if ( ( *ignumu == 0) && ( *ignumu < 100) ) {
		*iret = -11;
	    } else {
		strcpy ( _dgerr.errst, "IGNUMU<=0||IGNUMU>=100" );
	    }
	    return;
	}
	gidx = (*ignumu) - 1;
	strcpy ( time1, _dggrid.dttimd1[gidx] );
	strcpy ( time2, _dggrid.dttimd2[gidx] );
	*level1 = _dggrid.leveld1[gidx];
	*level2 = _dggrid.leveld2[gidx];
	*ivcord = _dggrid.ivcrdd[gidx];
	strcpy ( parm, _dggrid.gparmd[gidx] );

    /*
     * Get information where grid has not been computed.
     */
    } else {
	strcpy ( gvect, _dgstck.stack[_dgstck.itop] );
	if ( strcmp ( gvect, "OBS" ) == 0 || strcmp ( gvect, "WIND" ) == 0 ) {
	    strcpy ( gvect, "WND" );
	}
	ignum = 0;
	*ignumu = 0;
	*ignumv = 0;

	/*
	 * Get the file number.
	 */
	ifile = _dgtabl.icflnm[_dgstck.icpntr[_dgstck.itop]];
	if ( ( ifile < 1 ) || ( ifile > MMFILE ) ) {
	    *iret = -32;
	    return;
	} else {
	    _dgfile.idlun = _dgfile.idflnm[ifile-1];
	}

	/*
	 * Check for input time, level, vcord.
	 */
	if ( _dgtabl.cgdttm[_dgstck.icpntr[_dgstck.itop]][0] == '\0' ) {
	    strcpy ( time1, _dginpt.ddttim1 );
	    strcpy ( time2, _dginpt.ddttim2 );
	    if ( time1[0] == '\0' ) {
		*iret = -22;
		strcpy ( _dgerr.errst, "^" );
		strcat ( _dgerr.errst, _dginpt.ingdtm );
		return;
	    }
	} else {
	    grc_gtim ( _dgtabl.cgdttm[_dgstck.icpntr[_dgstck.itop]],
	        _dgfile.tfirst[ifile-1], _dgfile.tlast[ifile-1], time1,
		time2, iret );
	    if ( *iret != 0 ) {
		*iret = -22;
		strcpy ( _dgerr.errst, "^" );
		strcat ( _dgerr.errst, _dgtabl.cgdttm[_dgstck.icpntr[_dgstck.itop]] );
		return;
	    }
	}

	if ( _dgtabl.clevel[_dgstck.icpntr[_dgstck.itop]][0] == '\0' ) {
	    *level1 = _dginpt.ldlevl1;
	    *level2 = _dginpt.ldlevl2;
	    if ( *level1 == -1 ) {
		*iret = -23;
		strcpy ( _dgerr.errst, "@" );
                strncat ( _dgerr.errst, _dginpt.inglev,
                          sizeof(_dgerr.errst) - 2 );
                _dgerr.errst[sizeof(_dgerr.errst)-1] = '\0';
		return;
	    }
	} else {
	    grc_levl ( _dgtabl.clevel[_dgstck.icpntr[_dgstck.itop]], level1, 
	        level2, iret );
	    if ( ( *iret != 0 ) || ( *level1 == -1 ) ) {
		*iret  = -23;
		strcpy ( _dgerr.errst, "@" );
		strcat ( _dgerr.errst, _dgtabl.clevel[_dgstck.icpntr[_dgstck.itop]] );
		return;
	    }
	}

	if ( _dgtabl.cvcord[_dgstck.icpntr[_dgstck.itop]][0] == '\0' ) {
	    *ivcord = _dginpt.lvcord;
	    if ( *ivcord < 0 ) {
		*iret  = -24;
		strcpy ( _dgerr.errst, "%" );
		strcat ( _dgerr.errst, _dginpt.invcrd );
		return;
	    }
	} else {
	    clv_cord ( _dgtabl.cvcord[_dgstck.icpntr[_dgstck.itop]],
	        _dgtabl.cvcord[_dgstck.icpntr[_dgstck.itop]], ivcord, &ier );
	    if ( ier != 0 ) {
		*iret  = -24;
		strcpy ( _dgerr.errst, "%" );
		strcat ( _dgerr.errst, _dgtabl.cvcord[_dgstck.icpntr[_dgstck.itop]] );
		return;
	    }
	}

	/*
	 * Check for parameter already computed and stored in an
	 * internal grid.
	 */
	if ( *ignumv == 0 ) {
	    dg_fndv ( time1, time2, level1, level2, ivcord, gvect, ignumu,
	        ignumv, iret );
	}
    }

    return;
}
