#include "dg.h"

void dg_mhdr ( const char *func, const int *num1, const int *num2,
               char *time1, char *time2, int *level1, int *level2,
	       int *jvcord, char *parm, int *iret )
/************************************************************************
 * dg_mhdr								*
 *									*
 * This subroutine takes a function name and two grids and constructs	*
 * a name for the output grid and finds the time, level, and vertical	*
 * coordinate to use to update the header.				*
 *									*
 * dg_mhdr ( func, num1, num2, time1, time2, level1, level2, jvcord,	*
 *           parm, iret )						*
 *									*
 * Input parameters:							*
 *	*func		const char	Function computed		*
 *	*num1		const int	Pointer to first input grid	*
 *	*num2		const int	Pointer to second input grid	*
 *									*
 * Output parameters:							*
 *	*time1		char		Grid time			*
 *	*time2		char		Grid time			*
 *	*level1		int		Grid level			*
 *	*level2		int		Grid level			*
 *	*jvcord		int		Vertical coordinate		*
 *	*parm		char		Parameter name			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 9/97	Changed IVCORD to LVCORD		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char p1[LLMXLN], p2[LLMXLN], g1;
    int nnn[2], n, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * If there are no input grids, use user input information.
     */
    if ( ( (*num1) <= 0 ) && ( (*num2) <= 0 ) ) {
	strcpy ( time1, _dginpt.ddttim1 );
	strcpy ( time2, _dginpt.ddttim2 );
	*level1 = _dginpt.ldlevl1;
	*level2 = _dginpt.ldlevl2;
	*jvcord = _dginpt.lvcord;
	strcpy ( parm, func );
	
	return;
    }

    /*
     * Get the grids that are input.  Make sure the first grid is
     * non-zero.
     */
    n = 0;
    if ( (*num1) > 0 ) {
	nnn[n++] = *num1;
    }
    if ( (*num2) > 0 ) {
	nnn[n++] = *num2;
    }
    if ( n != 2 )  nnn[1] = 0;

    /*
     * Make the output grid name.
     */
    p1[0] = '\0';
    p2[0] = '\0';
    if ( (*num1) > 0 ) strcpy ( p1, _dggrid.gparmd[(*num1)-1] );
    if ( (*num2) > 0 ) strcpy ( p2, _dggrid.gparmd[(*num2)-1] );
    g1 = p1[0];
    if ( g1 == 'U' ) {
	memmove ( p1, &p1[1], strlen(&p1[1])+1 );
	if ( strcmp ( p1, "OBS" ) == 0 ) strcpy ( p1, "WND" );
    }
    g1 = p2[0];
    if ( g1 == 'V' ) {
	memmove ( p2, &p2[1], strlen(&p2[1])+1 );
	if ( strcmp ( p2, "OBS" ) == 0 )  strcpy ( p2, "WND" );
    }
    dg_mnam ( func, p1, p2, parm, &ier );

    /*
     * Get information from first grid.
     */
    if ( nnn[0] > 0 ) {
	strcpy ( time1, _dggrid.dttimd1[nnn[0]-1] );
	strcpy ( time2, _dggrid.dttimd2[nnn[0]-1] );
	*level1 = _dggrid.leveld1[nnn[0]-1];
	*level2 = _dggrid.leveld2[nnn[0]-1];
	*jvcord = _dggrid.ivcrdd[nnn[0]-1];
    }

    /*
     * Get information from second grid.
     */
    if ( (*num2) > 0 ) {
	/*
	 * Check for different time.
	 */
	if ( strcmp ( _dggrid.dttimd1[nnn[0]-1], _dggrid.dttimd1[nnn[1]-1] )
	     != 0 ) {
	    strcpy ( time2, _dggrid.dttimd1[nnn[1]-1] );
	}

	/*
	 * Check for different levels.
	 */
	if ( _dggrid.leveld1[nnn[0]-1] != _dggrid.leveld1[nnn[1]-1] ) {
	    *level2 = _dggrid.leveld1[nnn[1]-1];
	}
    }

    return;
}
