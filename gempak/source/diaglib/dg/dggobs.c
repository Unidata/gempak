#include "dg.h"

void dg_gobs ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, int *igu, int *igv,
	       int *iret )
/************************************************************************
 * dg_gobs								*
 *									*
 * This subroutine retrieves the observed wind from the grid file.	*
 * The u- and v-components are returned in meters/second in grid	*
 * relative coordinates.  GOBS does not decrement ITOP, allowing	*
 * for multiple calls in computing layer quantities.			*
 *									*
 * dg_gobs ( time1, time2, level1, level2, ivcord, igu, igv, iret )	*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Vertical level			*
 *	*level2		const int	Vertical level			*
 *	*ivcord		const int	Vertical coordinate		*
 *									*
 * Output parameters:							*
 *	*igu		int		U grid number			*
 *	*igv		int		V grid number			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid ... cannot be found	*
 *					-10 = internal grid list full	*
 **									*
 * Log:									*
 * G. Huffman/GSC	 9/88	Adapted from DV_OBS			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 4/89	Add check for grid relative winds	*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * K. Brill		 9/89   Set IFILED				*
 * J. Whistler/SSAI	 6/91	Flipped u and v arrays in PD_SDUV call	*
 * K. Brill/NMC		 4/93	Save wind as UOBS, VOBS			*
 * M. desJardins/NMC	 7/93	Update file number			*
 * S. Jacobs/EAI	11/93	Change GD_RDAT to DG_RDAT		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * T. Lee/SAIC		 1/04	Free internal grids when errs		*
 * R. Tian/SAIC		 1/04	Changed GR_R/WOBS to DG_R/WOBS		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char wparm[14];
    int igx, igy, wmks, wcmp, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get numbers for the vector.
     */
    dg_nxtv ( igu, igv, iret );
    if ( *iret != 0 ) return;

    /*
     * Check file for grid relative components.
     */
    dg_robs ( &_dgfile.idlun, time1, time2, level1, level2, ivcord, 
	_dggrid.dgg[(*igu)-1].grid, _dggrid.dgg[(*igv)-1].grid, 
	&igx, &igy, iret );

    /*
     * If not found, get the observed wind relative to north.
     */
    if ( *iret != 0 ) {
	dg_wobs ( &_dgfile.idlun, time1, time2, level1, level2, ivcord, 
	    _dggrid.dgg[(*igu)-1].grid, _dggrid.dgg[(*igv)-1].grid, 
	    &wcmp, &wmks, wparm, &igx, &igy, iret );

	if ( *iret == 0 ) {
	    /*
	     * Convert from speed/direction to u,v components, if needed.
	     */
	    if ( wcmp == G_FALSE ) {
		pd_sduv ( _dggrid.dgg[(*igv)-1].grid,
		          _dggrid.dgg[(*igu)-1].grid, &_dgfile.kxyd,
			  _dggrid.dgg[(*igu)-1].grid, 
	                  _dggrid.dgg[(*igv)-1].grid, &ier );
	    }

	    /*
	     * Convert from knots to m/sec (MKS), if needed.
	     */
	    if ( wmks == G_FALSE ) {
		pd_knms ( _dggrid.dgg[(*igu)-1].grid, &_dgfile.kxyd,
			  _dggrid.dgg[(*igu)-1].grid, &ier );
		pd_knms ( _dggrid.dgg[(*igv)-1].grid, &_dgfile.kxyd,
			  _dggrid.dgg[(*igv)-1].grid, &ier );
	    }

	    /*
	     * Convert to grid relative components.
	     */
	    dg_grel ( _dggrid.dgg[(*igu)-1].grid, 
		      _dggrid.dgg[(*igv)-1].grid,
		      _dggrid.dgg[(*igu)-1].grid, 
		      _dggrid.dgg[(*igv)-1].grid, 
		      &ier );
	}
    }

    /*
     * Update both grid headers.  Use wind type as parameter name.
     */
    if ( *iret == 0 ) {
	dg_upvg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
	          "OBS", igu, igv, &ier );
    } else {
	*iret = -7;
	dg_merr ( "WND", time1, time2, level1, level2, ivcord, _dgerr.errst,
	          &ier );
	dg_frig ( igu, &ier );
	dg_frig ( igv, &ier );
    }

    return;
}
