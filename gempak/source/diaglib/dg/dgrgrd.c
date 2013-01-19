#include "dg.h"

void dg_rgrd ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_rgrd								*
 *									*
 * This subroutine reads a grid from a grid file into an internal	*
 * grid array which is in the common area.				*
 *									*
 * dg_rgrd ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
 *									*
 * Input parameters:							*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *									*
 * Output parameters:							*
 *	*num		int		Number of internal grid		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 *					-12 = must be a scalar		*
 *					-16 = map proj is invalid	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME  is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. Goodman/RDS	11/85   Fixed call to GD_RDNUM			*
 * M. desJardins/GSFC	 7/87	Changed to GEMPAK4 GD library		*
 * M. desJardins/GSFC	 5/88	Fixed scaling functions			*
 * G. Huffman/GSC        9/88	Error messages				*
 * K. Brill/GSC          9/89   IFILED					*
 * M. desJardins/NMC	 7/93	Fixed ifiled				*
 * S. Jacobs/EAI	11/93	Changed GD_RDAT to DG_RDAT		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * R. Tian/SAIC		10/03	Removed GD_GGRD, added DG_NRDT		*
 * T. Lee/SAIC		 1/04	Free internal grid when errs 		*
 * R. Tian/SAIC		 2/05	Recoded from Fortran			*
 ************************************************************************/
{
    int gidx, igx, igy, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get the next grid number to use.
     */
    dg_nxts ( num, iret );
    if ( *iret != 0 ) return;

    gidx = (*num) - 1;
    if ( _nfile.nucode == G_TRUE ) {
	dgc_nrdt ( &_dgfile.idlun, time1, time2, level1, level2, ivcord,
	    parm, _dggrid.dgg[gidx].grid, &igx, &igy, _dgfile.ighdr, &ier );
    } else {
 	*iret = -7;
	return;
    }

    /*
     * Check for alternate parameter names.
     */
    if ( ier != 0 ) {
	dg_chck ( time1, time2, level1, level2, ivcord, parm, num, iret );
	if ( *iret != 0 ) {
	    dg_merr ( parm, time1, time2, level1, level2, ivcord,
	              _dgerr.errst, &ier );
	}
    /*
     * Check for error in grid size.
     */
    } else if ( ( igx != _dgfile.kxd ) || ( igy != _dgfile.kyd ) ) {
	*iret = -8;
	dg_merr ( parm, time1, time2, level1, level2, ivcord,
	          _dgerr.errst, &ier );
    /*
     * Otherwise the grid was found.
     */
    } else {

	/*
	 * Check for parameters which must be scaled.
	 */
	dg_scal ( parm, num, &ier );

	/*
	 * Update grid header.
	 */
	dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
	          parm, num, &ier );
    }
    
    return;
}
