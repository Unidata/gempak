#include "dg.h"

void dg_grdr ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_grdr								*
 *									*
 * This subroutine reads a grid from a grid file into an internal	*
 * grid array which is in the common area.  The grid will be found 	*
 * in the grid location pointed to by NUM.  If NUM = 0 on input, 	*
 * the next grid location will be used and returned.  If NUM > 0, 	*
 * the grid will be found at NUM.					*
 *									*
 * This subroutine is the same as the last part of subroutine DG_RGRD.	*
 * It is used to avoid problems with recursion.				*
 *									*
 * dg_grdr ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
 *									*
 * Input parameters:							*
 *      *time1          const char      Date/time                       *
 *      *time2          const char      Date/time                       *
 *      *level1         const int       Level                           *
 *      *level2         const int       Level                           *
 *      *ivcord         const int       Vertical coordinate             *
 *      *parm           const char      Parameter name                  *
 *									*
 * Input and output parameters:						*
 *	*num		int		Number of internal grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/87	Changed to GEMPAK4 GD library		*
 * M. desJardins/GSFC	 5/88	Fix scaling problems			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 9/89   Set IFILED				*
 * M. desJardins/NMC	 7/93	DG_UHDR --> DG_UPSG			*
 * S. Jacobs/EAI	11/93	Changed GD_RDAT to DG_RDAT		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * R. Tian/SAIC		10/03	Added DG_NRDT call			*
 * T. Lee/SAIC		 1/04	Free internal grid when errs		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int igx, igy, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Read in the grid data.
     */
    if ( _nfile.nucode == G_TRUE ) {
	dgc_nrdt ( &_dgfile.idlun, time1, time2, level1, level2, ivcord,
	    parm, _dggrid.dgg[(*num)-1].grid, &igx, &igy, _dgfile.ighdr, &ier );
    } else {
    	*iret = -7;
	return;
    }

    /*
     * Check for errors.
     */
    if ( ier != 0 ) {
	*iret = -7;
	dg_merr ( parm, time1, time2, level1, level2, ivcord,
	          _dgerr.errst, &ier );
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
	dg_upsg (time1, time2, level1, level2, ivcord, &_dgfile.idlun,
	         parm, num, &ier);
    }

    return;
}
