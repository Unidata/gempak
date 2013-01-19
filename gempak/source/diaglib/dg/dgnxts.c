#include "dg.h"

void dg_nxts ( int *num, int *iret )
/************************************************************************
 * dg_nxts								*
 *									*
 * This subroutine returns the next internal grid number to be used	*
 * for a scalar grid.							*
 *									*
 * dg_nxts ( num, iret )						*
 *									*
 * Output parameters:							*
 *	*num		int		Grid number			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = internal grid list full	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 5/88	Renamed from DG_NXTG			*
 * G. Huffman/GSC	 9/88	New error messages			*
 * K. Brill/NMC          4/93	Change to wrap around int grids		*
 * L. Sager/NMC		 5/93	Change .le. to .lt. in NDGRD test	*
 * M. desJardins/NMC	 7/93	Cleaned up				*
 * S. Jacobs/NMC	11/94	Reset grid header information		*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg			*
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Assign ISUBID to IUSESV; call ER_WMSG	*
 * K. Brill/HPC		 5/02	Documentation of IUSESV			*
 * S. Jacobs/NCEP	11/02	Add check for savflg before resetting	*
 * R. Tian/SAIC		 2/06	Recoded/Modified from Fortran		*
 ************************************************************************/
{
    int minus1, ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *num = 0;
    minus1 = -1;

    /*
     * Searching for the next available grid, otherwise, allocate one.
     */
    for ( ii = 0; ii < _dggrid.maxdgg; ii++ ) {
        if ( _dggrid.idglst > _dggrid.maxdgg - 1 ) _dggrid.idglst = 0;

	if ( ( _dggrid.iusesv[_dggrid.idglst] == 0 ) &&
	     ( _dggrid.savflg[_dggrid.idglst] == G_FALSE ) ) {
	    *num = _dggrid.idglst + 1;

	    if ( _dggrid.dgg[_dggrid.idglst].size == _dgfile.kxyd ) {
	        /*
		 * An available grid is found.
		 */
	    } else if ( _dggrid.dgg[_dggrid.idglst].size == 0 ) {
		/*
		 * Allocate a new grid.
		 */
	        dg_newg ( &_dgfile.kxyd, &(_dggrid.dgg[_dggrid.idglst].grid), 
		          &ier );
	        if ( ier != 0 ) {
		    *iret = -1;
		    return;
	        }
		_dggrid.dgg[_dggrid.idglst].size = _dgfile.kxyd;
	    } else {
	        /*
		 * Something goes wrong.
		 */
		*num = 0;
	        dg_merr ( "Grid size mismatch", "", "", &minus1,
		          &minus1, &minus1, _dgerr.errst, &ier );
	        break;
	    }
#ifdef MEM_DEBUG
	    printf ( "Allocatd grid %d at %p with size: %d\n",
	        _dggrid.idglst+1, 
		(void *)_dggrid.dgg[_dggrid.idglst].grid, 
		_dggrid.dgg[_dggrid.idglst].size );
#endif
            /*
             * Set the in-use integer value to current subroutine ID.
             */
            _dggrid.iusesv[_dggrid.idglst] = _dggrid.isubid;

            /*
             * Reset the rest of the grid information.
             */
            _dggrid.dttimd1[_dggrid.idglst][0] = '\0';
            _dggrid.dttimd2[_dggrid.idglst][0] = '\0';
            _dggrid.leveld1[_dggrid.idglst] = 0;
            _dggrid.leveld2[_dggrid.idglst] = 0;
            _dggrid.ivcrdd[_dggrid.idglst] = 0;;
            _dggrid.gparmd[_dggrid.idglst][0] = '\0';
            _dggrid.savflg[_dggrid.idglst] = G_FALSE;
	}

	/*
	 * Increase grid index.
	 */
	_dggrid.idglst++;

	if ( *num != 0 ) break;
    }

    if ( *num == 0 ) {
	*iret = -10;
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
    }

    return;
}
