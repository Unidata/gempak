#include "dg.h"

void dg_nxtv ( int *numu, int *numv, int *iret )
/************************************************************************
 * dg_nxtv								*
 *									*
 * This subroutine returns the next two internal grid numbers to be used*
 * for a vector.							*
 *									*
 * dg_nxtv ( numu, numv, iret )						*
 *									*
 * Output parameters:							*
 *	*numu		int		U component grid number		*
 *	*numv		int		V component grid number		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = internal grid list full	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 5/88	Renamed and changed variables		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/NMC		 4/93	Change to wrap around int grids		*
 * L. Sager/NMC		 5/93	Correct .le. to  .lt. in NDGRD test	*
 * M. desJardins/NMC	 7/93	Cleaned up				*
 * S. Jacobs/NMC	11/94	Reset grid header information		*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg			*
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Assign ISUBID to IUSESV; call ER_WMSG	*
 * K. Brill/HPC		 5/02	Documentation of IUSESV			*
 * S. Jacobs/NCEP	11/02	Add check for savflg before resetting	*
 * R. Tian/SAIC          2/06   Recoded/Modified from Fortran		*
 ************************************************************************/
{
    int minus1, ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *numu = 0; 
    *numv = 0; 
    minus1 = -1;

    /*
     * Loop throuth looking for two grids.
     */
    for ( ii = 0; ii < _dggrid.maxdgg; ii++ ) {    
        if ( _dggrid.idglst > _dggrid.maxdgg - 1 ) _dggrid.idglst = 0;

	if ( ( _dggrid.iusesv[_dggrid.idglst] == 0 ) &&
	     ( _dggrid.savflg[_dggrid.idglst] == G_FALSE ) ) {
	    if ( *numu == 0 ) {
		*numu = _dggrid.idglst + 1;
	    } else {
		*numv = _dggrid.idglst + 1;
	    }

	    if ( _dggrid.dgg[_dggrid.idglst].size == _dgfile.kxyd ) {
	        /*
		 * An available grid is found.
		 */
	    } else if ( _dggrid.dgg[_dggrid.idglst].size == 0 ) {
	        /*
		 * Allocatea a new grid.
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
		*numu = 0;
		*numv = 0;
		dg_merr ( "Grid size mismatch", "", "", &minus1,
		          &minus1, &minus1, _dgerr.errst, &ier );
	        break;
	    }
#ifdef MEM_DEBUG
            printf ( "Allocated grid %d at %p with size: %d.\n",
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
	    _dggrid.ivcrdd[_dggrid.idglst] = 0;
	    _dggrid.gparmd[_dggrid.idglst][0] = '\0';
	    _dggrid.savflg[_dggrid.idglst] = G_FALSE;
	}

	/*
	 * Increase grid index.
	 */
	_dggrid.idglst++;

	if ( *numu != 0 && *numv != 0 ) break;
    }

    /*
     * Check that two grids have been found.
     */
    if ( *numu == 0 || *numv == 0 ) {
	*iret = -10;
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
    }

    return;
}
