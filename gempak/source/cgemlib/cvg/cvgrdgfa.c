#include "cvgcmn.h"
#include "pgprm.h"

void cvg_rdgfa ( FILE *fptr, long fil_pos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdgfa								*
 *									*
 * This function reads a GFA element from the given file. 		*
 *									*
 * cvg_rdgfa ( fptr, fil_pos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	fil_pos		long		Position to read from		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Element structure		*
 *	*iret		int		Return code			*
 *					 -3 = Seek to a bad location	*
 *					 -8 = No file has been opened	*
 *					 -14 = Fail to read elem	*
 **									*
 * J. Wu/SAIC		01/04	create					*
 * J. Wu/SAIC		10/04	overhaul for the new GFA structure	*
 ***********************************************************************/
{
    int		ier, nbytes, nbin, nblks, ii, npts, tmp, one = 1;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( fptr == NULL ) {
        *iret = -8;
        return;
    }


    /*
     *	Seek to the given position.
     */
    cfl_seek( fptr, fil_pos, 0, &ier );
    if ( ier != 0 ) {
        *iret = -3;
        return;    
    }

    
    /*
     *	Read the number of blocks.
     *	Note: the number of blocks/points will be used in
     *        cvg_allocGfaBlock() to calculate the element's
     *        record size, so they must be swapped before 
     *        assigned and passed into the cvg_allocGfaBlock().
     *        Once the GFA element has been read, we need to
     *        swap the number of blocks/points one more time
     *        before they are passed back since the caller - 
     *        cvg_rdele() will call cvg_swap() to do another
     *        swap.
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&nblks, &nbin, &ier); 
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }
    
    if ( MTMACH == MTULTX ||
	 MTMACH == MTALPH ||
	 MTMACH == MTLNUX ) {
  	mv_swp4 ( &one, &nblks, &tmp );
	nblks = tmp;
    }

    if ( nblks < 0 || nblks > MAX_GFA_BLOCKS ) {
        *iret = -14;
        return;    
    }
        
    
    /*
     *	Read the number of points.
     */
    nbytes = sizeof( int );
    cfl_read ( fptr, nbytes, (void *)&npts, &nbin, &ier); 
    if ( nbin != nbytes ) {
        *iret = -14;
        return;
    }

    if ( MTMACH == MTULTX ||
	 MTMACH == MTALPH ||
	 MTMACH == MTLNUX ) {
  	mv_swp4 ( &one, &npts, &tmp );
	npts = tmp;
    }

    if ( npts < 0 || npts > MAXPTS ) {
        *iret = -14;
        return;    
    }

    el->elem.gfa.info.npts = npts;
    

    /*
     *	Allocate block pointers.
     */
    el->elem.gfa.info.nblocks = 0;
    while ( el->elem.gfa.info.nblocks < nblks ) {
        cvg_allocGfaBlock ( el );
    }

   
    /*
     *	Read the blocks.
     */
    nbytes = sizeof ( char ) * STD_STRLEN;    
    for ( ii = 0; ii < nblks; ii++ ) {
        cfl_read ( fptr, nbytes, (void *)el->elem.gfa.info.blockPtr[ii],
	                         &nbin, &ier); 
        if ( nbin != nbytes ) {
            *iret = -14;
            return;
        }    
    }
    

    /*
     *	Read the latlon points.
     */
    nbytes = sizeof(float) * npts * 2;    
    cfl_read ( fptr, nbytes, (void *) &el->elem.gfa.latlon, &nbin, &ier); 
    
    if ( nbin != nbytes ) {
        *iret = -14;
    }


    /*
     *	Swap back the number of blocks/points fields.
     */
    if ( MTMACH == MTULTX ||
	 MTMACH == MTALPH ||
	 MTMACH == MTLNUX ) {
  	
	mv_swp4 ( &one, &el->elem.gfa.info.nblocks, &tmp );
	el->elem.gfa.info.nblocks = tmp;
  	
	mv_swp4 ( &one, &el->elem.gfa.info.npts, &tmp );
	el->elem.gfa.info.npts = tmp;
    }
        
}
