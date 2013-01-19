#include "cvgcmn.h"
#include "pgprm.h"


void cvg_writgfa ( FILE *fptr, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_writgfa								*
 *									*
 * This function writes a GFA element to file at the CURRENT location.	*
 * 									*
 * Note: if a different location is desired, use cfl_seek before calling*
 *       this funcion.							*
 *									*
 * cvg_writgfa ( fptr, el, iret )					*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	*el		VG_DBStruct	Pointer to a GFA element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -6  = No file has been opened	*
 *					 -28 = Insufficient points	* 
 *					 -53 = no blocks allocated	*
 **									*
 * J. Wu/SAIC		01/04	create					*
 * J. Wu/SAIC		10/04	rewrite for new GFA structure		*
 * E. Safford/SAIC	06/07	do not write elements with insufficient *
 *				 points (mins =  2 if open, 3 if closed)*
 ***********************************************************************/
{
    int		ier, nbytes, ii, nblks, npts, tmp, one = 1;
    int		swapBlks = 0, swapPts = 0, closed = 1;
    int		minPts = 3;

    char	hazard[ 32 ];
/*---------------------------------------------------------------------*/
	
    *iret = 0;

    if ( fptr == NULL ) {
	*iret = -6;
	return;
    }

    /*
     *  Get the number of blocks and the number of points.
     */
    nblks = el->elem.gfa.info.nblocks;
    npts = el->elem.gfa.info.npts;
    
    /*
     *  Swap the number of blocks/points for some platforms before
     *  using them to write out blocks and lat/lons.
     */
    if ( MTMACH == MTULTX ||
	 MTMACH == MTALPH ||
	 MTMACH == MTLNUX )   {
	 
	mv_swp4 ( &one, &nblks, &tmp );
	swapBlks = tmp;

	mv_swp4 ( &one, &npts, &tmp );
	swapPts = tmp;    
    }
    else {
	swapBlks = nblks;
	swapPts  = npts;
    }

    if( swapBlks <= 0 ) {
	*iret = -53;
	return;
    }

    /*
     *  Verify the minimum number of points are present.  Closed GFAs must
     *  have at least 3 points, open FZLVL contours must have at least 2.
     */
    cvg_getFld ( el, TAG_GFA_AREATYPE, hazard, &ier );
    closed = ( int )el->hdr.closed;

    if( ( strcmp( hazard, "FZLVL" ) == 0 ) && !closed ) {
	minPts = 2;
    }

    if( swapPts < minPts ) {
	*iret = -28;
	return;
    } 


    /*
     *  Write the header.
     */
    nbytes = sizeof(VG_HdrStruct);
    cfl_writ ( fptr, nbytes, (unsigned char *)&el->hdr, &ier );


    /*
     *  Write the raw (potentially swapped) nblks and npts values out.
     */
    nbytes = sizeof(int);
    cfl_writ ( fptr, nbytes, (unsigned char *)&nblks, &ier );
    cfl_writ ( fptr, nbytes, (unsigned char *)&npts, &ier );
        

    /*
     *  Write all blocks.
     */
    nbytes = sizeof(char) * STD_STRLEN;           
    for ( ii = 0; ii < swapBlks; ii++ ) {
        cfl_writ ( fptr, nbytes, 
	          (unsigned char *)el->elem.gfa.info.blockPtr[ii], &ier );
    }

    
    /*
     *  Write the latlon array.
     */    
    nbytes = sizeof(float) * swapPts * 2;
    cfl_writ ( fptr, nbytes, (unsigned char *)&el->elem.gfa.latlon, &ier );

}

