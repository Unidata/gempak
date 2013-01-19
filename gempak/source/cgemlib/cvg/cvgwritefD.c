#include "cvgcmn.h"
#include "pgprm.h"

void cvg_writefD ( VG_DBStruct *el, int start, int numbytes, 
				char *fname, int *location, int *iret )
/************************************************************************
 * cvg_writefD								*
 *									*
 * This function writes an element record to an unopened VG file	*
 * *WITHOUT* consideration of placement.  This gives the option of using*
 * VG files without needing device transformations (which placement     *
 * will need to get the meta data populated).                           *
 * NOTE: This is actually uses the original cvg_writef, so the log has  * 
 * been kept from that version.                                         *
 *									*
 * cvg_writefD ( el, start, numbytes, fname, location, iret )		*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG file name for output		*
 *									*
 * Output parameters:							*
 *	*location	int		location of the element in file *
 *	*iret		int		Return code			*
 *					-1  = error opening VGF file	*
 *					-2  = error closing VGF file	*
 *					-15 = error creating VG file    *
 *					-17 = error writing to VG file	*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created	based on cvg_writelm() 	        *
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		ier;
    char	outfile[FILE_FULLSZ], newfil[256];
    FILE	*ofp;
    long	fsize;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Find the proper file to write.
     */ 
    if ( !fname ) {
 	strcpy( outfile, work_file );
    }
    else {
	cst_ncpy( outfile, fname, sizeof( outfile ) - 1, &ier ); 
    }
  
  
    /*
     *  Inquire if the file exists. If not, create it. 
     */
    cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    if ( ier < 0 ) {
        cvg_crvgf( outfile, &ier );
	if ( ier < 0 ) {
	    *iret = -15;
	    return;
	}
	cfl_inqr( outfile, NULL, &fsize, newfil, &ier);
    }
   
    
    /*
     *  Record the position of the element in the file.
     */
    if ( start == -1 )
        *location = (int)fsize;
    else
        *location = start; 
	

    /*
     *  Open file for update. 
     */
    cvg_open( outfile, TRUE, &ofp, &ier ); 
    
    if ( ( ier != 0 ) || ( ofp == NULL ) ) {
         *iret = -1;
         return;
    }


    /*
     *  Write to file & close.
     */    
    cvg_writeD ( el, start, numbytes, ofp, &ier );
    if ( ier != 0 )  *iret = -17;

    cvg_clos( ofp, &ier );
    if ( ier != 0 )  *iret = -2;
    
}
