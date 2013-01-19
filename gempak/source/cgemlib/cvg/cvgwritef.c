#include "cvgcmn.h"
#include "pgprm.h"

void cvg_writef ( VG_DBStruct *el, int start, int numbytes, 
		char *fname, Boolean inc_place, int *location, int *iret )
/************************************************************************
 * cvg_writef								*
 *									*
 * This function writes an element record to an unopened VG file.	*
 *									*
 * cvg_writef ( el, start, numbytes, fname, inc_place, location, iret )	*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to VG record structure	*
 *	start		int		Offset to start VG record	*
 *					( -1 indicates write to EOF )	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fname		char		VG file name for output		*
 *	inc_place	Boolean		flag to include VG in placement *
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
 * S. Danz/AWC		07/06	Add flag to send objects to placement	*
 * E. Safford/SAIC	06/07	pass lower level cvg error code upward	*
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
    cvg_write ( el, start, numbytes, ofp, inc_place, &ier );
    if ( ier != 0 )  *iret = ier;

    cvg_clos( ofp, &ier );
    if ( ier != 0 )  *iret = -2;
    
}
