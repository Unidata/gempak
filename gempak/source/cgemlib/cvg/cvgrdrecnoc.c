#include "cvgcmn.h"

void cvg_rdrecnoc ( char *fname, FILE *fptr, int fpos, VG_DBStruct *el, 
                    int *iret )
/************************************************************************
 * cvg_rdrecnoc								*
 *									*
 * This function reads a VG record from an opened VG file based upon 	*
 * the file position.							*
 *									*
 * cvg_rdrecnoc ( fname, fptr, fpos, el, iret )				*
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name			*
 *	*fptr		FILE		File pointer to read from 	*
 *	fpos		int		File position to read from 	*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	*iret		int		Return code			*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/02	move from cvgrdrec.c 			*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		ier, flag;
    long	size;
    char	newfil[133];
/*---------------------------------------------------------------------*/

    *iret = 0;
        
    if ( fptr == NULL ) {
	*iret = -8;
        return;
    }

    
    /*
     *  Inquire the size of the VG file.
     */
    cfl_inqr ( fname, NULL, &size, newfil, &ier );


    /*
     *  Read the VG header.
     */
    cvg_rdhdr ( fname, fptr, fpos, (int)size, el, &flag, &ier );
    
    if ( ier != 0 ) {
	*iret = -13;
    }
    else {
        /*
         *  Read the VG element.
         */
        cvg_rdele ( el, fpos, el->hdr.recsz, fptr, &ier );
        if ( ier < 0 ) {
	    *iret = -14;
        }
    }

}

