#include "cvgcmn.h"

void cvg_rdjhdr ( char *fname, FILE *fp, int start, int size, 
				VG_DBStruct *el, int *flag, int *iret );
void cvg_rdjele ( VG_DBStruct *el, int el_start, int el_size, 
				FILE *fp, int *iret );


void cvg_rdjrecnoc ( char *fname, FILE *fptr, int fpos, VG_DBStruct *el, 
                    int *iret )
/************************************************************************
 * cvg_rdjrecnoc							*
 *									*
 * This function reads a VG record from an opened VG file based upon 	*
 * the file position.							*
 *									*
 * cvg_rdjrecnoc ( fname, fptr, fpos, el, iret )			*
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
 *									*
 * Log:									*
 * S. Jacobs/NCEP	12/09	Copied from cvg_rdrecnoc		*
 * S. Jacobs/NCEP	12/09	Changed to call cvg_rdjele		*
 * Q. Zhou/Chug		02/10   Added function declaration		*
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
    cvg_rdjhdr ( fname, fptr, fpos, (int)size, el, &flag, &ier );
    
    if ( ier != 0 ) {
	*iret = -13;
    }
    else {
        /*
         *  Read the VG element.
         */
        cvg_rdjele ( el, fpos, el->hdr.recsz, fptr, &ier );
        if ( ier < 0 ) {
	    *iret = -14;
        }
    }

}

