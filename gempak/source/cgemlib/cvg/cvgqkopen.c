#include "cvgcmn.h"

void cvg_qkopen ( char *filnam, FILE **fptr, int *bytes_inFile, int *iret )
/************************************************************************
 * cvg_qkopen								*
 *									*
 * This function opens a VG file.  It differs from cvg_open in that 	*
 * only minimal error checking is done and no errors are reported.  Any *
 * error detected will result in a iret value of -1.  This routine is   *
 * designed to be used on files previously opened by the more rigorous  *
 * cvg_open.  All files will be opened with write access.		*
 *									*
 * cvg_qkopen  ( filnam, fptr, bytes_inFile, iret )			*
 *									*
 * Input parameters:							*
 *	*filnam		char		VG filename			*
 *									*
 * Output parameters:							*
 *	**fptr		FILE		File pointer			*
 *	*bytes_inFile   int		number of bytes in file		*
 *	*iret		int		Return code			*
 *					   0 = normal 			*
 *					  -1 = error opening VG file	*
 **									*
 * Log:									*
 * E. Safford/GSC	10/98	copied from cvg_open         		*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL to int for LINUX	*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * A. Hardy/GSC          1/01   changed fptr from int to FILE           *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * S. Jacobs/NCEP	 3/06	Fixed 64bit long problem		*
 ************************************************************************/
{
    int			ier;
    char		newfil[256];
    long		numbytes;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  Check to see if there is a file of the specified name, 
     *  open it if it is there.
     */    

    cfl_inqr(filnam, NULL, &numbytes, newfil, &ier);
    *bytes_inFile = (int) numbytes;
    
    *fptr =  cfl_uopn(filnam, &ier);


    if (ier < 0) {
	*bytes_inFile = 0;
	*fptr = NULL;
        *iret = -1;
    }
}
