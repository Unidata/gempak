#include "cvgcmn.h"

void cvg_valid ( char *filnam, int *iret )
/************************************************************************
 * cvg_valid								*
 *									*
 * This function checks validity of a VG file.				*
 *									*
 * cvg_valid ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		VG filename			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   1 = VG file does not exist	*
 *					   0 = normal 			*
 *					  -1 = error opening VG file	*
 *					 -13 = error verifying vg file  *
 *					 -25 = no filehead elm in file  *
 *					 -47 = no file name specified   *
 **									*
 * Log:									*
 * T. Lee/SAIC		05/02	created					*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int			ier, flag, curpos;
    long		maxbytes;
    VG_DBStruct		el;
    char		newfil[256];
    FILE		*fptr;
/*---------------------------------------------------------------------*/
    *iret = 0;
    fptr = NULL;

    /*  	
     * Check to see if there is a file of the specified name.
     */    

    if ( strlen (filnam) <= (size_t)0 ) {
	*iret = -47;
	return;
    }

    cfl_inqr(filnam, NULL, &maxbytes, newfil, &ier);
    
    if (ier < 0) {

	/*
	 *  if file does not exist retrun error code 1.
	 */
	*iret = 1;
	return;
    }

    /*  
     *  Open the file. 
     */

    fptr = cfl_ropn (newfil, NULL, &ier);

    if ( ( ier != 0 ) || ( fptr == NULL ) ) {	   /* open failed */
	*iret = -1;
        return;
    }
    else {

	if ( maxbytes < (long)(sizeof(FileHeadType) + sizeof(VG_HdrStruct)) ) {
	    *iret = -25;
	}
        else {
            /* 
	     * Check to see that the file is a valid VGF file...
	     */
	    curpos = 0;
            cvg_rdhdr (newfil, fptr, curpos, (int)maxbytes, &el, &flag, &ier);
	    if (ier != 0) {
                *iret = -13;
    	    }

	    if (el.hdr.vg_type != FILEHEAD_ELM) {
	        *iret = -25;
	    }
	}
	cfl_clos ( fptr, &ier );
    }
}
