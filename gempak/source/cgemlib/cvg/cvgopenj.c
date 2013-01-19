#include "cvgcmn.h"

void cvg_openj ( char *filnam, int wrtflg, FILE **fptr, int *iret )
/************************************************************************
 * cvg_openj								*
 *									*
 * This function opens a VG file.					*
 * Note: It doesn't attempt to create a vg file if specified file       *
 *       doesn't exist.			                                *
 *                                                                      *
 * cvg_openj ( filnam, wrtflg, fptr, iret )				*
 *									*
 * Input parameters:							*
 *	*filnam		char		VG filename			*
 *	wrtflg		int		Write flag			*
 *									*
 * Output parameters:							*
 *	**fptr		FILE		File pointer			*
 *	*iret		int		Return code			*
 *					   2 = created vg file		*
 *					   0 = normal 			*
 *					  -1 = error opening VG file	*
 *					 -13 = error verifying vg file  *
 *					 -25 = no filehead elm in file  *
 *					 -47 = no file name specified   *
 **									*
 * Log:									*
 * Q. Zhou/Chug         01/10   Copied from cvf_open			*
 * Q. Zhou/Chug         01/10   remove er_lmsg and cvg_crvgf function   *
 ***********************************************************************/
{
    int			ier, flag, curpos;
    long		maxbytes;
    int			err_log, dtl_log, dbg_log;
    VG_DBStruct		el;
    char		grp[4], newfil[256];
/*---------------------------------------------------------------------*/
    *fptr = NULL;

    strcpy(grp, "CVG");
    err_log  = 0;
    dtl_log  = 2;
    dbg_log  = 4; 

    *iret = 0;

    /*  Check to see if there is a file of the specified name, 
     *  open it if it is there.
     */    

    if ( !filnam) {
	*iret  = -47;
	return;
    }

    cfl_inqr(filnam, NULL, &maxbytes, newfil, &ier);
    
    if ( ier < 0 && wrtflg == G_TRUE ) {
	*iret  = -1;
	return;
    }

    /*  
     *  Open file with either read, or read-write permissions
     */
    if (wrtflg == G_TRUE) {
	*fptr = cfl_uopn(filnam, &ier);
    }
    else {
	*fptr = cfl_ropn(filnam, " ", &ier);
    }

    if (( ier != 0 ) || ( *fptr == NULL )) {
	*iret = -1;
    }
    else {
	if ( (size_t)maxbytes < (sizeof(FileHeadType) + sizeof(VG_HdrStruct)) ) {
	    *iret = -25;
	}
        else {
            /* 
	     * Check to see that the file is a valid VGF file...
	     */
	    curpos = 0;
            cvg_rdjhdr(newfil, *fptr, curpos, (int)maxbytes, &el, &flag, &ier);

	    if (ier != 0) {
                *iret = -13;
    	    }

	    if (el.hdr.vg_type != FILEHEAD_ELM) {
	        *iret = -25;
	    }
	}

    }
}
