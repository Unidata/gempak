#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"


void cvg_redraw ( char *fname, int *iret )
/************************************************************************
 * cvg_redraw								*
 *									*
 * This function re-displays all records in a given VG file. If no file	*
 * name is given, records in WORK_FILE will be redrawn.			*
 *									*
 * cvg_redraw  ( fname, iret )						*
 *									*
 * Input parameters:							*
 *	*fname		char		File name to redraw from	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 **									*
 * Log:									*
 * J. Wu/SAIC           12/01   modify from cvg_rfrsh()			*
 * J. Wu/SAIC           02/02   add layering with cvg_drawLayer()	*
 * J. Wu/SAIC           03/02   change loop max. from 2 to MAX_LAYERS	*
 * J. Wu/SAIC           03/02   set proper color/fill mode for redraw	*
 * E. Safford/SAIC	03/02	rm the display-on check for cur layer	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * S. Danz/AWC		08/06	Call cvg_rebuildplace to update placement	*
 *							before the redraw in case the device changed*
 ***********************************************************************/
{
    int 		ii, ier, icol, ifill, cur_layer;
    long		maxbytes;
    char		newfil[256], reqfil[256];
    FILE		*fp;
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    cur_layer = pglayer_getCurLayer ();
    
    if (!fname)
	strcpy( reqfil, work_file );
    else
 	strcpy( reqfil, fname );

    /*
     * Update the placement info
     */
    cvg_rebuildplace(fname, &ier);

    /* 
     *  Open the specified file or WORK_FILE if it exists.
     */
    cfl_inqr( reqfil, NULL, &maxbytes, newfil, &ier );

    /*
     *  If empty file just return, otherwise, attempt to open the file.
     *  Report an error if applicable.
     */
    if (maxbytes == 0) {
        return;
    }
    else {
	fp = (FILE *) cfl_ropn( newfil, NULL, &ier );
	if ( ( ier != 0 ) || ( fp == NULL ) ) {
	    *iret = -1;
	    return;
	}
    }

    /*
     *  Draw elements layer by layer except those on the current layer.       
     */
    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {
        			
        icol = 0;
        ifill = G_TRUE;

	/*
	 *  skip current layer, layers not in use or dsply_on is off.
	 */ 	
	if ( ii == cur_layer || ( !pglayer_getInUse(ii) ) || 
	                        ( !pglayer_getDsplOn(ii) ) ) {
	    continue;
	}
	
	/*
	 *  Set required color & fill mode.
	 */ 	
	if ( !pglayer_getDsplClr(ii) )  icol = pglayer_getMonoClr ( ii );
	
	if ( !pglayer_getFill(ii) )  ifill = G_FALSE;

	cds_scol ( icol, iret );
	cds_sfill ( &ifill, &ier );
	
	/*
	 *  Draw elements.
	 */
	cvg_drawLayer ( fp, newfil, ii, (int)maxbytes, &ier );
		 
    }


    /*
     *  Always draw elements on the current layer in full color &
     *  fill mode if its dspl_on flag is on.       
     */
    icol = 0;
    ifill = G_TRUE;
    cds_scol ( icol, iret );
    cds_sfill ( &ifill, &ier );
    cvg_drawLayer ( fp, newfil, cur_layer, (int)maxbytes, &ier ); 


    cfl_clos( fp, &ier );
    if ( ier != 0 ) {
	*iret = -2;
    }

    geplot(&ier);

}
