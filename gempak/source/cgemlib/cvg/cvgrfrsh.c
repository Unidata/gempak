#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"


void cvg_rfrsh ( char *fname, float dllx, float dlly, float durx, 
						float dury, int *iret )
/************************************************************************
 * cvg_rfrsh								*
 *									*
 * This function re-displays all VG records that fall within a specified*
 * range.								*
 *									*
 * cvg_rfrsh  ( fname, dllx, dlly, durx, dury, iret )			*
 *									*
 * Input parameters:							*
 *	*fname		char		File name to refresh from	*
 *	dllx		float		Lower left X coordinate		*
 *	dlly		float		Lower left Y coordinate		*
 *	durx		float		Upper right X coordinate	*
 *	dury		float		Upper right Y coordinate	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	11/96		Created				*
 * D. Keiser/GSC	 1/97		Clean up			*
 * E. Wehner/Eai	 2/97		Added selected flag		*
 * D. Keiser/GSC	 4/97		Remove unused variables		*
 * E. Wehner/EAi	 5/97		Add double tier refresh		*
 * D. Keiser/GSC	 5/97		Change cvg_dsply to cds_dspvg	*
 * E. Wehner/EAi	 8/97		Don't display centroids		*
 * C. Lin/EAI	         8/97		Change calling seq. cds_dspvg	*
 * E. Wehner/EAI	 9/97		Pass filename instead of grinfo	*
 * G. Krueger/EAI	 1/98		Ignore non-fatal read warnings.	*
 * F. Yen/NCEP           1/98   Updated calls for crg library cleanup   *
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * C. Lin/EAI	         5/98	bug fix, plus check filled area 	*
 * F. Yen/NCEP		 5/98	Changed cvg_dspvg to cvg_dspelm		*
 * J. Wu/SAIC           12/01   locate displaying level	with cvg_level  *
 * J. Wu/SAIC           01/02   add layer param to crg_get()		*
 * J. Wu/SAIC           02/02   add layering with cvg_rfrshLayer()	*
 * J. Wu/SAIC           03/02   set proper color/fill mode for refresh	*
 * E. Safford/SAIC	03/02	rm the dsply on condition on cur layer  *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 		ii, ier, icol, ifill, cur_layer;
    long		maxbytes;
    char		newfil[256], reqfil[256];
    FILE		*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  If the nmap2 information is available for placement, then update it
     */
    if (cvg_metadata) {
        cap_psplace(cvg_placements, cvg_metadata, &ier);
    }

    cur_layer = pglayer_getCurLayer ();

    if ( !fname )
	strcpy ( reqfil, work_file );
    else
 	strcpy ( reqfil, fname );

    /*  
     *  Check to see if there is a VG file of the specified name,
     *  open it if it is there.
     */
    cfl_inqr ( reqfil, NULL, &maxbytes, newfil, &ier );

    /*
     *  If empty file just return, otherwise, attempt to open the file.
     *  Report an error if applicable.
     */
    if ( maxbytes == 0 ) {
        return;
    }
    else {
	fp = (FILE *) cfl_ropn ( newfil, NULL, &ier );
	if ( ( ier != 0 ) || ( fp == NULL ) ) {
	    *iret = -1;
	    return;
	}
    }

    /*
     *  Draw elements that fall within the specified range, layer by 
     *  layer except those on the current layer.       
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
	cvg_rfrshLayer ( fp, newfil, ii, (int)maxbytes, 
	                     dllx, dlly, durx, dury, &ier );
		 
    }


    /*
     *  Draw elements that fall within the specified range and on 
     *  the current layer, always in full color & fill mode.       
     */
    icol = 0;
    ifill = G_TRUE;
    cds_scol ( icol, iret );
    cds_sfill ( &ifill, &ier );
    cvg_rfrshLayer ( fp, newfil, cur_layer, (int)maxbytes, 
	                     dllx, dlly, durx, dury, &ier ); 


    cfl_clos(fp, &ier);
    if ( ier != 0 ) {
	*iret = -2;
    }

    geplot(&ier);

}
