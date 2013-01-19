#include "cvgcmn.h"

void cvg_undel ( char *fname, int offset, Boolean inc_place, int *iret )
/************************************************************************
 * cvg_undel								*
 *									*
 * This function revives a record in a VG file.		                *
 *									*
 * cvg_undel ( fname, offset, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		VG File name 	                *
 *	offset		int		Byte offset in file where to	*
 *					write				*
 *	inc_place	Boolean		send changes to auto placement  *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					-13 = error reading VG header	*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/00   Copied from cvgdelet.c                  *
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int			ier, level, ier1, flag;
    long		size;
    char		newfil[133], grp[4];
    VG_DBStruct		el;
    FILE		*fp;
    Boolean     	autopl_enabled;
/*---------------------------------------------------------------------*/
    *iret = 0;
    level = 2;
    strcpy(grp, "CVG");

    if (!fname)
    {
	*iret = -1;
	return;
    }
/*
 *  Inquire the size of the VG file and open.
 */
    cfl_inqr(fname, NULL, &size, newfil, &ier);
    fp = (FILE *) cfl_uopn( fname, &ier );
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
	*iret = -1;
	return;
    }

/*
 *  Read the VG header from the file, clear deletion flag, and write the
 *  revised header to the file. If we seeked to a bad location, skip the
 *  write and close the file.
 */ 

    cvg_rdhdr ( fname, fp, offset, (int)size, &el, &flag, &ier );
    if ( ier == 0 ) {
	el.hdr.delete = 0;   /* clear deletion flag */

        /*
         * If placement is on, and we have initialized, then send
         * the object to placement if the caller requested it
         */
        if (inc_place) {
            ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
            if (autopl_enabled) {
                if (cvg_metadata) {
                    /*
                     * Get the rest of the information for the object
                     */
	            cvg_rdele(&el, offset, el.hdr.recsz, fp, &ier);

                    /*
                     * Send it along to placement
                     */
                    cvg_el2place(offset, &el, iret);
                }
            }
        }

	cfl_seek( fp, (long)offset, 0, &ier );

        if ( MTMACH == MTULTX ||
	     MTMACH == MTALPH ||
	     MTMACH == MTLNUX ) {	        
	    cvg_swap( SWPHDR, G_FALSE, el, &el, &ier1 );
	}
	cfl_writ( fp, sizeof(el.hdr), (unsigned char *)&el, &ier );
    }
    else {
	er_lmsg ( &level, grp, &ier, fname, &ier1, strlen(grp),
			strlen(fname) );
	*iret = -13;
    }

    cfl_clos( fp, &ier );
    if ( ier != 0 )
	*iret = -2;

}
