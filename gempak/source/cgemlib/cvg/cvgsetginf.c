#include "cvgcmn.h"

void cvg_setginf ( char *fname, int fpos, char grptyp, int grpnum, int *iret )
/************************************************************************
 * cvg_setginf								*
 *									*
 * This function sets the group type and number to the passed-in values	*
 * for the element located at the passed-in position in the passed-in	*
 * file name.								*
 *									*
 * cvg_setginf ( fname, fpos, grptyp, grpnum, iret )			*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *	fpos		int		Position in file 		*
 *	grptyp		char		Group type for the element	*
 *	grpnum		int		Group number for the element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -5 = VG file is empty		*
 *					-17 = error writing to VG file  *
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * C. Lin/EAI	 	10/97	read/write header instead of element 	*
 * F. J. Yen/NCEP       10/97   Corrected bug in reading header and	*
 * C. Lin/EAI	 	04/98	add crg_getinx, crg_sgrp call 		*
 * I. Durham/GSC	05/98	Changed underscore decl. to an include	*
 * J. Wu/GSC		02/01	Cleanup & used cvg_write()		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * S. Danz/AWC		07/06	Update to new cvg_write() parameter     *
 * E. Safford/SAIC	05/07	fix bug with autopl_enabled check	*
 ***********************************************************************/
{
    int 	elnum, ier, flag;
    Boolean     autopl_enabled = False;
    long	maxbytes;
    char 	newfil[256];
    VG_DBStruct	el;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;


    /*  
     *  Check if the file exists and if it is empty;
     */    
    cfl_inqr( fname, NULL, &maxbytes, newfil, &ier );
    
    if ( maxbytes == 0) {
	*iret = -5;
        return;
    }
    else {
	fp = cfl_uopn( newfil, &ier );
	if ( ( ier != 0 ) || ( fp == NULL ) ) {
	    *iret = -1;
            return;
        }
    }


    /*
     *  Read the VG header.
     */
    cvg_rdhdr( newfil, fp, fpos, (int)maxbytes, &el, &flag, &ier );


    /*
     *  Alter the group number & type if necesary.
     */
    if ( ( el.hdr.recsz > 0 ) && ( el.hdr.recsz < 65536 ) ) {
	el.hdr.grptyp = grptyp;
	el.hdr.grpnum = grpnum;

	cvg_write( &el, fpos, sizeof(el.hdr), fp, FALSE, &ier );
	if ( ier != 0 ) {
            *iret = -17;
	}
        else {

            ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
            if (autopl_enabled && cvg_metadata) {
                /*
                 * Placement only deals in entire records
                 */
                cvg_rdele ( &el, fpos, el.hdr.recsz, fp, &ier );
                cvg_el2place(fpos, &el, iret);

                /*
                 * Free TCA/GFA memory
                 */
                if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
                }
                else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr ( &el );
                }
            }

	    /*
	     * update the range record in crg library
	     */
	    crg_getinx( fpos, &elnum, iret );
	    crg_sgrp( elnum, grptyp, grpnum, iret ); 
	}	
    }


    /*
     *  Close file.
     */
    cfl_clos( fp, iret );
    if ( *iret != 0 ) *iret = -2; 
       
}
