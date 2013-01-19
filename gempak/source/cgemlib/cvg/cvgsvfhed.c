#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

void cvg_svfhed ( char *fname, int *iret )
/************************************************************************
 * cvg_svfhed								*
 *									*
 * This function creates a VG record for a file header.			*
 *									*
 * cvg_svfhed ( fname, iret )						*
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					-17 = error writing VG record	*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * C. Lin et al	 	 7/97	Initialize header data structure	*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	 9/97	Change file name for default vgf	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		10/98	Prolog update				*
 * J. Wu/GSC		02/01	Cleanup & used cvg_write()		*
 * J. Wu/GSC	        02/01	Modified 'unused1' & 'unused2' in VG 	*
 *			        struture to 'smooth' & 'version' 	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * S. Danz/AWC		07/06	Switch to new cvg_writeD() function     *
 ***********************************************************************/
{
    int			ier, one = 1, start;
    char		outfile[256], version[128];
    FILE		*ofp;
    VG_DBStruct		*el_ptr;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = 0;   
    
    if ( fname == NULL ) {
	strcpy( outfile, work_file );
    }
    else {
	strcpy( outfile, fname );
    }


    /* 
     * Get the version to place in the file.
     */
    ss_vers( version, iret, sizeof(version) );

    G_CALLOC(el_ptr, VG_DBStruct, one, "cvg_svfhed:  VG_DBStruct");
    /* 
     *  Get the size of the record.
     */
    el_ptr->hdr.vg_type = FILEHEAD_ELM;
    el_ptr->hdr.recsz = ( sizeof(FileHeadType) +     
                     sizeof(VG_HdrStruct) );
    strcpy(el_ptr->elem.fhed.version, version);
    strcpy(el_ptr->elem.fhed.notes, VGFHEAD_COMMENT);

    /* 
     *  Write element in slot. 
     */

    ofp = cfl_uopn( fname, &ier );
    if ( ( ier != 0 ) || ( ofp == NULL ) ) {
	*iret = -1;
    }
    else {
	cvg_writeD ( el_ptr, start, el_ptr->hdr.recsz, ofp, &ier );
	if ( ier != 0 )  *iret = -17;
	cfl_clos( ofp, &ier );
	if ( ier != 0 )  *iret = -2;
    }
    G_FREE(el_ptr, VG_DBStruct);
}
