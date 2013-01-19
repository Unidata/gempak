#include "cvgcmn.h"

void cvg_rdrec ( char *fname, int fpos, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_rdrec								*
 *									*
 * This function reads a VG record from a VG file based upon the file	*
 * position.								*
 *									*
 * cvg_rdrec ( fname, fpos, el, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name			*
 *	fpos		int		File position to read from 	*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	11/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 8/97	Remove unneeded include 		*
 * E. Wehner/EAi	 9/97	Handle null file name			*
 * F. J. Yen/NCEP       11/97   Replace " " with NULL for       	*
 *                              default directory.              	*
 * G. Krueger/EAI	 1/98	Ignore non-fatal read warnings.		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * E. Safford/GSC	10/98	removed end from param list		*
 * J. Wu/SAIC		04/02	call cvg_rdrecnoc to read element	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		ier;
    FILE 	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( !fname ) {
	*iret = -47;
	return;
    }


    /*
     *  Open the file for READ only.
     */

    fp = (FILE *) cfl_ropn(fname, "", &ier);
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
	*iret = -1;
        return;
    }


    /*
     *  Read the element.
     */
    cvg_rdrecnoc ( fname, fp, fpos, el, iret );


    /*
     *  Close the VG file.
     */
    cfl_clos ( fp, &ier );
    if ( ier != 0 )  *iret = -2;

}

