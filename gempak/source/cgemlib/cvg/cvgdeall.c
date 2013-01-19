#include "cvgcmn.h"

void cvg_deall ( char *fname, int layer, Boolean inc_place, int *iret )
/************************************************************************
 * cvg_deall								*
 *									*
 * This function marks records on a given layer as deleted.		*
 *									*
 * cvg_deall ( fname, layer, inc_place, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		VG filename			*
 *	layer		int		layer to be operated on		*
 *	inc_place	Boolean		flag to include VG in placement *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -3 = seek to a bad location	*
 *					-13 = unable to read header	*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/97						*
 * E. Wehner/Eai	 6/97	Don't delete the file header 		*
 * E. Wehner/EAi	 9/97	Handle null file name			*
 * F. J. Yen/NCEP	11/97	Replace " " with NULL for		*
 *				default directory.			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * E. Safford/GSC	10/98	clean up and use cvg_qkopen 		*
 * M. Li/GSC             1/00   Cleaned up and used cvg_delet           *
 * E. Safford/GSC	03/00	remove cvg_delet call, add cvg_swap 	*
 * A. Hardy/GSC		01/01   changed fp from int->FILE in cvg_qkopen *
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * J. Wu/SAIC	 	01/02	change to layer-specific deletion	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    VG_DBStruct		el;
    int			ier, ier1, level, offset, size, flag;
    int			rec_size, el_layer;
    char		grp[4];
    FILE		*fp;
    Boolean     	autopl_enabled;
/*---------------------------------------------------------------------*/

    *iret = 0;
    strcpy(grp, "CVG");
    offset = 0;
    level = 2;

    if (!fname) {
	*iret = -47;
	return;
    }

    /*
     * If the caller is asking to include placement, see if its even on
     * if not, just clear the flag
     */
    if (inc_place) {
        ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
        if (!autopl_enabled || !cvg_metadata) {
            inc_place = FALSE;
        }
    }

    /*
     *  open the VG file. 
     */
    cvg_qkopen (fname, &fp, &size, &ier);

    if ( ( ier != 0 ) || ( fp == NULL ) ) {
	*iret = ier1 = -1;
        er_lmsg ( &level, grp, &ier1, fname, &ier, strlen(grp),
                        strlen(fname) );
	return;
    }


    /*
     *  Read headers and mark each as deleted till EOF. 
     */ 
    while ( offset < size ) {
	el.hdr.recsz = 0;

        /*
         *	Read the VG header.
         */
	cvg_rdhdr(fname, fp, offset, size, &el, &flag, &ier);
	if ( ( ier < 0 ) && ( ier != -26 ) ) {
	    *iret = -13;
	    er_lmsg ( &level, grp, &ier, fname, &ier1, strlen(grp),
			strlen(fname) );
            cvg_clos(fp, &ier);
	    return;
	}
	rec_size = el.hdr.recsz;

	el_layer = crg_getLayer ( offset );
	if ( (el_layer == layer) && (el.hdr.vg_type != FILEHEAD_ELM) ) {

	    /*
	     *	If element is not deleted as deleted and move to next header 
	     *  position.
	     */
  	    if ( !el.hdr.delete ) { 
	        el.hdr.delete = G_TRUE;

                /*
                 * If placement is on, then send the object to placement 
                 */
                if (inc_place) {
                    cvg_el2place(offset, &el, iret);
                }

	        cfl_seek(fp, (long)offset, 0, &ier);

                if ( MTMACH == MTULTX ||
		     MTMACH == MTALPH ||
		     MTMACH == MTLNUX ) {
	            cvg_swap( SWPHDR, G_FALSE, el, &el, &ier1 );
	        }

	        if ( ier == 0 ) {
	            cfl_writ(fp, sizeof(el.hdr), (unsigned char *)&el, &ier);
		}
	        else {
	            *iret = -3;
		}
  	    } 
	}
	offset += rec_size;

    }

    cvg_clos(fp, &ier);
    if ( ier != 0 )
	*iret = -2;

}
