#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"


void cvg_drawLayer ( FILE *fp, char *fname, int layer, int fsize, int *iret )
/************************************************************************
 * cvg_drawLayer							*
 *									*
 * This function draws all records on a given layer. The elements are	*
 * displayed in sequence of their displaying level. By default, records	*
 * on the given layer of WORK_FILE will be drawn.			*
 *									*
 * Note: The displaying level refers to the order in which VG elements	*
 *       are displayed ( i.e.,filled elements first, then lines, etc.)	*
 *       This is different from production layers.			*
 *									*
 * cvg_drawLayer ( fp, fname, layer, fsize, iret )			*
 *									*
 * Input parameters:							*
 *	*fp		FILE		Pointer to VG "fname"		*
 *	*fname		char		VG File to be read 		*
 *	 layer		int		layer to to be drawn		*
 *	 fsize		int		size of the VG file in bytes	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  1 = Num. of elem. over limit	*
 *					 -8 = No VG file is open	*
 **									*
 * Log:									*
 * J. Wu/SAIC           02/02   modified from cvg_redraw()		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC           07/04   add display filter			*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block pointers			*
 * J. Wu/SAIC		06/06	call new cvg_matchFilter 		*
 * M. Li/SAIC		03/07	Updated cvg_matchfilter			*
 ***********************************************************************/
{
    int 		ii, kk, ier, flag, level, location, el_layer;
    int			elms[3][MAX_EDITABLE_ELEMS], lvl_cnt[LEVELS];
    VG_DBStruct		el;
    filter_t		el_filter, timeMatched;
    Boolean		filter_match, matchAny = False;
/*---------------------------------------------------------------------*/

    *iret = 0;

    for ( ii = 0; ii < 3; ii++ ) {
        lvl_cnt[ ii ] = 0;
    }

    if ( fp == NULL ) {
        *iret = -1;
	return;
    }

    /*
     *  Scan the range records and locate elements on the given layer
     *  with matching filters.       
     */        
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

	crg_goffset( ii, &location, &ier );
	
	el_layer = crg_getLayer ( location );
	crg_getfilter ( ii, el_filter, &ier );
		
	if ( location >= 0 && el_layer == layer ) {
	     
             cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );
	     
	     if ( filter_match ) {
	         cvg_rdhdr( fname, fp, location, fsize, &el, &flag, &ier );

                 cvg_level( &el.hdr, &level, &ier );
	         if ( ier == 0 ) {	
	             elms[ level ] [ lvl_cnt[level]++ ] = location;
                 }
	     }
         }
    }


    /*
     *  Now loop thru the levels and display elements on the given layer.
     */
    level = 0;    
    for ( ii = 0; ii < LEVELS; ii++ ) {

	for ( kk = 0; kk < lvl_cnt[level]; kk++ ) {
	    location = elms[ii][kk];
	    cvg_rdhdr( fname, fp, location, fsize, &el, &flag, &ier );
	    cvg_rdele( &el, location, el.hdr.recsz, fp, &ier );
    	    cds_dspelm( &el, &ier );

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

	level++;
    }

}
