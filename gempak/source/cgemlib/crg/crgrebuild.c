#include "crgcmn.h"
#include "vgstruct.h"


void crg_rebuild ( void )
/************************************************************************
 * crg_rebuild                                                          *
 *                                                                      *
 * This function rebuilds range records for all elem. in WORK_FILE.	*
 *                                                                      *
 * Note: the layer assignment is not affected by the rebuild - the 	*
 *       existing layer values are kept unchanged in range records. 	*
 *	 They could only be changed by crg_set() or crg_setLayer().	*
 *									*
 * void crg_rebuild ( void ) 						*
 *                                                                      *
 * Input/output parameters:                                             *
 *      none                               				*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		12/01	initial coding				*
 * J. Wu/SAIC		12/01	replace crg_rdrec with crg_rdhdr/ele    *
 * J. Wu/SAIC		12/01	Get VG file size with cfl_inqr()  	*
 * T. Lee/SAIC		11/03	added user directory to work_file	*
 * T. Lee/SAIC		11/03	used cvg_getworkfile()			*
 * B. Yin/SAIC          07/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block pointers			*
 * J. Wu/SAIC		07/07	skip records for GFA text boxes		*
 ***********************************************************************/
{
    int 	ii, elnum, joffset, flag, ier;
    long	maxbytes;
    char	newfil[FILE_FULLSZ];
    VG_DBStruct	el;
    FILE	*fp;
    
/*---------------------------------------------------------------------*/

    cfl_inqr( cvg_getworkfile(), NULL, &maxbytes, newfil, &ier);

    cvg_open( cvg_getworkfile(), TRUE, &fp, &ier );

    if ( ( ier != 0 ) || ( fp == NULL ) ) {
	return;
    }
    
    
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++)
    {
        joffset = range[ ii ].ioffset;
	if ( joffset >= 0 && !( crg_isauxrec( ii, &ier ) ) ) {            
	    crg_getinx( joffset, &elnum, &ier);
            
            cvg_rdhdr( cvg_getworkfile(), fp, joffset, (int)maxbytes, 
			&el, &flag, &ier );
	    cvg_rdele( &el, joffset, el.hdr.recsz, fp, &ier );
	    	    
	    if ( ier == 0 ) {
	        crg_mkRange( &el, joffset, elnum, &ier );
	    }

            /*
             * Free TCA break point/GFA blocks memory
             */
            if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
            else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
	    }
	}
    }

    cvg_clos( fp, &ier );

}
