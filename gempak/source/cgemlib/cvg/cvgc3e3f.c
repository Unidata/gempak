#include "cvgcmn.h"

void cvg_c3e3f ( char *fname, int *iret )
/************************************************************************
 * cvg_c3e3f								*
 *									*
 * This function converts a VGF file from version 5.4.3e to 5.4.3f	*
 *									*
 * cvg_c3e3f ( fname, iret )						*
 *									*
 * Input parameters:							*
 *	*fname		char		Input filename			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = Error opening input file	*
 *					 -2 = Error during conversion	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/97	Created					*
 * A. Hardy/GSC          1/01   change fptr from int to FILE            *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 	bytesin, more, ier, flag;
    char 	newfil[256];
    long	maxb;

    VG_DBStruct	el;
    FILE	*fptr;
    char        grp[4];
    int         fpos, ier1;
    int		loglev, wrtflg;
/*---------------------------------------------------------------------*/

    strcpy(grp, "CVG");
    loglev = 0;
    wrtflg = 1;
    *iret = 0;
    fpos  = 0;
    bytesin = 0;
    maxb = 0;
    more = G_TRUE;

    if (!fname)  {
	*iret = -1;
	return;
    }

    cvg_open(fname, wrtflg, &fptr, &ier);

    if ( ( ier != 0 ) || ( fptr == NULL ) )  {
	*iret = -1;
	er_lmsg( &loglev, grp, &ier, fname, &ier1, strlen(grp),
                                strlen(fname));

	return;
    }

    cfl_inqr(fname, NULL, &maxb, newfil, &ier);

    /* 
     * Checking for regular text types only.
     */
    while ( ( more ) && ( bytesin < maxb ) )  {

	/*
	 *  	Read the next VG header.
	 */
        cvg_rdhdr(fname, fptr, fpos, maxb, &el, &flag, &ier);

	/*
	 *	If the number of bytes is valid, continue.
	 */
        if ((el.hdr.recsz >0) && (el.hdr.recsz < 65536) )  {

	    if ( el.hdr.delete == 0 )  {
            
	    	if ( el.hdr.vg_type == TEXTC_ELM ||
	    	     el.hdr.vg_type == TEXT_ELM ||
		     el.hdr.vg_type == SPTX_ELM  )  {
            
		    /*
		     * Read the VG element and set the range.
		     */
		    cvg_rdele(&el, fpos, el.hdr.recsz, fptr, &ier);

		    if ( el.hdr.vg_type == TEXT_ELM )  
			el.elem.txt.info.ialign += 2;

		    if ( el.hdr.vg_type == TEXTC_ELM )
			el.elem.txt.info.ialign += 2;

/*
		    if ( el.hdr.vg_type == SPTX_ELM )
			el.elem.spt.info.ialign += 2;
*/

		    cfl_seek( fptr, fpos, 0, &ier );
		    cfl_writ( fptr, el.hdr.recsz, (unsigned char *)&el, &ier );

		    if ( ier != 0 )  {
                        *iret = -2;
		        more = G_FALSE;
		    }

		}

	    }

	}
	else
	{
	    more = G_FALSE;
        }
	fpos += el.hdr.recsz;
	if ( fpos >= maxb )  more = G_FALSE;

	bytesin = fpos;

    }

    cfl_clos( fptr, &ier);

    return;

}
