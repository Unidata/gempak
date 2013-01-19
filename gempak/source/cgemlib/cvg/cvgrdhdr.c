#include "cvgcmn.h"

void cvg_rdhdr ( char *fname, FILE *fp, int start, int size, 
				VG_DBStruct *el, int *flag, int *iret )
/************************************************************************
 * cvg_rdhdr								*
 *									*
 * This function reads the header of an element from a vector graphics  *
 * file that is composed of elements that match the structure of a 	*
 * VG_DBStruct element.							*
 *									*
 * cvg_rdhdr ( fname, fp, start, size, el, flag, iret )			*
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name			*
 *	*fp		FILE		Pointer to open VG file		*
 *	start		int		Offset start of element to read	*
 *	size		int		Size of the VG file in bytes	*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *	*flag		int		End of file reached		*
 *	*iret		int		Return code			*
 *					 -3 = seek to a bad location	*
 *					 -8 = no VG file is open	*
 *					-25 = invalid VG header read	*
 *					-26 = no VG header found	*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 6/97	Allow empty colors			*
 * I. Durham/GSc	 5/98	Changed underscore decl. to an include	*
 * M. Li/GSC		12/99	Added cvg_swap				*
 * A. Hardy/GSC          1/01   Changed fp to equal NULL		*
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * T. Piper/GSC		 3/01	Fixed IRIX6 compiler warnings		*
 * A. Hardy/NCEP         7/02   Modified ier == 4 error checking	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		level, nbin, ier, ier1, ierr;
    char	grp[4];
/*---------------------------------------------------------------------*/
    *iret = 0;
    strcpy(grp, "CFL");
    level = 4;
    *flag = G_FALSE;

/*
 *  Seek to the proper position and read the header. 
 */
    if ( fp == NULL )
	*iret = -8;
    else {
	cfl_seek(fp, (long)start, 0, &ier);

	if ( ier == 0 ) {
	    cfl_read(fp, sizeof(el->hdr), (void *)el, &nbin, &ier);

	    if ( ier == 4 ) {
		er_lmsg ( &level, grp, &ier, fname, &ier1, strlen(grp),
				strlen(fname) );
		*flag = G_TRUE;
		*iret = -26;
		return;
	    }
            if ( MTMACH == MTULTX ||
	         MTMACH == MTALPH ||
		 MTMACH == MTLNUX ) {	        
		cvg_swap( SWPHDR, G_TRUE, *el, el, &ierr );
	    }

	    if ( el->hdr.recsz == 0 ) {
		*iret = -26;
	    }
	    else if ( ( (int)el->hdr.delete < 0 || (int)el->hdr.delete > 1 ) ||
	              ( (int)el->hdr.vg_type < 0 || 
					el->hdr.vg_type > MAX_RECTYPES ) ||
		      ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 ) ||
		      ( el->hdr.min_col < 0 || el->hdr.min_col > 32 ) ||
		      ( el->hdr.recsz < 0 || el->hdr.recsz > size ) ) {
		*iret = -25;
		el->hdr.delete = 0;
		el->hdr.vg_type = 0;
		el->hdr.maj_col = 0;
		el->hdr.min_col = 0;
		el->hdr.recsz = 0;
	    }
 	}
	else {
	    *iret = -3;
        }
    }
}

