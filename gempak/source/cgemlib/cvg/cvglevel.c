#include "cvgcmn.h"
#include "drwids.h"
#include "pgprm.h"


void cvg_level ( VG_HdrStruct *hdr, int *level, int *iret )
/************************************************************************
 * cvg_level								*
 *									*
 * This function locates a VG element's displaying level. 		*
 *									*
 * Note: The displaying level refers to the order in which Vg elements	*
 *       are displayed ( i.e.,filled elements first, then lines, etc.)	*
 *       This is different from production layers.			*
 *									*
 * void cvg_level ( hdr, level, iret )					*
 *									*
 * Input parameters:							*
 *	*hdr		VG_HdrStruct	Pointer to a VG elem. header	*
 *									*
 * Output parameters:							*
 *	*level		int		Displaying level of VG elem.	*
 *	*iret		int		Return code			*
 *					 0 - normal			*
 *					-4 - unkown VG type/class	*
 *				       -24 - No VG header loaded	*
 *				       -48 - Element deleted already	*
 **									*
 * Log:									*
 * J. Wu/SAIC           12/01   initial coding				*
 * J. Wu/SAIC           09/02   expand class/type to 14/34 for LIST	*
 * D.W.Plummer/NCEP	06/03	rm hard-wired value for MAX_RECTYPES	*
 * J. Wu/SAIC           09/03   expand class to 15 for CLASS_MET-JET_ELM*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    *iret = G_NORMAL;
    *level = -1;

    if ( hdr == NULL )  {
        *iret = -24;
    }
    else if ( hdr->delete )  {
        *iret = -48;
    }
    else if ( hdr->vg_class < 1 ||  hdr->vg_class > 15 || 
              hdr->vg_type < 1 ||  hdr->vg_type > MAX_RECTYPES  ) {    
        *iret = -4;
    }
    else {
	if ( hdr->filled ) {
	    *level = LEVEL_0;
	}
	else if ( hdr->vg_type  != FILEHEAD_ELM &&
	    	  hdr->vg_class != CLASS_TEXT &&
		  hdr->vg_class != CLASS_WINDS &&
		  hdr->vg_class != CLASS_SYMBOLS ) {
	    *level = LEVEL_1;
	} 
	else {
	    *level = LEVEL_2;	
	}    
    }    
}
