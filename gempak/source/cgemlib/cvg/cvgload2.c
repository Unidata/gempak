#include "geminc.h"
#include "gemprm.h"

void cvg_load2 ( char *fname, int icol, int *iret )
/************************************************************************
 * cvg_load2								*
 *									*
 * This function is a variant of cvg_load(). It does not build any 	*
 * range records for the loaded VG elements, nor does it order a geplot	*
 * to display the loaded VG elements.					*
 *									*
 * void cvg_load2 ( fname, icol, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *	icol		int		Plot color			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As in cvg_load()		*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 2/97	Added selection flag.			*
 * E. Wehner/EAi	 5/97	Add double tier loading 		*
 * E. Wehner/EAi	 5/97	Stop loading if > MAX_EDITABLE_ELEMS	*
 * E. Wehner/EAi	 5/97	Added refresh after displaying 		*
 * D. Keiser/GSC	 5/97	Change cvg_dsply to cds_dspvg		*
 * S. Jacobs/NCEP	 6/97	Added write flag			*
 * C. Lin/NCEP	 	 6/97	Bug fix in calling cfl_ropn 		*
 * E. Wehner/EAi	 6/97	Added check to not load header		*
 * E. Wehner/EAi	 7/97	Filled areas on all elements		*
 * E. Wehner/EAi	 8/97	Removed the graphics info record.	*
 * E. Wehner/EAi	 9/97	Allow NULL file name			*
 * F. Yen/NCEP		 1/98	Updated calls for crg library cleanup	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * F. J. Yen/NCEP	 5/98	Updated cds function names		*
 * E. Safford/GSC	10/98	added display levels revised file reads *
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/GSC	12/98	move display levels to pgprm.h		*
 * S. Jacobs/NCEP	 3/99	Added symbols to the last display level	*
 * S. Jacobs/NCEP	 9/99	Copied cvg_load and removed geplot call	*
 * A. Hardy/GSC          1/01   changed fptr from int to FILE           *
 * J. Wu/SAIC		11/01	Remove all codes & call cvg_load instead*
 * J. Wu/SAIC		12/01	add param to cvg_load calll		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    cvg_load ( fname, FALSE, FALSE, FALSE, 0, icol, iret );
}
