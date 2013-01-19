#include "xwcmn.h"

void xg2pxm ( int *ipxm, int *iret )
/************************************************************************
 * xg2pxm								*
 *									*
 * This subroutine set the current pixmap pointer to the specified      *
 * pixmap index 'ipxm'.   						*
 *									*
 * This routine should be replaced with xpxm2win as time and money	*
 * allows.								*
 *									*
 * xg2pxm( ipxm, iret )							*
 *									*
 * Input parameters:							*
 *	*ipxm		int		index to the pixmap		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 G_NORMAL = Normal return	*
 *					 -1 = invalid pixmap index	*
 **									*
 * Log:									*
 * C. Lin/EAI	         5/96						*
 * E. Wehner/EAi	 1/97 	Add graphics info record		*
 * S. Jacobs/NCEP        6/97   Added write flag of TRUE to cvg_load    *
 * C. Lin/EAI	         6/97   Use new copy area info   		*
 * E. Wehner/Eai	 9/97 	Remove grInfo for call to cvgload	*
 * E. Wehner/Eai	 9/97   Remove grinfo record			*
 * E. Safford/GSC	12/98	new refresh strategy			*
 * E. Safford/GSC	12/98	fix error in condition on cvg_load call	*
 * E. Safford/GSC	03/99	add check on xw_refresh for cvg_load    *
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h                  *
 * E. Safford/GSC	01/00	correct reference to refresh flag	*
 * S. Law/GSC		01/00	replaced with call to xpxm2win		*
 ***********************************************************************/
{
    xpxm2win (*ipxm, current_window, iret);
}
