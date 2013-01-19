#include	"vgcmn.h"

void vswind ( int *iwnd, float *size, int *iwidth, int *itype, 
						float *hdsiz, int *iret )
/************************************************************************
 * vswind								*
 *									*
 * This subroutine sets the wind attributes.				*
 *									*
 * vswind ( iwnd, size, iwidth, itype, hdsiz, iret )			*
 *									*
 * Input parameters:							*
 *	*iwnd		int		Wind category			*
 *					  1 = Barbs			*
 *					  2 = Arrows			*
 *	*size		float		Wind size			*
 *	*iwidth		int		Wind line width			*
 *	*itype		int		Wind type			*
 *	*hdsiz		float		Wind arrow head size		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * I. Durham/GSC	 4/98	Added Case 3				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	switch ( *iwnd ) {

	    case 1: /* Barbs */
		rbrsiz = *size;
		kbrwid = *iwidth;
		kbrtyp = *itype;
		break;

	    case 2: /* Arrows */
	    case 3: /* Directional arrows */
		rarsiz = *size;
		karwid = *iwidth;
		kartyp = *itype;
		rarhsz = *hdsiz;
		break;

	    default:
		break;

	}

}
