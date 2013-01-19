#include	"vgcmn.h"

void vssymb ( int *isym, int *itype, float *size, int *iwidth, int *iret )
/************************************************************************
 * vssymb								*
 *									*
 * This subroutine sets the symbol attributes.				*
 *									*
 * vssymb ( isym, itype, size, iwidth, iret )				*
 *									*
 * Input parameters:							*
 *	*isym		int		Symbol category			*
 *					  1 = Weather symbols		*
 *					  2 = Cloud type symbols	*
 *					  3 = Icing symbols		*
 *					  4 = Pressure tendency symbols	*
 *					  5 = Past weather symbols	*
 *					  6 = Sky cover symbols		*
 *					  7 = Special symbols		*
 *					  8 = Turbulence symbols	*
 *					  9 = Combination wx symbols    *
 *	*itype		int		Symbol type (specific per sym )	*
 *	*size		float		Symbol size			*
 *	*iwidth		int		Symbol line width		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * A. Hardy/GSC         10/98   Added combination symbols case          *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	switch ( *isym ) {

	    case 1: /* Weather */
		rwtsiz = *size;
		kwtwid = *iwidth;
		break;

	    case 2: /* Cloud type */
		rctsiz = *size;
		kctwid = *iwidth;
		break;

	    case 3: /* Icing */
		ricsiz = *size;
		kicwid = *iwidth;
		break;

	    case 4: /* Pressure tendency */
		rptsiz = *size;
		kptwid = *iwidth;
		break;

	    case 5: /* Past weather */
		rpwsiz = *size;
		kpwwid = *iwidth;
		break;

	    case 6: /* Sky cover */
		rsksiz = *size;
		kskwid = *iwidth;
		ksktyp = *itype;
		break;

	    case 7: /* Special */
		rspsiz = *size;
		kspwid = *iwidth;
		break;

	    case 8: /* Turbulence */
		rtbsiz = *size;
		ktbwid = *iwidth;
		break;

	    case 9: /* Combination symbols */
		rcsysz = *size;
		kcsywd = *iwidth;
		break;

	    default:
		break;

	}

}
