#include	"vgcmn.h"

void vsmark ( int *imark, int *imkhw, float *size, int *iwidth, int *iret )
/************************************************************************
 * vsmark								*
 *									*
 * This subroutine sets the marker attributes.				*
 *									*
 * vsmark ( imark, imkhw, size, iwidth, iret )				*
 *									*
 * Input parameters:							*
 *	*imark		int		Marker number			*
 *	*imkhw		int		Marker hardware flag		*
 *	*size		float		Marker size			*
 *	*iwidth		int		Marker line width		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	kmark  = *imark;
	rmksiz = *size;
	kmkwid = *iwidth;
	kmktyp = *imkhw;

}
