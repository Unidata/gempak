#include	"vgcmn.h"

void vsline ( int *mltyp, int *mlthw, int *mlwid, int *mlwhw, int *iret )
/************************************************************************
 * vsline								*
 *									*
 * This subroutine sets the line attributes.				*
 *									*
 * vsline ( mltyp, mlthw, mlwid, mlwhw, iret )				*
 *									*
 * Input parameters:							*
 *	*mltyp		int		Line type			*
 *	*mlthw		int		Sw/hw line type flag		*
 *	*mlwid		int		Line width size multiplier	*
 *	*mlwhw		int		Sw/hw line width flag		*
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

	kltyp = *mltyp;
	klthw = *mlthw;
	klwid = *mlwid;
	klwhw = *mlwhw;

}
