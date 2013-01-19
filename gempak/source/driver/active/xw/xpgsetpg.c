#include "xwgui.h"

void xpgsetpg ( Boolean pg_status )
/************************************************************************
 * xpgsetpg								*
 *									*
 * This subroutines sets the value of the _pgpalwIsUp flag.		*
 *									*
 * xpgsetpg ( pg_status )          					*
 *									*
 * Input parameters:							*
 *	pg_status	Boolean		status of product generation	*
 *									*
 * Output parameters:							*
 **									*
 * E. Safford/GSC	03/00	initial coding				*
 ***********************************************************************/
{

    _pgpalwIsUp = pg_status;

}
