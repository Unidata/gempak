#include "dg.h"

void dg_cndg ( const char *cname, int *igrd, int *cmptd, int *iret )
/************************************************************************
 * dg_cndg								*
 *									*
 * This subroutine is used to check to see if a navigation dependent	*
 * grid has been computed or not.  If IGRD is zero, a new grid number	*
 * is assigned and CMPTD is returned as .false.  If IGRD is not zero,	*
 * then GPARMD for the internal grid whose number is IGRD is checked.	*
 * If the GPARMD entry is equal to CNAME, then the IUSESV flag for IGRD *
 * is updated and CMPTD is set to .true.  If the GPARMD entry is not	*
 * equal to CNAME, then a new grid number is assigned and CMPTD is	*
 * returned as .false.							*
 *									*
 * dg_cndg ( cname, igrd, cmptd, iret )					*
 *									*
 * Input parameters:							*
 *	*cname		const char	Name of internal grid		*
 *									*
 * Input and output parameter:						*
 *	*igrd		int		Internal grid number		*
 *									*
 * Output parameters:							*
 *	*cmptd		int		Flag for grid already computed	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. Brill/HPC		 5/02						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check to see if navigation dependent grid has been computed.
     */
    if ( *igrd == 0 ) {
	dg_nxts ( igrd, iret );
	if ( *iret != 0 ) return;
	*cmptd = G_FALSE;
    } else if ( strcmp ( _dggrid.gparmd[(*igrd)-1], cname ) != 0 ) {
	dg_nxts ( igrd, iret );
	if ( *iret != 0 ) return;
	*cmptd = G_FALSE;
    } else {
	*cmptd = G_TRUE;
    }

    if ( _dggrid.iusesv[(*igrd)-1] == 0 )
        _dggrid.iusesv[(*igrd)-1] = _dggrid.isubid;
    if ( *cmptd == G_FALSE ) strcpy ( _dggrid.gparmd[(*igrd)-1], cname );

    return;
}
