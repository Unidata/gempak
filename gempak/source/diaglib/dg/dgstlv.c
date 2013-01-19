#include "dg.h"

void dg_stlv ( const char *gdattm, const char *glevel, const char *gvcord,
               const char *caller, char *gfunc, int *iret )
/************************************************************************
 * dg_stlv								*
 *									*
 * This subroutine takes the user input for date/time, level and 	*
 * vertical coordinate and saves it in the DG common area.		*
 *									*
 * dg_stlv ( gdattm, glevel, gvcord, caller, gfunc, iret )		*
 *									*
 * Input parameters:							*
 *	*gdattm		const char	Input date/time			*
 *	*glevel		const char	Input level			*
 *	*gvcord		const char	Input vertical coordinate	*
 *	*caller		const char	Caller--GFUNC or GVECT		*
 *									*
 * Input and output parameters:						*
 *	*gfunc		char		Input grid function		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  3 = user typed EXIT		*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/88	Eliminated call to GR_FIND		*
 * M. desJardins/GSFC	 4/89	Added first time			*
 * S. Schotz/GSC	 6/90	Removed respnd flag			*
 * S. Jacobs/EAI	 3/93	Changed call to GR_LIST			*
 * K. Brill/NMC		05/93	Don't call ST_LCUC for GFUNC		*
 * S. Maxwell/GSC	10/96	Put ST_LCUC back in; changes to GR_LIST	*
 * S. Jacobs/NCEP	 4/97	Renamed IP_ULOC to IP_PUTV		*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * D.W.Plummer/NCEP	 4/00	Changed call to GR_LIST			*
 * R. Tian/SAIC		 3/05	Removed call to GR_LIST			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gvc[5];
    int ier;
/*----------------------------------------------------------------------*/
    *iret  =  0;

    /*
     * Get the times.
     */
    strcpy ( _dginpt.ingdtm, gdattm );

    /*
     * Get the levels.
     */
    strcpy ( _dginpt.inglev, glevel );
    grc_levl ( glevel, &_dginpt.ldlevl1, &_dginpt.ldlevl2, &ier );

    /*
     * Get the vertical coordinate.
     */
    strcpy ( _dginpt.invcrd, gvcord );
    clv_cord ( gvcord, gvc, &_dginpt.lvcord, &ier );

    return;
}
