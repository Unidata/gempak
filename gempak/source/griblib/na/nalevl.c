#include "na.h"

void na_levl ( const int *jvcord, const int *jlevel1, const int *jlevel2,
               int *ivcord, int *level1, int *level2, int *iret )
/************************************************************************
 * na_levl								*
 *									*
 * This routine will convert the GRIB vertical coordinate and level	*
 * definitions to vertical coordinate and level values for GEMPAK.	*
 *									*
 * na_levl ( jvcord, jlevel1, jlevel2, ivcord, level1, level2, iret )	*
 *									*
 * Input parameters:							*
 *	*jvcord		const int	Vertical coordinate		*
 *	*jlevel1	const int	Level				*
 *	*jlevel2	const int	Level				*
 *									*
 * Output parameters:							*
 *	*ivcord		int		GEMPAK vertical coordinate	*
 *	*level1		int		GEMPAK level info		*
 *	*level2		int		GEMPAK level info		*
 *	*iret		int		Return code			*
 *					   +2 = No valid vert coord	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * S. Jacobs/EAI	12/93		Copied from NAGLVL		*
 *					   Eliminate reading table	*
 * T. Piper/GSC		11/98		Updated prolog			*
 * R. Tian/SAIC		 7/06		Recoded from Fortran		*
 ************************************************************************/
{
    char vparm[5];
    int ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Set the vertical coordinate.
     *
     * If there is no valid value, return.
     */
    if ( _navcrd.mvcord[(*jvcord)-1][0] == '\0' ) {
	*iret = +2;
	return;
    }
    clv_cord ( _navcrd.mvcord[(*jvcord)-1], vparm, ivcord, &ier );

    /*
     * Set the levels.
     */
    *level1 = (int)( *jlevel1 * pow(10, _navcrd.mvscal[(*jvcord)-1]) );
    if ( *jlevel2 == -1 ) {
	*level2 = *jlevel2;
    } else {
	*level2 = (int)( *jlevel2 * pow(10, _navcrd.mvscal[(*jvcord)-1]) );
    }

    return;
}
