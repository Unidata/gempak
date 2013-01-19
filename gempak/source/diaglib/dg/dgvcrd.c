#include "dg.h"

void dg_vcrd ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *np, int *iret )
/************************************************************************
 * dg_vcrd								*
 *									*
 * This subroutine checks to see if the parameter is a vertical		*
 * coordinate.  If it is, level (1) is returned.  If not, the file is	*
 * searched for the parameter.  The grid will be returned in the grid	*
 * location pointed to by NP.  If NP = 0 on input, the next grid	*
 * location will be used and returned.  If NP > 0, the grid will be 	*
 * found at NP.								*
 *									*
 * dg_vcrd ( time1, time2, level1, level2, ivcord, parm, np, iret )	*
 *									*
 * Input parameters:							*
 * 	*time1		const char	Date/time			*
 * 	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *									*
 * Input and output parameters:						*
 *	*np		int		Location of grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 **									*
 * Log:									*
 * M. desJardins/NMC	 3/92	General coordinate check		*
 * K. Brill/NMC		 4/92	Initialize iret to -7			*
 * M. desJardins/NMC	 7/93	Fixed ifiled				*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char ppp[5];
    float value;
    int ivcnew, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Translate parameter into level number.
     */
    clv_cord ( parm, ppp, &ivcnew, &ier );

    /*
     * Check for level to be pressure.
     */
    if ( ( *ivcord == ivcnew ) && ( *level2 == -1 ) ) {
	/*
	 * Eliminate pressure at surface.
	 */
	if ( ( *ivcord != 1 ) || ( *level1 != 0 ) ) {
	    value = (float)( *level1 );
	    dg_real ( &value, np, iret );
	    dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
	        parm, np, iret);
	}
    } else {
        dg_grdr ( time1, time2, level1, level2, ivcord, parm, np, iret );
    }

    return;
}
