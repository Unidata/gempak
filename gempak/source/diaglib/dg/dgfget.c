#include "dg.h"

void dg_fget ( const char *ckey, const int *nval, float *fval, int *iret )
/************************************************************************
 * dg_fget                                                              *
 *                                                                      *
 * This subroutine queries, based on the key provided, the value(s) of 	*
 * floating point type global variable.					*
 *                                                                      *
 * dg_fget ( ckey, nval, fval, iret )                                   *
 *                                                                      *
 * Input parameter:							*
 *	*ckey		const char	Variable key			*
 *	*nval		const int	Number of values queried	*
 * Output parameters:                                                   *
 *	*fval		float		Values				*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/05						*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( strcmp ( ckey, "GDDX" ) == 0 ) {
	*fval = _mapscl.gddx;
    } else if ( strcmp ( ckey, "GDDY" ) == 0 ) {
	*fval = _mapscl.gddy;
    } else if ( strcmp ( ckey, "ORGXPT" ) == 0 ) {
	*fval = _dgorig.orgxpt;
    } else if ( strcmp ( ckey, "ORGYPT" ) == 0 ) {
	*fval = _dgorig.orgypt;
    } else if ( strcmp ( ckey, "ORNANG" ) == 0 ) {
	*fval = _dgovec.ornang;
    } else if ( strcmp ( ckey, "SNAV" ) == 0 ) {
	for ( i = 0; i < *nval; i++ ) {
	    fval[i] = _dgfile.snav[i];
	}
    }

    return;
}
