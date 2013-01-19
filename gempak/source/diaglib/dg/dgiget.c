#include "dg.h"

void dg_iget ( const char *ckey, const int *nval, int *ival, int *iret )
/************************************************************************
 * dg_iget                                                              *
 *                                                                      *
 * This subroutine queries, based on the key provided, the values of 	*
 * integer type global variables.					*
 *                                                                      *
 * dg_iget ( ckey, nval, ival, iret )                                   *
 *                                                                      *
 * Input parameter:							*
 *	*ckey		const char	Variable key			*
 *	*nval		const int	Number of values queried	*
 * Output parameters:                                                   *
 *	*ival		int		Values				*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/05						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( strcmp ( ckey, "KXYD" ) == 0 ) {
	*ival = _dgfile.kxyd;
    } else if ( strcmp ( ckey, "KSUB1" ) == 0 ) {
	*ival = _dgarea.ksub1;
    } else if ( strcmp ( ckey, "KSUB2" ) == 0 ) {
	*ival = _dgarea.ksub2;
    } else if ( strcmp ( ckey, "IDGLAT" ) == 0 ) {
	*ival = _dgfile.idglat;
    } else if ( strcmp ( ckey, "IDGLON" ) == 0 ) {
	*ival = _dgfile.idglon;
    } else if ( strcmp ( ckey, "IXMSCL" ) == 0 ) {
	*ival = _mapscl.ixmscl;
    } else if ( strcmp ( ckey, "IYMSCL" ) == 0 ) {
	*ival = _mapscl.iymscl;
    } else if ( strcmp ( ckey, "IXMSDY" ) == 0 ) {
        *ival = _mapscl.ixmsdy;
    } else if ( strcmp ( ckey, "IYMSDX" ) == 0 ) {
    	*ival = _mapscl.iymsdx;
    } else if ( strcmp ( ckey, "JGYMIN" ) == 0 ) {
	*ival = _dgarea.jgymin;
    } else if ( strcmp ( ckey, "JGYMAX" ) == 0 ) {
	*ival = _dgarea.jgymax;
    } else if ( strcmp ( ckey, "JGXMIN" ) == 0 ) {
	*ival = _dgarea.jgxmin;
    } else if ( strcmp ( ckey, "JGXMAX" ) == 0 ) {
	*ival = _dgarea.jgxmax;
    } else if ( strcmp ( ckey, "KGYMIN" ) == 0 ) {
	*ival = _dgarea.kgymin;
    } else if ( strcmp ( ckey, "KGYMAX" ) == 0 ) {
	*ival = _dgarea.kgymax;
    } else if ( strcmp ( ckey, "KGXMIN" ) == 0 ) {
	*ival = _dgarea.kgxmin;
    } else if ( strcmp ( ckey, "KGXMAX" ) == 0 ) {
	*ival = _dgarea.kgxmax;
    } else if ( strcmp ( ckey, "KXD" ) == 0 ) {
	*ival = _dgfile.kxd;
    } else if ( strcmp ( ckey, "KYD" ) == 0 ) {
	*ival = _dgfile.kyd;
    } else if ( strcmp ( ckey, "IDLUN" ) == 0 ) {
	*ival = _dgfile.idlun;
    } else if ( strcmp ( ckey, "DGSUBG" ) == 0 ) {
	*ival = _dgsubg.dgsubg;
    } else if ( strcmp ( ckey, "LDLEVL1" ) == 0 ) {
        *ival = _dginpt.ldlevl1;
    } else if ( strcmp ( ckey, "LDLEVL2" ) == 0 ) {
        *ival = _dginpt.ldlevl2;
    } else if ( strcmp ( ckey, "LVCORD" ) == 0 ) {
        *ival = _dginpt.lvcord;
    }

    return;
}
