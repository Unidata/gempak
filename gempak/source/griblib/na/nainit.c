#define NA_GLOBAL
#include "na.h"
#undef  NA_GLOBAL

void na_init ( int *iret )
/************************************************************************
 * na_init								*
 *									*
 * This routine will initialize the contents of the common block	*
 * variables.								*
 *									*
 * na_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	12/93						*
 * R. Tian/SAIC		 7/06		Recoded from Fortran		*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    _nacent.meditn  = IMISSD;
    _naparm.mcodtbl = IMISSD;
    _naparm.mcenter = IMISSD;

    for ( i = 0; i < MAXNUM; i++ ) {
	_nacent.mcnam[i][0] = '\0';
	_nacent.mcabb[i][0] = '\0';

	_navcrd.mvcnam[i][0] = '\0';
	_navcrd.mvunit[i][0] = '\0';
	_navcrd.mvcord[i][0] = '\0';
	_navcrd.mvscal[i] = IMISSD;

	_naparm.mprnam[i][0] = '\0';
	_naparm.mpunit[i][0] = '\0';
	_naparm.mparms[i][0] = '\0';
	_naparm.mpscal[i] = IMISSD;
	_naparm.rmssvl[i] = RMISSD;
    }

    return;
}
