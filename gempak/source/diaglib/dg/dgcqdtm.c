#include "dg.h"

void dgc_qdtm ( const int *intry, char *fstgt, char *lstgt, int *iret )
/************************************************************************
 * dgc_qdtm								*
 *									*
 * This subroutine retrieves the first and last grid times associated	*
 * with a GDFILE entry.	 If INTRY is not a valid GDFILE entry number,	*
 * then 1 is used.							*
 *									*
 * dgc_qdtm ( intry, fstgt, lstgt, iret )				*
 *									*
 * Input parameters:							*
 *	*intry		const int	GDFILE entry number (usually 1)	*
 *									*
 * Output parameters:							*
 *	*fstgt		char		First date time			*
 *	*lstgt		char		Last date time			*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 2/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( (*intry) <= 0 || (*intry) > NGDFLS ) {
	i = 0;
    } else {
	i = (*intry) - 1;
    }

    strcpy ( fstgt, _dgfile.tfirst[i] );
    strcpy ( lstgt, _dgfile.tlast [i] );

    return;
}
