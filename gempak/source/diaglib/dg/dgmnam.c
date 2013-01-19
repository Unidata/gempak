#include "dg.h"

void dg_mnam ( const char *func, const char *parm1, const char *parm2,
               char *dname, int *iret )
/************************************************************************
 * dg_mnam								*
 *									*
 * This subroutine uses the function name and up to two parameter names *
 * to create a grid parameter name.  The first four characters in FUNC, *
 * PARM1, and PARM2 will be concatenated to make DNAME.			*
 *									*
 * dg_mnam ( func, parm1, parm2, dname, iret )				*
 *									*
 * Input parameters:							*
 *	*func		const char	Function name			*
 *	*parm1		const char	First parameter name		*
 *	*parm2		const char	Second parameter name		*
 *									*
 * Output parameters:							*
 *	*dname		char		Output parameter name		*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/88	Allow 4 characters for func		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char f[5], p1[5], p2[5];
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check for simple functions where the parameters should not be
     * appended.
     */
    if ( strcmp ( func, "REL" ) == 0 ) {
	strcpy ( dname, "RELH" );
    } else if ( strcmp ( func, "THA" ) == 0 ) {
	strcpy ( dname, "THTA" );
    } else if ( strcmp ( func, "THE" ) == 0 ) {
	strcpy ( dname, "THTE" );
    } else if ( strcmp ( func, "MIX" ) == 0 ) {
	strcpy ( dname, "MIXR" );
    } else {
	strncpy ( f, func, 4 );
	strncpy ( p1, parm1, 4 );
	if ( parm2[0] == '\0' ) {
	    if ( strlen ( parm1 ) > 4 ) {
	        strncpy ( p2, &parm1[4], 4 );
	    } else {
	        p2[0] = '\0';
	    }
	} else {
	    strncpy ( p2, parm2, 4 );
	}
	f[4] = '\0';
	p1[4] = '\0';
	p2[4] = '\0';
	if ( strcmp ( p1, p2 ) == 0 )  p2[0] = '\0';
	strcpy ( dname, f );
	strcat ( dname, p1 );
	strcat ( dname, p2 );
    }

    /*
     * Remove blanks from name.
     */
    cst_rmbl ( dname, dname, &len, &ier );

    return;
}
