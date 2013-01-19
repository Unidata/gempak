#include "ensdiag.h"

void de_pfpn ( const char *argu, int ifpn[], int *nfil, int *iret )
/************************************************************************
 * de_pfpn								*
 *									*
 * This subroutine parses an ensemble diagnostic function argument and	*
 * finds all file position numbers.					*
 *									*
 * de_pfpn ( argu, ifpn, nfil, iret )					*
 *									*
 * Input parameters:							*
 *	*argu		const char	Ensemble diagnostic argument	*
 *									*
 * Input and Output parameters:						*
 *	ifpn[nfil]	int		File position number (FPN)	*
 *	*nfil		int		Number of IFPN elements already *
 *					set				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		02/05						*
 * K. Brill/HPC		02/05						*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 ************************************************************************/
{
    int nc, fil, chk, have, ival, iend, i, k, ier;
    char f[36], c, dargu[LLMXLN+1];
/*----------------------------------------------------------------------*/
    *iret  = 0;

    /*
     * Initialize variables.
     */
    fil = G_FALSE;
    chk = G_FALSE;
    nc = 0;
    ival = 0;
    f[0] = '\0';
    cst_rmbl ( (char *)argu, dargu, &iend, &ier );

    /*
     * First replace brackets with parentheses and semicolons with commas.
     */
    for ( i = 0; i < iend; i++ ) {
	c = dargu[i];
        if ( c == '[' ) {
	    dargu[i] = '(';
	} else if ( c == ']' ) {
	    dargu[i] = ')';
	} else if ( c == ';' ) {
	    dargu[i] = ',';
        }
    }

    /*
     * Loop through input string checking each character.
     */
    for ( i = 0; i < iend; i++ ) {
        c = dargu[i];

	/*        
	 * Right parentheses, ), or commas indicate end of a parm.
	 */
	if ( i == iend - 1 || c == ')' || c == ',' ) {
	    if ( fil == G_TRUE ) {
		if ( i == iend - 1 && c != ')' && c != ',' ) {
		    f[nc++] = c;
		    f[nc] = '\0';
		}
		cst_numb ( f, &ival, &ier );
	    } else if ( ival == 0 ) {
		ival = 1;
	    }
	    fil = G_FALSE;
	    nc = 0;
	    chk = G_TRUE;
	} else if ( c == '+' ) {
	    fil = G_TRUE;
	    nc = 0;
	    f[0] = '\0';
	} else if ( ( c == '%' || c == '^' || c == '@' ) && fil == G_TRUE ) {
	    cst_numb ( f, &ival, &ier );
	    f[0] = '\0';
	    fil = G_FALSE;
	} else if ( fil == G_TRUE ) {
	    f[nc++] = c;
	    f[nc] = '\0';
	}

	/*
	 * Stored any valid file position number. Note that
	 * file numbers do not repeat.
	 */
	if ( chk == G_TRUE && ( ival > 0 ) && ( ival <= NGDFLS ) ) {
	    if ( *nfil > 0 ) {
		have = G_FALSE;
		k = 0;
		while ( have == G_FALSE && k < *nfil ) {
		    if ( ifpn[k++] == ival ) have = G_TRUE;
		}
		if ( have == G_FALSE ) {
		    ifpn[(*nfil)++] = ival;
		}
	    } else {
	        ifpn[(*nfil)++] = ival;
	    }
	    ival = 0;
	    chk = G_FALSE;
        }
    }

    return;
}
