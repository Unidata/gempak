#include "dg.h"

void dg_pfun ( const char *func, int *iret )
/************************************************************************
 * dg_pfun								*
 *									*
 * This subroutine parses a diagnostic function entered by the user.	*
 * The output is returned in the diagnostic function tables.  In-line	*
 * parameters may be appended to an operand in any combination.  The	*
 * flags are:								*
 *									*
 *     Parameter:   time   level   ivcord				*
 *     Flag:         ^       @       %					*
 *									*
 * dg_pfun ( func, iret )						*
 *									*
 * Input parameters:							*
 *	*func		const char	Input function			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * J. Nielsen/MIT	 4/88	Added ^ and & 				*
 * M. desJardins/GSFC	 7/88	Added %; changed & to @ for TAE		*
 * K. Brill/GSC 	 9/89   Addd +					*
 * K. Brill/NMC		 5/93	Assume FUNC is already upper case	*
 * M. desJardins/NMC	 8/93	Use MMFILE as maximum number of files	*
 * S. Jacobs/EAI	 9/93	Fixed typo				*
 * P. Bruehl/Unidata     8/94   Increased dimension on f from 20 to 36	*
 *				to allow for grids w/ 2 times after "^"	*
 * T. Piper/GSC		 7/01	Fixed typo f*30 -> f*36			*
 * m.gamazaychikov/SAIC	09/05	Added call to ST_NARG			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char f[37], c, dfunc[133];
    int dtm, lev, vcr, fil;
    int nf, iend, ival, ier, i;
/*----------------------------------------------------------------------*/
    /*
     * Initialize variables.
     */
    *iret  = 0;
    _dgtabl.ltabl = -1;
    memset ( f, 0, sizeof(f) );
    nf = 0;
    strcpy ( dfunc, func );
    cst_lstr ( dfunc, &iend, &ier );

    /*
     * First replace brackets with parentheses and semicolons with commas.
     * Brackets and semicolons may be used to make the function easier to
     * enter in the TAE tutor.
     */
    for ( i = 0;  i < iend; i++ ) {
	c  =  dfunc[i];
	if ( c == '[' ) dfunc[i] = '(';
	if ( c == ']' ) dfunc[i] = ')';
	if ( c == ';' ) dfunc[i] = ',';
    }

    /*
     * Loop through input string checking each character.
     */
    dtm = G_FALSE;
    lev = G_FALSE;
    vcr = G_FALSE;
    fil = G_FALSE;

    for ( i = 0; i < iend; i++ ) {
	c = dfunc[i];

	if ( c == ' ' ) {
	    /*
	     * Do nothing for blanks.
	     */
	} else if ( c == '(' ) {
	    /*
	     * Left parentheses, (, indicate the end of a function.
	     */
	    if ( nf != 0 ) {
		_dgtabl.ltabl++;
	        strcpy ( _dgtabl.ctabl[_dgtabl.ltabl], "*" );
		strcat ( _dgtabl.ctabl[_dgtabl.ltabl], f );
		cst_narg ( dfunc, i, ',', &_dgtabl.nfargs[_dgtabl.ltabl], &ier );
    		memset ( f, 0, sizeof(f) );
		nf = 0;
	    }


	} else if ( ( c == ')' ) || ( c == ',' ) || ( c == '%' ) ||
	            ( c == '^' ) || ( c == '@' ) || ( c == '+' ) ) {
	    /*
	     * Right parentheses, ), or commas indicate end of a parm.
	     * %, ^, @, + flag end of parm and beginning of vcord, time
	     * level or file number.
	     */
	    if ( nf != 0 ) {
		if ( dtm == G_TRUE ) {
		    if ( _dgtabl.ltabl > -1 ) {
		        strcpy ( _dgtabl.cgdttm[_dgtabl.ltabl], f );
		    }
		    dtm = G_FALSE;
		} else if ( lev == G_TRUE ) {
		    if ( _dgtabl.ltabl > -1 ) {
		        strcpy ( _dgtabl.clevel[_dgtabl.ltabl], f );
		    }
		    lev = G_FALSE;
		} else if ( vcr == G_TRUE ) {
		    if ( _dgtabl.ltabl > -1 ) {
		        strcpy ( _dgtabl.cvcord[_dgtabl.ltabl], f );
		    }
		    vcr = G_FALSE;
		} else if ( fil == G_TRUE ) {
		    if ( _dgtabl.ltabl >= 0 ) {
			cst_numb ( f, &ival, &ier );
			if ( ( ival > 0 ) && ( ival <= MMFILE ) ) {
			    _dgtabl.icflnm[_dgtabl.ltabl] = ival;
			}
		    }
		    fil = G_FALSE;
		} else {
		    _dgtabl.ltabl++;
		    strcpy ( _dgtabl.ctabl[_dgtabl.ltabl], f );
		    _dgtabl.nfargs[_dgtabl.ltabl] = 0;
		    _dgtabl.clevel[_dgtabl.ltabl][0] = '\0';
		    _dgtabl.cvcord[_dgtabl.ltabl][0] = '\0';
		    strcpy ( _dgtabl.cgdttm[_dgtabl.ltabl], _dginpt.ingdtm );
		    _dgtabl.icflnm[_dgtabl.ltabl] = 1;
		}
    		memset ( f, 0, sizeof(f) );
		nf = 0;
		if ( c == '%' )  vcr = G_TRUE;
		if ( c == '^' )  dtm = G_TRUE;
		if ( c == '@' )  lev = G_TRUE;
		if ( c == '+' )  fil = G_TRUE;
	    }
	} else if ( i == iend - 1) {
	    /*
	     * At end of function, check for single parameter.
	     */
	    f[nf++] = c;
	    if ( dtm == G_TRUE ) {
		if ( _dgtabl.ltabl > -1 ) {
		    strcpy ( _dgtabl.cgdttm[_dgtabl.ltabl], f );
		}
		dtm = G_FALSE;
	    } else if ( lev == G_TRUE ) {
		if ( _dgtabl.ltabl > -1 ) {
		    strcpy ( _dgtabl.clevel[_dgtabl.ltabl], f );
		}
		lev = G_FALSE;
	    } else if ( vcr == G_TRUE ) {
		if ( _dgtabl.ltabl > -1 ) {
		    strcpy ( _dgtabl.cvcord[_dgtabl.ltabl], f );
		}
		vcr = G_FALSE;
	    } else if ( fil == G_TRUE ) {
		if ( _dgtabl.ltabl > -1 ) {
		    cst_numb ( f, &ival, &ier );
		    if ( ( ival > 0 ) && ( ival <= MMFILE ) ) {
			_dgtabl.icflnm[_dgtabl.ltabl] = ival;
		    }
		}
		fil = G_FALSE;
	    } else {
		_dgtabl.ltabl++;
		strcpy ( _dgtabl.ctabl[_dgtabl.ltabl], f );
		_dgtabl.nfargs[_dgtabl.ltabl] = 0;
		_dgtabl.clevel[_dgtabl.ltabl][0] = '\0';
		_dgtabl.cvcord[_dgtabl.ltabl][0] = '\0';
		strcpy ( _dgtabl.cgdttm[_dgtabl.ltabl], _dginpt.ingdtm );
		_dgtabl.icflnm[_dgtabl.ltabl] = 1;
	    }
	} else {
	    /*
	     * Otherwise, add this character to the function name.
	     */
	    f[nf++] = c;
	}
    }

    /*
     * Compute the area to include.
     */
    dg_sare ( &ier );

    return;
}
