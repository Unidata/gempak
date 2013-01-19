#include "dg.h"

void dg_merr ( const char *name, const char *time1, const char *time2,
               const int *level1, const int *level2, const int *ivcord,
	       char *errst, int *iret )
/************************************************************************
 * dg_merr								*
 *									*
 * This subroutine builds ERRST from NAME, the parameter name or any	*
 * string, and TIME, LEVEL, and IVCORD, if given:			*
 *									*
 *     ERRST = NAME // '^' // TIME // '@' // LEVEL // '%' // IVCORD	*
 *									*
 * If the inputs have missing values, that part of ERRST is omitted.	*
 * Missing values are:							*
 *									*
 *     NAME    blank							*
 *     TIME    blank  (both values omitted if first is blank)		*
 *     LEVEL    -1    (both values omitted if first is -1)		*
 *     IVCORD   -1							*
 *									*
 * The extra check on TIME and LEVEL allow the user to put a single	*
 * missing value in the call, rather than defining a dummy array.	*
 * If the user wishes to force a missing value to appear in ERRST, it	*
 * must be set explicitly in the calling program (e.g., ERRST = '%-1'),	*
 * rather than calling DG-MERR.						*
 *									*
 * ERRST is returned to the calling program, which should put it in	*
 * COMMON for retrieval by DG_GRID or DG_VECT and inclusion in the call	*
 * to ER_WMSG as the text string.					*
 *									*
 * dg_merr ( name, time1, time2, level1, level2, ivcord, errst, iret )	*
 *									*
 * Input parameters:							*
 *	*name		const char	A name or string		*
 *	*time1		const char	Date/time			*
 *	*time2		const char	Date/time			*
 *	*level1		const int	Grid level			*
 *	*level2		const int	Grid level			*
 *	*ivcord		int		Vertical coordinate		*
 *									*
 * Output parameters:							*
 *	*errst		char		Diagnostic error string		*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 9/88						*
 * M. desJardins/GSFC	 4/89	Added spaces in message			*
 * T. Piper/SAIC	 2/02	Increased lvl and vcor to 12 due to UMR	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char lvl[13], vcor[13];
    int ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Initialize variables; put NAME at the beginning.
     */
    strcpy ( errst, name );

    /*
     * Process TIME into '^' // TIME (1) // ':' // TIME (2)
     */
    if ( strlen ( time1 ) > 0 ) {
        strcat ( errst, " ^" );
	strcat ( errst, time1 );
	if ( strlen ( time2 ) > 0 ) {
	    strcat ( errst, ":" );
	    strcat ( errst, time2 );
	}
    }

    /*
     * Process LEVEL into '@' // LEVEL (1) // ':' // LEVEL (2)
     */
    if ( *level1 != -1 ) {
	cst_inch ( *level1, lvl, &ier );
	strcat ( errst, " @" );
	strcat ( errst, lvl );
	if ( *level2 != -1 ) {
	    cst_inch ( *level2, lvl, &ier );
	    strcat ( errst, ":" );
	    strcat ( errst, lvl );
	}
    }

    /*
     * Process IVCORD into '%' // IVCORD
     * IVCORD is given as a number if it's not a standard name.
     */
    if ( *ivcord != -1 ) {
	clv_ccrd ( *ivcord, vcor, &ier );
	strcat ( errst, " %" );
	strcat ( errst, vcor );
    }

    return;
}
