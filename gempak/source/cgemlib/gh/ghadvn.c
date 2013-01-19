#include "geminc.h"
#include "gemprm.h"

#define MAXADVN 5

void gh_advn ( char *advisory, int *adnum, int *iflag, int *iret )
/************************************************************************
 * gh_advn								*
 *									*
 * This function validates a tropical storm advisory number and returns *
 * the numeric part of the advisory number as well as a flag indicating *
 * whether it is a regular or intermediate advisory.                    *
 *									*
 * gh_advn ( advisory, adnum, iflag, iret )  				*
 *									*
 * Input parameters:							*
 *									*
 *      *advisory        char           Storm Advisory Number           *
 *									*
 * Output parameters:							*
 *      *adnum           int            Numeric part of Storm Advisory  *
 *                                      Number                          *
 *      *iflag           int            0 = regular advisory            *
 *                                      1 = 1st intermediate advisory=a *
 *                                      2 = 2nd intermediate advisory=b *
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					  -51 = Invalid advisory number *
 *                                              format                  *
 *									*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	 1/06   					*
 ***********************************************************************/
{
    int             len, ier;
    char            letter, tmpadnum[MAXADVN];
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  check if the advisory number is numeric
     */
    cst_numb ( advisory, adnum, &ier );
    if ( ier == 0 ) {
        *iflag = 0;
        /*
         *  value is numeric...check range
         */
        if ( *adnum <= 0 || *adnum > 999 ) *iret = -51;
        return;
    }

    /*
     *  Not numeric; check to see if ends with 'a' or 'b'.
     *  remove last character and check again if numeric.
     */
    cst_lstr ( advisory, &len, &ier );
    if ( len >= MAXADVN || len == 1 ) {
        *iret = -51;
        return;
    }
  
    letter = advisory [ len - 1 ];
    cst_ncpy ( tmpadnum, advisory, len - 1, &ier );
    cst_numb ( tmpadnum, adnum, &ier );
    if ( ier == 0 ) {
        if ( letter == 'a' || letter == 'A' )
            *iflag = 1;
        else if ( letter == 'b' || letter == 'B' )
            *iflag = 2;
        else
            *iret = -51;
    }
    else {
        *iret = -51;
    }

    return;

}
