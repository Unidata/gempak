#include "geminc.h"
#include "gemprm.h"

void cst_zpad ( char *strin, int *nchar, char *strout, int *iret )
/************************************************************************
 * cst_zpad								*
 * 									*
 * This function will zero-pad to the left of a numeric value. The	*
 * total number of characters to the left of the decimal point is	*
 * input by the user. 							*
 * 									*
 * cst_zpad ( strin, nchar, strout, iret )				*
 *									*
 * Input parameters:							*
 *	*strin		char	Input value as a string			*
 *	*nchar		int	Number of total chars to the left of	*
 *				the decimal point			*
 *									*
 * Output parameters:							*
 *	*strout		char	Zero-padded value as a string		*
 *	*iret		int	Return code				*
 *				  0 = Normal return			*
 *				 +4 = Invalid number of chars		*
 *				 -8 = Cannot zero-pad non-numeric	*
 * 									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/06	Created					*
 ***********************************************************************/
{
    int		ival, ier;
    float	fval;
    char	frmt[12], *cdec, cval[160];

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * If the number of output chars is less than or equal to 0, 
     * copy the input string into the output string and return.
     */
    if  ( *nchar <= 0 )  {
	strcpy ( strout, strin );
	*iret = 4;
	return;
    }

    /*
     * If the input string is not a numeric value, copy the input
     * string into the output string and return with an error.
     */
    cst_crnm ( strin, &fval, &ier );
    if  ( ier != 0 )  {
	strcpy ( strout, strin );
	*iret = -8;
	return;
    }

    /*
     * Create the format string for the zero-padding.
     */
    sprintf ( frmt, "%%%d.%dd", *nchar, *nchar );

    /*
     * Split the input string on the decimal point. Apply the
     * zero-padding to the integral part and save the decimal part.
     */
    cdec = cst_split ( strin, '.', 160, cval, &ier );
    cst_numb ( cval, &ival, &ier );
    sprintf ( strout, frmt, ival );

    /*
     * If there is a decimal part, add it back onto the output string.
     */
    if  ( cdec )  {
	strcat ( strout, "." );
	strcat ( strout, cdec );
    }

}
