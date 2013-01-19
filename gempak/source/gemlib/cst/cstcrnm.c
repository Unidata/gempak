#include "geminc.h"
#include "gemprm.h"

void cst_crnm ( char *str, float *value, int *iret )
/************************************************************************
 * cst_crnm								*
 *									*
 * This routine converts a character string to a real number.  If the	*
 * conversion fails for any reason (including overflow and underflow),	*
 * RMISSD is returned with an error code of -2.				*
 *									*
 * cst_crnm ( str, value, iret )					*
 *									*
 * Input parameters:							*
 *	*str		char		String				*
 *									*
 * Output parameters:							*
 *	*value		float		Conversion result		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -2 = conversion error		*
 **									*
 * Log: 								*
 * L. Williams/EAI	 4/96						*
 * M. Linda/GSC 	10/97	Corrected the prologue format		*
 * G. Krueger/EAI	11/97	Remove blanks beforehand; Fix RMISSD	*
 * D.W.Plummer/NCEP	 8/00	Add check for length of incoming string	*
 * D.W.Plummer/NCEP	 6/06	Recode using strtod (chks 'E' exponent)	*
 ***********************************************************************/
{
char	*buffer, *endptr;
int	ier, lens;
double  dval;

/*---------------------------------------------------------------------*/
	ier = 0;
	*iret = 0;
	errno = 0;
	*value = RMISSD;

	G_MALLOC ( buffer, char, strlen(str)+1, 
		"cst_crnm - Error allocating buffer" );

	/*
	 * remove blanks from string.
	 */
	cst_rmbl( str, buffer, &lens, &ier );

	dval = strtod( buffer, &endptr );

	/*
	 * 'strtod' returns a double but we will be casting to a float.
	 * Therefore, check double value against the float limits first.
	 * Set error code if outside of these limits.
	 */

	if ( dval >  FLT_MAX  || 
	     dval < -FLT_MAX  ||
	     ( dval > 0.0 && dval <  FLT_MIN )  ||
	     ( dval < 0.0 && dval > -FLT_MIN ) )  {
	    *iret = -2;
	}
	else  {

	    *value = (float)dval;

	    /*
	     * If the conversion is successful, 'endptr' will point to the
	     * training NULL; otherwise not. Other indicators or conversion
	     * failure lies with 'errno' and the actual value of 'value'.
	     */

	    if ( endptr[0] != CHNULL || errno != 0 ||
		    ( !(*value <  HUGE_VAL) && !(*value >  HUGE_VAL) ) || 
		    ( !(*value < -HUGE_VAL) && !(*value > -HUGE_VAL) ) )  {
	        *value = RMISSD;
	        *iret = -2;
	    }

	}

	G_FREE ( buffer, char );

}
