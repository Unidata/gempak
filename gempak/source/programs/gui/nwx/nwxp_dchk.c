#include "nwx_cmn.h"


/************************************************************************
 * nwxp_dchk.c                                                          *
 *                                                                      *
 * This module checks the  type of input data (string).			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      dchk_alpha()   check to see if the data consists of alphabetic  *
 *					letter.   			*
 *      dchk_digit()   check to see if the data consists of digit       *
 * 					number.     			*
 ***********************************************************************/

/*=====================================================================*/

int dchk_alpha ( char *str )
/************************************************************************
 * dchk_alpha                                                           *
 *                                                                      *
 * This function checks if the input string consists of alphabetic      *
 *	letters.                                			*
 *                                                                      *
 * int dchk_alpha ( str )                                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *str		char           Input string            		*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *	 dchk_alpha	int	0  - it consists of alphabetic letters	*
 *				-1 - not				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI            6/95                   			*
 * C. Lin/EAI		      8/95 	header				*
 ***********************************************************************/
{
char    *ptr;
/*---------------------------------------------------------------------*/
	ptr = str;
	while (*ptr) {
	    if ( !isalpha(*ptr) ) return (-1);
	    ptr ++;
	}
	return(0);
}

/*=====================================================================*/

int dchk_digit ( char *str )
/************************************************************************
 * dchk_digit                                                           *
 *                                                                      *
 * This function checks if the input string consists of digit numbers.  *
 *                                                                      *
 * int dchk_digit ( str )                                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *str		char            Input string          		*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *	dchk_digit	int	0  - it consists of digit numbers	*
 *				-1 - not				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI            6/95                   			*
 * C. Lin/EAI		      8/95 	header				*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/
	for ( ii=0; ii < (int)strlen(str); ++ii ) {
	    if (( str[0] == '+' ) || ( str[0] == '-' ))
	       continue;
	    if ( !isdigit(str[ii]) && str[ii] != '.')
		return(-1);
	}
	return(0);
}
