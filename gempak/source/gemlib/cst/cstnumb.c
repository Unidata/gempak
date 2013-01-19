#include "geminc.h" 
#include "gemprm.h"

void cst_numb ( char *str, int *intg, int *iret )
/************************************************************************
 * cst_numb								*
 *									*
 * This routine converts a string to an integer.			*
 *									*
 * cst_numb ( str, intg, iret ) 					*
 *									*
 * Input parameters:							*
 *	*str		char	String					*
 *									*
 * Output parameters:							*
 *	*intg		int 	Conversion result			*
 *	*iret		int	Return code				*
 *				 0 = normal return			*
 *				-2 = conversion error			*
 **									*
 * Log: 								*
 * L. Williams/EAI	 4/96	Created					*
 * M. Linda/GSC 	10/97	Corrected the prologue format		*
 * G. Krueger/EAI	10/97	Remove blanks beforehand; Fix IMISSD	*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
char	*ptr, buffer[13];
int	ier, lens;

/*---------------------------------------------------------------------*/
	ier = 0;
	*iret = 0;
	*intg = IMISSD;

	/*
	 * remove blanks from string.
	 */
	cst_rmbl( str, buffer, &lens, &ier );
	ptr = buffer;

	/*
	 * ignore '-' and '+' signs in first character
	 */
	if( ( *ptr == '-' ) || ( *ptr == '+' ) )
	   ++ptr;

	/*
	 * check if the input is valid
	 */
	while( *ptr ) {
	   if( ( !isdigit( *ptr ) ) && ( *ptr != '.' ) ) {
	      *iret = -2;
	      return;
	   }
	   else {
	      if( *ptr == '.' )
		 if( isdigit( *(ptr+1) ) ){
		    *iret = -2;
		    return;
		 }
	   }
	   ++ptr;
	}


	/*
	 * convert string into an integer
	 */
	*intg = atoi( buffer );

}
