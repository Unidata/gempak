#include "geminc.h"
#include "gemprm.h"

void cst_rang ( char *str, char *first, char *last, char *inc, 
						int *type, int *iret )
/************************************************************************
 * cst_rang								*
 *									*
 * This subroutine changes a string range into the beginning, end, and	*
 * increment values.  The values must be separated by '-'.		*
 *									*
 * cst_rang ( str, first, last, inc, type, iret )			*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*first	 	char		First value in range		*
 * 	*last		char		Last value in range		*
 *	*inc		char		Range increment			*
 *	*type		int		Range type			*
 *					  0 = no range input		*
 *					  1 = range without increment	*
 *					  2 = range with increment	*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * L. Williams/EAI	 7/96	Rewrite without STRTOK			*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL to char for LINUX	*
 ***********************************************************************/
{
char	*ptr, *next;
int	pos;
int	ier;

/*---------------------------------------------------------------------*/
	*iret = 0;
	*type = 0;
	*first = '\0';
	*last = '\0';
	*inc = '\0';

	/*
	 * remove the minus sign 
	 */
	if( str[0] == '-' )
	   ptr = &str[1];
	else
	   ptr=str;

	next=str;

	/*
	 * check for first dash (-)
	 */
	ptr = strstr( ptr, "-" );
	if( ! ptr )
	   return;
	pos = strlen( next ) - strlen( ptr );
	cst_ncpy( first, next, pos, &ier );
	strcpy( last, first );
	*type = 1;

	/*
	 * advance pointers
	 */
	if( *(ptr+1) != (char)NULL ) {
	   next = ptr + 1;
	}
	else {
	   next = '\0';
	}
	ptr++; 
	
	if( next ) {
	   /*
	    * check for second dash (-)
	    */
	   ptr = strstr( ptr, "-" );
	   if( ptr ) {
	      pos = strlen( next ) - strlen( ptr );
	      cst_ncpy( last, next, pos, &ier );
	      if( *(ptr+1) != (char)NULL ) {
	         strcpy( inc, ptr+1 );
	         *type = 2;
	      }
	   }	
	   else {
	      strcpy( last, next );
	   }
	}
	
}
