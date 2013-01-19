#include "geminc.h"
#include "gemprm.h"

void cst_flst ( char *str, char sep, char *defstr, int nexp, 
			int maxchr, char **namptr, int *num, int *iret )
/************************************************************************
 * cst_flst								*
 *									*
 * This subroutine breaks a string containing a list of file names into *
 * array of file names.  The separator for the strings is input as SEP.	*
 * If the seperator is a blank, multiple blanks will be treated as one.	*
 * If null strings are encountered or fewer than NEXP strings are found	*
 * in the string, the appropriate NAMPTR locations are set to DEFSTR.	*
 *									*
 * If the first file name has two "\"s in it, the subsequent file names	*
 * are used to replace the text delineated by the "\"s in order to	*
 * generate new file names.						*
 *									*
 * cst_flst ( str, sep, defstr, nexp, maxchr, namptr, num, iret )	*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	sep		char		Separator			*
 *	*defstr		char	 	Default string			*
 *	nexp		int		Number of expected values	*
 *	maxchr		int		Maximum characters in strings	*
 *									*
 * Output parameters:							*
 *	**namptr	char		Pointer to file names		*
 *	*num		int		Number of names returned	*
 *	*iret		int		Return code			*
 *					  2 = string too long		*
 *					  1 = more than nexpv values	*
 *					  0 = normal			*
 *					 -1 = invalid nexp		*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * G. Krueger/EAI	10/97	Removed MALLOC, RSPTB; Add str limit	*
 ***********************************************************************/
{
char	*ptr, *ptr1, *ptr2;
int	count;
int	i, len1, len2, leni;

/*---------------------------------------------------------------------*/

	*iret = 0;
	*num = 0;

	/*
	 * check the number of expected values.
	 */
	if( nexp <= 0 ) {
	   namptr = NULL;
	   *iret = -1;
	   return;
	}

	cst_clst( str, sep, defstr, nexp, maxchr, namptr, num, iret );

	if ( *iret != 0 ) return;

	if( *num == 0 ) return;

	ptr = namptr[0];

	/*
	 * check if the first file name has two "\"s
	 */
	count=0;
	while( *ptr ) {
	   if( *ptr == CHBKSL )
	      ++count; 
	   ++ptr;
	}

	/*
	 * if two "\"s were found process the new file names
	 */
	if( count == 2 ) {

	   /*
	    * identify first part of file name
	    */
	   ptr1 = strchr( namptr[0], CHBKSL ) - 1;
	   len1 = ptr1 - namptr[0] + 1;

	   /*
	    * identify second part of file name
	    */
	   ptr2 = strchr( ptr1 + 2, CHBKSL ) + 1;
	   len2 = strlen( ptr2 );

	   /*
	    * add file name to file extensions
	    */
	   for( i=1; i < *num; i++ ) {
	      leni = strlen(namptr[i]);
	      memmove( &namptr[i][len1], namptr[i], leni + 1 );
	      memcpy( namptr[i], namptr[0], len1 );
	      strcat( namptr[i], ptr2 );
	   }

	   /*
	    * remove the two backslashes from the first string
	    */
	   memmove( ptr2 - 1, ptr2, len2 + 1 );
	   len2 = strlen( ptr1 + 2 );
	   memmove( ptr1 + 1, ptr1 + 2, len2 + 1 );
	}

}
