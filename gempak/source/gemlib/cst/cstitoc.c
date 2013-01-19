#include "geminc.h"
#include "gemprm.h"

#define MAXCHAR 4

void cst_itoc ( int *intary, int nval, char **strptr, int *iret )
/************************************************************************
 * cst_itoc								*
 *									*
 * This subroutine decodes an array of integers containing four		*
 * characters each into a character string array.			*
 *									*
 * cst_itoc ( intary, nval, strptr, iret )				*
 *									*
 * Input parameters:							*
 *	*intary		int		Integer array			*
 *	nval		int	 	Number of integers 		*
 *									*
 * Output parameters:							*
 *	**strptr	char		Pointer to character array	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid number of itegers *
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 ***********************************************************************/
{
union {
       int    value;
       char   byte[4];
} data;

int	i, j;
int	index, clen;

/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * check the number of integers
	 */
	if( nval <= 0 ) {
	   **strptr = '\0';
	   *iret = -1;
	   return;
	}

	/*
	 * process each integer.
	 */
	for( i=0; i < nval; i++ ) {
	   data.value = intary[i];
	   index=0;
	   /*
	    * check for NULL
	    */
	   for( j=0; j < 4; j++ ) {
	      if( data.byte[j] == 0 )
		   index++;
	      else
		   break;
	   }
	   clen = MAXCHAR - index;
	   memcpy( strptr[i], &(data.byte[index]), clen );
	   strptr[i][clen] = '\0';
	}

}
