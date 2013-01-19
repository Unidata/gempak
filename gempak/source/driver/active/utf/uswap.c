#include "utfcmn.h"

void uswap ( unsigned char va[], int ilen, unsigned char newva[], 
						int *jlen, int *iret )
/************************************************************************
 * uswap								*
 *                                                                      *
 * This subroutine processes an array prior to writing to the output    *
 * file.  There are some control characters in AFOS which cannot be     *
 * used where not appropriate.  If these values match one of those      *
 * desired for a relative vector byte, then a substitution must be      *
 * made.  The two substitutions are:					*
 *	0x10   becomes  0x1010  (that is the value is duplicated in	*
 *				 next byte)				*
 *	0x83   becomes  0x100c  (by itself, 0x83 signals the end of a	*
 *				 map or data)				*
 *									*
 * uswap ( va, ilen, newva, jlen, iret )				*
 *									*
 * Input parameters:							*
 *      va [ilen]	unsigned char   Array of original information	*
 *      ilen		int		Num of elements in orig array	*
 *									*
 * Output parameters:							*
 *	newva [jlen]	unsigned char	Array of swapped information	*
 *	*jlen		int		Num of elements in swapped array*
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Updated header and added comments	*
 ***********************************************************************/
{

	int		i;
	
/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	*jlen = 0;

	for ( i = 0; i < ilen; i++ )  {
/*
 *	    Replace the value 0x10 with 0x1010.
 */
	    if  ( va[i] == 0x10 )  {
		newva[*jlen]     = 0x10;
		newva[*jlen + 1] = 0x10;
		*jlen += 2;
	    }
/*
 *	    Replace the value 0x83 with 0x100c.
 */
	    else if  ( va[i] == 0x83 )  {
		newva[*jlen]     = 0x10;
		newva[*jlen + 1] = 0x0c;
		*jlen += 2;
	    }
/*
 *	    This is not a special value, just add it to the new array.
 */
	    else {
		newva[*jlen]     = va[i];
	        *jlen += 1;
	    }
	}

}
