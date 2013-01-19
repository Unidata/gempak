#include "gbcmn.h"

int gb_btoi ( unsigned char *ptarray, int indx, int no_bytes, int neg )
/************************************************************************
 * gb_btoi								*
 *									*
 * This function will convert the specified number of bytes into an	*
 * integer.								*
 *									*
 * int gb_btoi ( ptarray, indx, no_bytes, neg )				*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 *	indx		int		Index into buffer array		*
 *	no_bytes	int		Number of bytes requested	*
 *	neg		int		Negative value flag		*
 *									*
 * Output parameters:							*
 *	gb_btoi		int		Integer value of the combined	*
 *					    bytes			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI	 8/93		Combined functions into one	*
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 8/94		Changed pow calculation to a	*
 *					  LEFT SHIFT with an OR		*
 * T. Piper/GSC		11/98		Updated prolog			*
 ***********************************************************************/
{
	int		i, indx1, itemp, value=0, sign=1;

/*---------------------------------------------------------------------*/

	indx1 = indx;

	for ( i = 0; i < no_bytes; i++ ) {
	    itemp = (int) ptarray[indx1];
	    indx1++;
	    if ( ( i == 0 ) && ( neg ) ) {
		if ( ( ( itemp & 128 ) >> 7 ) == 1 ) {
		    itemp &= 127;
		    sign = -1;
		}
	    }
	    value = value << 8 | itemp;
	}
	return ( value * sign );
}
