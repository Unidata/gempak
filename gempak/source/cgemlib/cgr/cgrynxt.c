#include "geminc.h"
#include "gemprm.h"
#include "scnfll.h"

int cgrynxt ( int cnt, int iy[], int k, int *iret );

/* NOTE:  This function is currently NOT used.
 * 	  Can it be trashed?
 */

int cgrynxt ( int cnt, int iy[], int k, int *iret )
/************************************************************************
 * cgrynxt								*
 *									*
 * This function returns the next "y" coordinate value from an array	*
 * based upon the passed in array and the current "y" value.  		*
 *									*
 * int cgrynxt ( cnt, iy, k, iret )					*
 *									*
 * Input parameters:							*
 *  cnt		int	number of vertices passed in IY			*
 *  iy []	int	array of passed in "y" vertices for the polygon	*
 *  k		int	current count of vertices			* 
 *									*
 * Output parameters:							*
 * *iret	int	Return code					*
 * cgrynxt  	int	y coordinate of next vertex			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Safford/GSC	11/96	Adapted for use in UTF driver   	*
 * E. Safford/GSC	03/97	Modified includes and moved to cgemlib	*
 * M. Linda/GSC		 6/97	Changed 1 <= j <= cnt to 0 <= j < cnt	*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{
  	int j;

	*iret = G_NORMAL;
/*
 *	Determine if the next value (j) is to be found at k + 1 or 
 *      back to the beginning of the list of values (0).
 */
	if  ( ( k + 1 ) < cnt )
            j = k + 1;
	else
            j = 0;

/*
 *	Advance j if the y values of [k] and [j] are equal.  This
 *	throws out horizontal lines.
 */
	while ( iy[k] == iy[j])
     	{
            if  ( ( j + 1 ) < cnt )
                j++;
            else
                j = 0;
        }

        return (iy[j]);

}

