#include "geminc.h"
#include "gemprm.h"

void clo_times ( fdttms_t start, int inc, int ntimes, fdttms_t *time, 
								int *iret )
/************************************************************************
 * clo_times								*
 *									*
 * This function calculates a range of times based on the increment.	*
 *									*
 * clo_times (start, inc, ntimes, time, iret)				*
 *									*
 * Input parameters:							*
 *	start		fdttms_t	Point #2 Time (YYMMDD/HHMM)	*
 *	inc		int		Track increment in minutes	*
 *	ntimes		int		Number of future times		*
 *									*
 * Output parameters:                                                   *
 *	*time		fdttms_t	Time array (YYMMDD/HHMM)	*
 *	*iret		int		Return value			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	moved from clo_track			*
 * S. Law/GSC		07/00	changed to use GEMPAK times		*
 * M. Li/GSC		10/00	changed increment to any integer	*
 ***********************************************************************/
{
    int ii, iarray[5], ier, imod;
/*---------------------------------------------------------------------*/

    *iret = 0;

    ti_ctoi (start, iarray, &ier, strlen (start));

    imod = inc - (iarray[4] % inc);

    for (ii = 0; ii < ntimes; ii++) {
	ti_addm (iarray, &imod, iarray, &ier);

	ti_itoc (iarray, time[ii], &ier, (FDTTMS_SIZE - 1));
	time[ii][DTTMS_SIZE - 1] = '\0';

	imod = inc;
    }
}
