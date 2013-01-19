#include "geminc.h"
#include "gemprm.h"

void cst_tims ( int ntimes, char *timestr, dttms_t tmarry[], int *iret )
/************************************************************************
 * cst_tims								*
 *									*
 * This subroutine parses the time string separated by ';' into an	*
 * array of time strings.						*
 *									*
 * cst_tims (ntimes, timestr, tmarry, iret)				*
 *									*
 * Input parameters:							*
 *	ntimes		int		Number of times in the string	*
 *	*timestr	char		Time string			*
 *									*
 * Output parameters:							*
 *	tmarry[]	dttms_t		array of time strings		*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * C. Lin/EAI		 8/98                                         	*
 * S. Law/GSC		12/98	Dimension tmarry with LLDTSZ		*
 * S. Law/GSC		05/99	tmarry moved to typedef dttms_t		*
 ***********************************************************************/
{
    int		ii, numstr, ier;
    char	**times;
/*---------------------------------------------------------------------*/

    *iret = 0;
    times = (char **) malloc(sizeof(char *) * ntimes);
    for (ii = 0; ii < ntimes; ii++) {
	times[ii] = (char *) malloc(128);
    }

    cst_clst (timestr, ';', " ", ntimes, 128, times, &numstr, &ier);

    for (ii = 0; ii < ntimes; ii++) {
	strcpy (tmarry[ii], times[ii]);
	free (times[ii]);
    }

    free (times);
}
