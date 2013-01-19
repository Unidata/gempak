#include "geminc.h"
#include "gemprm.h"

void ggoadl ( char *filname, int *iret );

void ggoadl ( char *filname, int *iret )
/************************************************************************
 * ggoadl                                                             	*
 *                                                                      *
 * This function deletes a file created by the graph-to-grid OA pgm.	*
 *                                                                      *
 * ggoadl ( filnam, iret )                      			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *filnam         char            File name                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal, file exists       *
 *                                       -1 = File does not exist       *
 **                                                                     *
 * D.W.Plummer/NCEP	05/05						*
 ***********************************************************************/
{
int	ier;
long	flen;
char	newfil[FILE_FULLSZ], defdir[LLPATH];
/*---------------------------------------------------------------------*/

    *iret = 0;

    cfl_inqr ( filname, defdir, &flen, newfil, &ier );

    if ( ier == 0 )  {

	/*
	 * File exists
	 * - assume file is already closed (by OABSDR),
	 * - delete it.
	 */
	ier = unlink ( (const char *)newfil );

    }

}

