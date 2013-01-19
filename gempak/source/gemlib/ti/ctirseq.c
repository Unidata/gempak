#include "geminc.h"
#include "gemprm.h"

void cti_rseq ( int ntimes, char **timin, char **timout )
/************************************************************************
 * cti_rseq								*
 *									*
 * This subroutine resequences a list of times.				*
 *									*
 * void cti_rseq ( ntimes, timin, timout, iret )			*
 *									*
 * Input parameters:                                                    *
 *	ntimes		int		Number of times			*
 *	**timin		char		GEMPAK times			*
 *									*
 * Output parameters:                                                   *
 *	**timout	char		Resequenced times		*
 *									*
 **									*
 *  Log:								*
 * T. Piper/SAIC	09/07	Created.				*
 ***********************************************************************/
{
    char *p_tim;
    int ii, jj;
/*---------------------------------------------------------------------*/
    jj = ntimes-1;
    for ( ii = 0; ii < jj/2; ii++ ) {
	p_tim = timin[ii];
	timout[ii] = timin[jj];
	timout[jj] = p_tim;
	jj--;
    }
}
