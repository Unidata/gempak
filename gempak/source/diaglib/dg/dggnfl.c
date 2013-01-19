#include "dg.h"

void dg_gnfl ( const int *entry, char *ntmplt, char *gflpth,
               char *crtfnm, char *crtgdt1, char *crtgdt2,
	       int *mbrnum, int *iret )
/************************************************************************
 * dg_gnfl                                                              *
 *                                                                      *
 * This subroutine gets the NFILE structure members.			*
 *                                                                      *
 * dg_gnfl ( entry, ntmplt, gflpth, crtfnm, crtgdt1, crtgdt2, mbrnum,	*
 *           iret )							*
 *                                                                      *
 * Input parameter:							*
 *	*entry		const int	GDFILE entry number		*
 * Output parameters:                                                   *
 *	*ntmplt		char		Member ntmplt			*
 *	*gflpth		char		Member gflpth			*
 *	*crtfnm		char		Member crtfnm			*
 *	*crtgdt1	char		Member crtgdt1			*
 *	*crtgdt2	char		Member crtgdt2			*
 *	*mbrnum		int		Member mbrnum			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/05						*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    i = (*entry) - 1;
    strcpy ( ntmplt, _nfile.ntmplt[i] );
    strcpy ( gflpth, _nfile.gflpth[i] );
    strcpy ( crtfnm, _nfile.crtfnm[i] );
    strcpy ( crtgdt1, _nfile.crtgdt1[i] );
    strcpy ( crtgdt2, _nfile.crtgdt2[i] );
    *mbrnum = _nfile.mbrnum[i];

    return;
}
