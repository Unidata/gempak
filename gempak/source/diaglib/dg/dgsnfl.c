#include "dg.h"

void dg_snfl ( const int *entry, const char *ntmplt, const char *gflpth,
               const char *crtfnm, const char *crtgdt1, const char *crtgdt2,
	       const int *mbrnum, int *iret )
/************************************************************************
 * dg_snfl                                                              *
 *                                                                      *
 * This subroutine sets the NFILE structure members.			*
 *                                                                      *
 * dg_snfl ( entry, ntmplt, gflpth, crtfnm, crtgdt1, crtgdt2, mbrnum,	*
 *           iret )							*
 *                                                                      *
 * Input parameter:							*
 *	*entry		const int	GDFILE entry number		*
 *	*ntmplt		const char	Member ntmplt			*
 *	*gflpth		const char	Member gflpth			*
 *	*crtfnm		const char	Member crtfnm			*
 *	*crtgdt1	const char	Member crtgdt1			*
 *	*crtgdt2	const char	Member crtgdt2			*
 *	*mbrnum		const int	Member mbrnum			*
 * Output parameters:                                                   *
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
    strcpy ( _nfile.ntmplt[i], ntmplt );
    strcpy ( _nfile.gflpth[i], gflpth );
    strcpy ( _nfile.crtfnm[i], crtfnm );
    strcpy ( _nfile.crtgdt1[i], crtgdt1 );
    strcpy ( _nfile.crtgdt2[i], crtgdt2 );
    _nfile.mbrnum[i] = *mbrnum;

    return;
}
