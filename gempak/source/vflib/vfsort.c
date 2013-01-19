#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern SpcInfo_t      spcinfo;

void vfsort ( int *iind, int *iret )
/************************************************************************
 * vfsort                                                               *
 *                                                                      *
 * This program sorts the independent cities list.			*
 *                                                                      *
 * vfsort ( iind, iret )                                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *iind            int            Number of independent cities	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		 6/00   					*
 * A. Hardy/GSC		 5/01		Initialized iret to 0		*
 ***********************************************************************/
{
    char    temp[33];
    int     ii, jj, icnt;
/*-------------------------------------------------------------------*/
    *iret = 0;
    icnt = *iind;
    for ( ii = 0; ii < icnt; ii++ ) {
        for ( jj = ii+1; jj < icnt; ++jj ) {
	    if ( strcmp(spcinfo.cnty[ii].indnam, spcinfo.cnty[jj].indnam) > 0 ) {
	        strcpy(temp, spcinfo.cnty[ii].indnam);
		strcpy(spcinfo.cnty[ii].indnam, spcinfo.cnty[jj].indnam);
		strcpy(spcinfo.cnty[jj].indnam, temp);
	    }
	}
    }
}

