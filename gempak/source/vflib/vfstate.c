#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t   newinfo;
extern  SpcInfo_t   spcinfo;

void vfstate ( int *iret )
/************************************************************************
 * vfstate								*
 *                                                                      *
 * This function finds the full name of the state associated with the   *
 * two anchor point stations.						*
 *                                                                      *
 * vfstate ( iret )                                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC         11/99   Added correction for Wash. DC           *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 5/01   Initialized iret to 0 			*
 ***********************************************************************/
{
    int      ier;
    char     stid[3], stnnam[17];
/*-------------------------------------------------------------------*/
     *iret = 0;
     ier = 0;
   /*
    * Retrieve the first state name.
    */

    strcpy ( stnnam, "\0" );
    strcpy ( stid, spcinfo.ancrpt.stateid1 );
    if ( strcmp (stid, "DC") != 0 ) {
        tb_idst ( stid, stnnam, &ier, strlen(stid), sizeof(stnnam) ); 
        strcpy ( spcinfo.ancrpt.statnm1, stnnam );
    }
    else {
        strcpy ( spcinfo.ancrpt.statnm1, stid );
    }

   /*
    * Retrieve the second state name.
    */

    strcpy ( stnnam, "\0" );
    strcpy ( stid, spcinfo.ancrpt.stateid2 );
    if ( strcmp (stid, "DC") != 0 ) {
        tb_idst ( stid, stnnam, &ier, strlen(stid), sizeof(stnnam) ); 
        strcpy ( spcinfo.ancrpt.statnm2, stnnam );
    }
    else {
        strcpy ( spcinfo.ancrpt.statnm2, stid );
    }
}
