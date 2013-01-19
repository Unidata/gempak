#include <mel_bufr.h>

#include "geminc.h"
#include "gemprm.h"

#include "sigbcmn.h"

void sigbeof ( int n, int *iret )
/************************************************************************
 * sigbeof                                                              *
 *                                                                      *
 * This function processes a BUFR end-of-file condition.                *
 *                                                                      *
 * sigbeof ( n, iret )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      n               int             BUFR flag value                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         leverr, numerr, ier;
    char        errgrp[8], cc[20];
/*---------------------------------------------------------------------*/
    *iret = 0;
 
    leverr = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

    if ( n == BUFR_EOF ) {
        numerr = -13;
    }
    else {
        cst_inch ( n, cc, &ier );
        numerr = -14;
    }

    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
              strlen(errgrp), strlen(cc) );

    BUFR_Err_Print ( NULL );
}
