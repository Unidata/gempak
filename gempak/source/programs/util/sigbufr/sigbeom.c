#include <mel_bufr.h>

#include "geminc.h"
#include "gemprm.h"

#include "sigbcmn.h"

void sigbeom ( int *n, int *iret )
/************************************************************************
 * sigbeom                                                              *
 *                                                                      *
 * This function processes a BUFR end-of message condition.             *
 *                                                                      *
 * sigbeom ( n, iret )                                                  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *n              int             BUFR flag value                 *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         leverr, numerr, ier;
    char        errgrp[8], cc[2];
/*---------------------------------------------------------------------*/
    *iret = 0;

    leverr = 0;
    numerr = -15;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

    *n     = Set_Status ();

   /*
    * There's another message to read. 
    */

    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
              strlen(errgrp), strlen(cc) );

    *n = BUFR_Find_End ();
    if ( *n != 0 ) {
        *n = BUFR_EOF;
    }
}
