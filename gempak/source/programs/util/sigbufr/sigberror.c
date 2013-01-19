#include <mel_bufr.h>

#include "geminc.h"
#include "gemprm.h"

#include "sigbcmn.h"

void sigberror ( const char *str, int *iret )
/************************************************************************
 * sigberror                                                            *
 *                                                                      *
 * This function processes a BUFR error condition.                      *
 *                                                                      *
 * sigberror ( str, iret )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      *str            char            Name of calling subroutine      *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int		leverr, numerr, ier;
    char	errgrp[8], cc[2];
/*---------------------------------------------------------------------*/
    *iret = 0;

    leverr = 0;
    numerr = -12;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
              strlen(errgrp), strlen(cc) );

   /* 
    * Print the reason for the error and exit.
    */

    BUFR_perror ( (char*)str );
    BUFR_Destroy (1);
}
