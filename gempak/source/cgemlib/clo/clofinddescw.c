#include "geminc.h"
#include "gemprm.h"

void clo_finddescw ( char *locnam, char *xid, char *xstate, int *srchtyp,
                     int *maxlen, int *nret, char *info, int *iret )
/************************************************************************
 * clo_finddescw                                                        *
 *                                                                      *
 * This function is a wrapper for FORTRAN function call to CLO_FINDDESC *
 * Therefore, the calling sequence is the same as CLO_FINDDESC except   *
 * that all integers are converted to pointers.                         *
 *                                                                      *
 * clo_finddescw ( locnam,xid,xstate,srchtyp,maxlen,nret,info,iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *locnam         char            Data location name              *
 *      *xid            char            Station name or substring       *
 *      *xstate         char            State name (optional)           *
 *      *srchtyp        int             Search type                     *
 *                                      = 1 - EXACT                     *
 *                                      = 2 - FIRST                     *
 *                                      = 3 - INDEX                     *
 *      *maxlen         int             Max length of info string       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nret           int             Number of stations returned     *
 *      *info           char            String w/ station information   *
 *      *iret           int             Return code                     *
 *                                      = 0 - normal                    *
 *                                      = >0 - >maxret stations availble*
 *                                      = -2 - unable to match station  *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	09/08	Created                                 *
 ***********************************************************************/
{

    clo_finddesc ( locnam, xid, xstate, *srchtyp,
                  *maxlen, nret, info, iret );

}
