#include "nagrib2.h"
#include "gb2def.h"

void  nadiag ( char *g2diag, int *g2dglst, int lstlen )
/************************************************************************
 * nadiag                                                               *
 *                                                                      *
 * This routine processes the G2DIAG input string.  The first part      *
 * of the string, before the "|" separator, contains a list of GRIB2    *
 * sections for which the contents should be printed.                   *
 * The second part, after the "|" separator specifies the number (and   *
 * ranges) of the GRIB2 messages that should have diagnostic message    *
 * printed.                                                             *
 *                                                                      *
 *  Usage:                                                              *
 *      nadiag( g2diag, g2dglst, lstlen );                              *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *g2diag           char             input string for G2DIAG      *
 *      lstlen            int              Max number of grids          *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *g2dglst          int              list of messages.  Each      *
 *                                         element's position refers to *
 *                                         the GRIB2 message number in  *
 *                                         the file, and the contents   *
 *                                         of each element specifies the*
 *                                         Section flags that should be *
 *                                         printed.  If content = 0,    *
 *                                         indicates no diag output.    *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          12/2004                                     *
 ***********************************************************************/
{
    int    i, err, num;
    int    sections, *ptrtmp;
    const int    numstr=2;
    /*char   parts[numstr][LLMXLN];  */
    /*char   *part[numstr];          */
    char   parts[2][LLMXLN];
    char   *part[2];
    char   tmpstr[LLMXLN], *cd;

    /*
    **    Set up string locations for parsing orig string.
    */
    memset( parts, 0, numstr*LLMXLN);
    for (i=0; i<numstr; i++)  part[i] = parts[i];

    /*
    **    Separate string into two parts with "|" delimiter
    */
    cst_clst( g2diag, '|', "\0", numstr, LLMXLN, part, &num, &err );

    /*
    **    Initialize output array.
    */
    for (i=0; i<lstlen; i++)  g2dglst[i] = 0;

    if ( strlen(part[0]) == (size_t)0 ) { 
        return;        /*  exit if no diagnostic sections requested.  */
    }
    else {
        /*
        **    Extract list of GRIB2 sections, and set the section
        **    flags appropriately.
        */
        sections=0;
        cst_lcuc( part[0], tmpstr, &err );
        cd = strtok( tmpstr, ";" );
        while ( cd != NULL ) {
            if ( strncmp( cd, "ALL", 3 ) == 0 ) {
                sections = G2_IS | G2_IDS | G2_GDS | G2_PDS | G2_DRS | G2_BMS;
            }
            else if ( strncmp( cd, "IS", 2 ) == 0 )
                sections |= G2_IS;
            else if ( strncmp( cd, "IDS", 3 ) == 0 )
                sections |= G2_IDS;
            else if ( strncmp( cd, "GDS", 3 ) == 0 )
                sections |= G2_GDS;
            else if ( strncmp( cd, "PDS", 3 ) == 0 )
                sections |= G2_PDS;
            else if ( strncmp( cd, "DRS", 3 ) == 0 )
                sections |= G2_DRS;
            else if ( strncmp( cd, "BMS", 3 ) == 0 )
                sections |= G2_BMS;
            cd = strtok( NULL, ";" );
        }
    }

    /*
    **    If no specific message numbers are requested,
    **    set to print diagnostic info for all messages.
    */
    if ( strlen(part[1]) == (size_t)0 ) { 
        for (i=0; i<lstlen; i++)  g2dglst[i] = sections;
        return;
    }
    /*
    **    Otherwise, calculate the exact message numbers, and
    **    set appropriate array elements with the diag section flags..
    */
    else {
        ptrtmp = (int *)calloc(lstlen,sizeof(int));
        cst_ilst( part[1], ';', 0, lstlen, ptrtmp, &num, &err );
        for (i=0; i<num; i++)  {
            /*printf(" %d",ptrtmp[i]);*/
            g2dglst[ptrtmp[i]-1] = sections;
        }
        if ( ptrtmp != 0 ) free(ptrtmp);
    }
    
}

