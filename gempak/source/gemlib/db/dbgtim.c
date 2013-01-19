#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_gtim ( char *queryType, char *source, char *qparms, 
               char *times, int *ltimes, int *iret )
/************************************************************************
 *                                                                      *
 * db_gtim               						*
 *                                                                      *
 * This function returns the available times in the AWIPS II database   *
 * for a data source with some parameters.				*
 *                                                                      *
 * int db_gtim ( char *queryType, char *source, char *qparms,		*
 *                     char *times, int *ltimes, int *iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      queryType       char            Type of query                   *
 *      source          char            Source of data                  *
 *      qparms          char            Additional parameters		*
 *                                                                      *
 * Output parameters:                                                   *
 *      times           char            Data times string          	*
 *      ltimes          int             Length of times string          *
 *      iret            int             Return code:                    *
 *                                        0 - normal return             *
 *                                       -1 - unable to get times	*
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS  04/11   Created                                 *
 ************************************************************************/
{
    int      bufferSize = 200000;
    char     xsltFile[LLSCRN] = "response.xlt";
    char     xsltDir[LLSCRN]  = "$NAWIPS/gempak/tables/xslt";
    int      ier, nbytes, ier1, ier2, iparm = 129;
    long     flen;
    char     queryText[320], queryResult[bufferSize+1];
    char     errStr[100], xsltfile[FILE_FULLSZ], parm[iparm-1];
    unsigned char*   bigStr;
/*---------------------------------------------------------------------*/

   /*
    * Initialization
    */
    *iret = 0;
    times[0] = '\0';
    queryText[0] = '\0';
    queryResult[0] = '\0';

    db_getparm ( parm, &ier2,  iparm);
    if ( ier2 != 0 ) {
      *iret = -1;
      return;
    }

   /*
    * Populate the query strings 
    */
    if (strcmp ( queryType, "dbTime" ) == 0 ) {
       strcpy (eSrc, source);
       if ( strcmp ( source, "GRID") == 0 ) {
         sprintf (ePlugIn,   "%s", parm);
         sprintf (eGrid,   "%s", qparms);
         sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
       }
/*
       else if ( strcmp ( source, "METAR") == 0 ) {
          sprintf (ePlugIn,   "%s", "obs");
          sprintf (eLibClass, "%s", "NomTimeQuery");
       }
       else if ( strcmp ( source, "BUFRUA") == 0 ) {
          sprintf (ePlugIn,   "%s", "bufrua");
          sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
       }
       else if ( strcmp ( source, "SYNOP") == 0 ) {
          sprintf (ePlugIn,   "%s", "sfcobs");
          sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
       }
*/
       else {
         ier = -9;
         sprintf (errStr, "%s+", queryType);
         strcat  (errStr, source);
         er_wmsg ( "DB", &ier, errStr, &ier1, 2, strlen(errStr) );
         *iret = -1;
         return;
       }
     }
     else {
       ier = -8;
       er_wmsg ( "DB", &ier, queryType, &ier1, 2, strlen(queryType) );
       *iret = -1;
       return;
     }

    /*
    * Get the query text
    */
    db_getQueryText ( queryType, queryText, &ier);

    if ( ier !=0 ) {
      /*
       * Query text not set -> returning
       */
       ier = -3;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

   /*
    * Connect to database and get the query result
    */
    db_runQuery ( queryText, queryResult, &ier);

    if ( ier !=0 ) {
       ier = -4; 
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }
   
   /*
    * Transform XML string to a string containing list of header parameters
    */
    cfl_inqr(xsltFile, xsltDir, &flen, xsltfile, &ier);
    if ( ier !=0 ) {
      /* 
       * XSLT file not found -> returning
       */
       ier = -5;
       er_wmsg ( "DB", &ier, xsltFile, &ier1, 2, strlen(xsltFile) );
       *iret = -1;
       return;
    }

    nbytes=xml_transform( queryResult, strlen(queryResult), xsltfile, &bigStr, &ier );

    if ( ier !=0 || nbytes==0) {
      /* 
       * XML Transform run unsuccessfully -> returning
       */
       ier = -6;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       G_FREE( bigStr, unsigned char );
       *iret = -1;
       return;
    }

    sprintf (times, "%s", bigStr);

    *ltimes = strlen(times);

    G_FREE( bigStr, unsigned char );
    return;

}
