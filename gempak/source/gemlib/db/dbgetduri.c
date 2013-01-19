#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_getduri ( char *queryType, char *source, char *model,
                  char *dattim, char *vcord, char *parm, int *level, 
                  char *dataURI, int *lDataURI, int *iret )
/************************************************************************
 *                                                                      *
 * db_getduri                                                           *
 *                                                                      *
 * This function returns data URI string from the AWIPS II DB           *
 * given certain data criteria.                                         *
 *                                                                      *
 * void db_getduri (char *queryType, char *source, char *model,         *
 *                char *dattim, char *vcord, char *parm, 		*
 *                int *level, char *dataURI, int *lDataURI, int *iret ) *
 *                                                                      *
 * Input parameters:                                                    *
 *      queryType       char            Type of query                   *
 *      source          char            Source of data                  *
 *      model           char            Model name                      *
 *      dattim          char            The data time                   *
 *      vcord           char            Vertical coordinate of the data *
 *      parm            char            Name of data parameter          *
 *      level           int             Level of the requested data     *
 *                                                                      *
 * Output parameters:                                                   *
 *      dataURI         char            Data URI                        *
 *      lDataURI        int             Length of dataURI string        *
 *      iret            int             Return code:                    *
 *                                        0 - normal return             *
 *                                       -1 - unable to get dataURI     *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS  04/11   Created                                 *
 ************************************************************************/
{
    int      bufferSize = 200000;
    char     xsltFile[LLSCRN] = "response.xlt";
    char     xsltDir[LLSCRN]  = "$NAWIPS/gempak/tables/xslt";
    int      ier, nbytes, ier1, ier2, iplgn = 129;
    long     flen;
    char     queryText[320], queryResult[bufferSize+1], xsltfile[FILE_FULLSZ];
    char     errStr[100], plgn[iplgn-1];
    unsigned char*   bigStr;
/*---------------------------------------------------------------------*/

   /*
    * Initialization
    */
    *iret = 0;
    dataURI[0] = '\0';
    queryText[0] = '\0';
    queryResult[0] = '\0';
    *lDataURI = 0;

   /*
    * Populate the query strings 
    */

   db_getparm ( plgn, &ier2,  iplgn);
   if ( ier2 != 0 ) {
      *iret = -1;
      return;
   }
   if (strcmp ( queryType, "dataURI" ) == 0 ) {
      if ( strcmp ( source, "GRID") == 0 ) {
         sprintf (ePlugIn,   "%s", plgn);
         sprintf (eSrc, "%s", source);
         sprintf (eMdl, "%s", model);
         sprintf (gDattim, "%s", dattim);
         sprintf (gCord, "%s", vcord);
         sprintf (gLevel, "%d", *level);
         sprintf (gParm, "%s", parm);
         sprintf (eLibClass, "%s", "GempakDataURIRequest");
      }
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
    
    sprintf (dataURI, "%s", bigStr);
    *lDataURI = strlen(dataURI);

    G_FREE( bigStr, unsigned char );
    return;

}
