#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_rdtr ( char *queryType, char *source, char *part, char *dattim,  
               char *stid, char *dataUri, int *icnt, float *rdata, int *nword, 
               int *iret )
/************************************************************************
 *									*
 * db_rdtr               						*
 *									*
 * This function returns array of data values from the AWIPS II DB      *
 * given certain data criteria.                                         *
 *                                                                      *
 * void db_rdtr ( char *queryType, char *source, char *part,		*
 *                char *dattim, char *stid, char *dataUri, int *icnt,	*
 *                float *rdata, int *nword, int *iret ) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *      queryType       char            Type of query                   *
 *      source          char            Source of data                  *
 *      part            char            Part name                       *
 *      dattim          char            The data time                   *
 *      stid            char            Station ID			*
 *      dataUri         char            Data URI                        *
 *      icnt            int             Number of stations (obsolete)	*
 *                                                                      *
 * Output parameters:                                                   *
 *      rdata           float           Array of data values		*
 *      nword           float           Length of rdata array		*
 *      iret            int             Return code:                    *
 *                                        0 - normal return             *
 *                                       -1 - unable to get dataURI     *
 *                                                                      *
 **                                                                     *
 * Log:									*
 * m.gamazaychikov/CWS	04/11	Created                                 *
 ************************************************************************/
{
    int      numberPayloadParts = 2;
    int      payloadPartLength = 50;
    int      bufferSize = 200000;
    char     xsltFile[LLSCRN] = "response.xlt";
    char     xsltDir[LLSCRN]  = "$NAWIPS/gempak/tables/xslt";
    int      ier, nbytes, ier1, nwrd, istmax, jj;
    long     flen;
    char     queryText[320], queryResult[bufferSize+1], xsltfile[FILE_FULLSZ];
    char     errStr[100], payload[100];
    unsigned char*   bigStr;
    char     **starr;
/*---------------------------------------------------------------------*/

   /*
    * Initialization
    */
    *iret = 0;
    queryText[0] = '\0';
    queryResult[0] = '\0';

   /*
    * Populate the query strings 
    */

   if (strcmp ( queryType, "gridDat" ) == 0 ) {
      if ( strcmp ( source, "GRID") == 0 ) {
         strcpy (eSrc, source);
         sprintf (eDistnctField, "%s", dataUri);
         sprintf (gDattim, "%s", dattim);
         eCount = 1;
         sprintf (eLibClass, "%s", "GempakGridLinkRequest");
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
    * Transform XML string to a string containing the name of the file 
    * stored on the server side
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

    sprintf( payload, "%s", bigStr );
    G_FREE( bigStr, unsigned char );
   /*
    * Break the payload string into parts containing the dbHost and fileName
    */
    starr = (char **)malloc((size_t)numberPayloadParts * sizeof(char *));
    for( jj=0; jj < numberPayloadParts; jj++ )
       starr[jj] = (char *)malloc( payloadPartLength * sizeof(char));
    cst_clst (payload, '|', " ", numberPayloadParts, payloadPartLength, starr, &istmax, &ier);
    if ( ier !=0 || istmax != 2 ) {
       ier = -13;
       er_wmsg ( "DB", &ier, source, &ier1, 2, strlen(source) );
       *iret = -1;
       for ( jj = 0; jj < numberPayloadParts; jj++ ) free( starr[jj] );
       if( starr ) free( (char **)starr );
       return;
    }

    
   /*
    * Get the data from the server side
    */
    db_getRData(starr[0], starr[1], rdata, &nwrd, &ier);
    for ( jj = 0; jj < numberPayloadParts; jj++ ) free( starr[jj] );
    if( starr ) free( (char **)starr );
    *nword = nwrd;
    if ( ier !=0 ) {
       if ( ier == -1 ) {
          ier = -10;
          er_wmsg ( "DB", &ier, source, &ier1, 2, strlen(source) );
          *iret = -1;
          return;
       }
       if ( ier == -2 ) {
          ier = -15;
          er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
          *iret = -1;
          return;
       }
    }
    return;

}
