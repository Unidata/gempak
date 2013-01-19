#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_runQuery ( char *queryText, char *queryResult, int *iret )
/************************************************************************
 *									*
 * db_runQuery               						*
 *                                                                      *
 * This function sends a query, specified in queryText to the AWISP II 	* 
 * end point and returns the result of the query in the queryResult	*
 * string.								*
 *                                                                      *
 * void db_runQuery ( char *queryText, char *queryResult, int *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *queryText      char            Text of the query               *
 *                                                                      *
 * Output parameters:                                                   *
 *      *queryResult	char            Query result			*
 *      *iret           int             return code:                    *
 *                                      -1: query run unsuccessfully	*
 *                                       0: normal                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS	01/10	Created                                 *
 * m.gamazaychikov/CWS  04/10   Changed the python user script handler  *
 *                              utility from curl to uengine CLI        *
 * m.gamazaychikov/CWS  04/11	Removed dbHost from the CS		*
 ************************************************************************/
{
char headcmd[128], popncmd[512], tailcmd[50], *errorReturn = NULL;
int  queryResultSize = 20000;
int  chars_read=0 , ier, ier1;
FILE *read_fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

   /*
    * command to direct string to standard input
    */
    sprintf (headcmd, "%s", "echo");

   /*
    * Create a string that invokes the AWIPS II uEngine CLI untility
    */
    sprintf (tailcmd, "%s", " | uengine -r python -m");

   /*
    * Create the string for the open pipe
    */
    sprintf (popncmd, "%s \'%s\' %s", headcmd, queryText, tailcmd);

   /*
    * Initialize the queryResult string, open the pipe, 
    * and read data from the pipe
    */
    memset (queryResult, '\0', sizeof(queryResult));
    read_fp = popen(popncmd, "r");
    if ( read_fp != NULL ) { 
       chars_read = fread( queryResult, sizeof( char ), 
                           queryResultSize, read_fp );
            while (chars_read > 0) {
                queryResult[chars_read - 1] = '\0';
                chars_read = fread(queryResult, sizeof(char), 
                                   queryResultSize, read_fp);
            } 

       pclose(read_fp);
    }
    else {
       ier = -11;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

    errorReturn = strstr (queryResult, "error");

    if ( errorReturn != NULL ) {
       ier = -12;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }
    return;
}
