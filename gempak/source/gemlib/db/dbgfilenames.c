#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_gFileNames ( const char *dir, struct dirent ***ret_namelist)
/************************************************************************
 *                                                                      *
 * db_gFileNames                                                      	*
 *                                                                      *
 * This function returns the number of entries in the AWIPS II database * 
 * matching certain criteria and the structure containing the names of 	*
 * these entries.  This function emulates the behavior of cfl_scandir.	*
 *                                                                      *
 * int db_gFileNames ( const char *dir, struct dirent ***ret_namelist)	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dir		char		list of criteria separated by \	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	***ret_namelist struct dirent   List of database entries	*
 *      db_gFileNames   int             Number of entries in namelist   *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS  01/10   Created                                 *
 * m.gamazaychikov/CWS  04/11   Removed the NMAP bridge code		*
 ************************************************************************/
{
    int      bufferSize       = 20000;
    int      max_dir_layer    = 13;
    int      max_layer_length = 50;
    int      num_responses    = 1000;
    int      response_length  = 200;
    char     queryType[11]    = "flName";
    char     xsltFile[LLSCRN] = "response.xlt";
    int      ii, jj, ier, nbytes, istmax, ipos, ier1, lstr, lstr2;
    int      used, allocated, len, ier2, icycle=25;
    char     queryText[320], queryResult[bufferSize+1], respstr[bufferSize+1];
    char      **starr;
    char     prefix[20], cycle[25];
    unsigned char*   bigStr;
    struct dirent *entry=NULL, *entry2=NULL;
    struct dirent **namelist = NULL;

/*---------------------------------------------------------------------*/

   /*
    * Initialize the strings
    */
    respstr[0]  = '\0';
    queryText[0] = '\0';
    queryResult[0]  = '\0';
    prefix[0] = '\0';
  
   /*
    * Break the dir string into parts
    */
    starr = (char **)malloc((size_t)max_dir_layer * sizeof(char *));
    for( jj=0; jj < max_dir_layer; jj++ )
       starr[jj] = (char *)malloc( max_layer_length * sizeof(char));
    cst_clst (dir, '/', " ", max_dir_layer, max_layer_length, starr, &istmax, &ier);

   /*
    * istmax == 1 -- the function is being called from grid diagnostics
    */
    if ( istmax == 1 ) {
      /*
       * Get the current time
       */
       db_getcycle ( cycle, &ier2,  icycle);
      /*
       sprintf (gDattim, "%s", "090701/1300F003");
       sprintf (gCycle, "%s", "100729/0000f001");
       sprintf (gCycle, "%s", "101108/0000f000");
      */
       sprintf (gDattim, "%s", cycle);
       sprintf (gCycle, "%s", cycle);
      /*
       * Populate the query strings
       */   
       cst_clst (dir, '_', " ", max_dir_layer, max_layer_length, starr, &istmax, &ier);
       sprintf (eSrc, "%s", "GRID");
      /*
       * need to get ncgrib cycle times
       */
       sprintf (ePlugIn, "%s", "ncgrib");
       sprintf (eLibClass, "%s", "GempakGridCycleQuery");
       sprintf (eParameters, "%s", starr[0]);
       strcat  (eParameters, "|");
       strcat  (eParameters, gCycle);
       sprintf (prefix, "%s", starr[0]);
       strcat  (prefix, "_");
       strcat  (prefix, starr[1]);
    }
   /*
    * istmax <=0  -- error
    */
    else {
       for ( jj = 0; jj < max_dir_layer; jj++ ) free( starr[jj] );
       if( starr ) free( (char **)starr );
       return (-1);
    }

 
    for ( jj = 0; jj < max_dir_layer; jj++ ) free( starr[jj] );
    if( starr ) free( (char **)starr );

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
       return(-1);
    }

   /*
    * Connect to database and get the query result
    */
    db_runQuery ( queryText, queryResult, &ier);
    if ( ier !=0 ) {
      /*
       * Query was run unsuccessfully -> returning
       */ 
       ier = -4;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       return(-1);
    }

   /*
    * Transform XML string to a string containing list of 'files'
    */
    nbytes=xml_transform( queryResult, strlen(queryResult), xsltFile, &bigStr, &ier );
    if ( ier !=0 || nbytes==0) {
      /* 
       * XML Transform run unsuccessfully -> returning
       */
       ier = -5;
       er_wmsg ( "DB", &ier, xsltFile, &ier1, 2, strlen(xsltFile) );
       G_FREE( bigStr, unsigned char );
       return(-1);
    }

    sprintf (respstr, "%s", bigStr);
    G_FREE( bigStr, unsigned char );

   /*
    * Change all the spaces in the string to underscores
    */
    cst_rspc ( respstr, &ier);

   /*
    * Break the respStr string into parts each representing 'file'
    */
    starr = (char **)malloc((size_t)num_responses * sizeof(char *));
    istmax = 0;
    for( jj=0; jj < num_responses; jj++ )
       starr[jj] = (char *)malloc( response_length * sizeof(char));
    cst_clst (respstr, '|', " ", num_responses, response_length, starr, &istmax, &ier);

   /*
    * Fake scandir function to return back namelist structure
    */
    used = 0;
    allocated = 2;
    namelist = malloc(allocated * sizeof(struct dirent *));

    entry = (struct dirent *) malloc (sizeof(struct dirent));
    for  ( ii = 0; ii < istmax; ii++ ) {
       cst_nocc ( starr[ii], '/', 1, 0, &ipos,  &ier );
       if ( strlen (prefix) > 0 ) {
          sprintf ( entry->d_name, "%s", prefix);
          strcat  ( entry->d_name, "-20");
          //sprintf ( entry->d_name, "%s", "20");
       }
       else {
          sprintf ( entry->d_name, "%s", "20");
       }
       strncat ( entry->d_name, starr[ii], ipos);
       
       strcat  ( entry->d_name, "_");
       cst_lstr (starr[ii], &lstr, &ier);
       lstr2 = lstr - ipos - 1;
       strncat ( entry->d_name, starr[ii]+ipos+1, lstr2);
       len = offsetof(struct dirent, d_name) + strlen(entry->d_name) + 1;
       if ((entry2 = malloc(len)) == NULL) {
          for ( jj = 0; jj < num_responses; jj++ ) free( starr[jj] );
          if( starr ) free( (char **)starr );
          free (entry);
          ier = -7;
          er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
          return(-1);
       }
       if (used >= allocated) {
            allocated *= 2;
            namelist = realloc(namelist, allocated * sizeof(struct dirent *));
            if (!namelist) {
               for ( jj = 0; jj < num_responses; jj++ ) free( starr[jj] );
               if( starr ) free( (char **)starr );
               free (entry);
               ier = -7;
               er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
               return(-1);
            }
       }
       memcpy(entry2, entry, len);
       namelist[used++] = entry2;
    }

    free (entry);
    for ( jj = 0; jj < num_responses; jj++ ) free( starr[jj] );
    if( starr ) free( (char **)starr );

    *ret_namelist = namelist;
    return(istmax);

}
