#include "gdgrib2.h"
#include "proto_dg.h"

void gdparseparm ( GDG2_input *input, char *line, int *iret )
/************************************************************************
 * gdparseparm                                                          *
 *                                                                      *
 * This routine parses a inputstring and set the parameters in input.   *
 *                                                                      *
 *  Input Arguments:                                                    *
 *    *line        char                 A string to parse               *
 *                                                                      *
 *  Output Arguments:                                                   *
 *    *input        GDG2_input          Structure to hold parameters    *
 *    *iret         int                 Error return                    *
 *                                      0  = Successfull                *
 *                                      -2 = Error getting one or more  *
 *                                           variables                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/ERT                9/2015   Initial creation                  *
 ***********************************************************************/
{
    int ier;

    char *key, *value, *ptr, paddedKey[VAR_LEN+1], convTbl[LLMXLN];
    int  ii, num, varlen, len;
    const int numtbls = NUM_TBLS;

/*---------------------------------------------------------------------*/

    /* 
     * Initialization
     */
    *iret = 0;
    varlen = VAR_LEN;

    strcpy( convTbl, input->g2conv );
    memset( input, 0, sizeof (GDG2_input) );
    strcpy( input->g2conv, convTbl );

    /*
     * Parse the input line
     */  
    cst_rxbl( line, line, &len, &ier );   
    *iret = ier;
    ptr = strtok( line, " " );

    while ( (ptr != NULL) && (*iret == 0)) {

        key = strsep( &ptr, "=" );
        value = ptr;

        if ( key != NULL ) {

	   cst_lcuc( key, key, &ier );
           *iret = ier;
  	   cst_padString( key, ' ' , 1, VAR_LEN, paddedKey ); 

	   if ( strncmp( paddedKey, gdfile, varlen ) == 0 ) {
              strcpy( input->gdfile, value );
              st_null( input->gdfile, input->gdfile, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
	   }
	   else if ( strncmp( paddedKey, g2file, varlen ) == 0 ) {
              strcpy( input->g2file, value );
              st_null( input->g2file, input->g2file, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, gfunc, varlen ) == 0 ) {
              strcpy( input->gfunc, value );
              st_null( input->gfunc, input->gfunc, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, gdattim, varlen ) == 0 ) {
              strcpy( input->gdattim, value);
              st_null( input->gdattim, input->gdattim, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, glevel, varlen ) == 0 ) {
              strcpy( input->glevel, value );
              st_null( input->glevel, input->glevel, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, gvcord, varlen ) == 0 ) {
              strcpy( input->gvcord, value );
              st_null( input->gvcord, input->gvcord, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
 	   else if ( strncmp( paddedKey, proj, varlen ) == 0 ) {
              strcpy( input->proj, value);
              st_null( input->proj, input->proj, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, kxky, varlen ) == 0 ) {
              strcpy( input->kxky, value );
              st_null( input->kxky, input->kxky, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
	   else if ( strncmp( paddedKey, grdarea, varlen ) == 0 ) {
              strcpy( input->grdarea, value );
              st_null( input->grdarea, input->grdarea, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
           else if ( strncmp( paddedKey, cpyfil, varlen ) == 0 ) {
              strcpy( input->cpyfil, value );
              st_null( input->cpyfil, input->cpyfil, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
           }
  	   else if ( strncmp( paddedKey, g2tbls, varlen ) == 0 ) {
              strcpy( input->g2tbls, value );
              st_null(input->g2tbls, input->g2tbls, &len, &ier, LLMXLN, LLMXLN );
              *iret += ier;
            }   
  	    else if ( strncmp( paddedKey, g2is, varlen ) == 0 ) {
               strcpy( input->g2is, value );
               st_null( input->g2is, input->g2is, &len, &ier, LLMXLN, LLMXLN );
               *iret += ier;
            }
 	    else if ( strncmp( paddedKey, g2ids, varlen ) == 0 ) {
               strcpy( input->g2ids, value );
               st_null(input->g2ids, input->g2ids, &len, &ier, LLMXLN, LLMXLN );
               *iret += ier;
            }
	    else if ( strncmp( paddedKey, g2pdt, varlen ) == 0 ) {
               strcpy( input->g2pdt, value );
               st_null( input->g2pdt, input->g2pdt, &len, &ier, LLMXLN, LLMXLN );
               *iret += ier;
            }
	    else if ( strncmp( paddedKey, g2drt, varlen ) == 0 ) {
               strcpy( input->g2drt, value );
               st_null( input->g2drt, input->g2drt, &len, &ier, LLMXLN, LLMXLN );
               *iret += ier;
            }
	    else if ( strncmp( paddedKey, wmohdr, varlen ) == 0 ) {
               strcpy( input->wmohdr, value );
               st_null(input->wmohdr, input->wmohdr, &len, &ier, LLMXLN, LLMXLN );
               *iret += ier;
            }

        }
 
        ptr = strtok( NULL, " " );

    } // while loop

    if ( *iret == 0 ) {
        /*
         * Initialize tables
         */ 
        for ( ii = 0; ii < numtbls; ii++ ) {
            input->tbllist[ii] = input->tables[ii];
        } 
        if ( strlen( input->g2tbls ) != 0 ) {
            cst_clst( input->g2tbls, ';', "\0", numtbls, LLMXLN, input->tbllist,
                      &num, &ier );
            *iret = ier;
        } 
    }
  
    if ( *iret != 0 ) {
       *iret = -2;
    }

    return;
}
