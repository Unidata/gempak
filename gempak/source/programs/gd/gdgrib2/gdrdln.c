#include "gdgrib2.h"

void gd_rdln( GDG2_input *input, FILE **convPtr, int *iret )
/************************************************************************
 * gdrdln                                                               *
 *                                                                      *
 * This routine reads one line from the conversion table and parses it  *
 * to gdgrib2 input parameters.                                         *
 *                                                                      *
 *  Input Arguments:                                                    *
 *    **convPtr     FILE                Conversiton table               *
 *                                                                      *
 *  Output Arguments:                                                   *
 *    *input        GDG2_input          Structure to hold user input    *
 *    *iret         int                 Error return                    *
 *                                      0  = Successfull                *
 *                                      1  = Reach conversion table end *
 *                                      -2 = Error getting one or more  *
 *                                           variables                  *
 *                                     -35 = Error reading conversion   *     
 *                                           table                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/ERT          09/2015         Initial creation                 *
 * B. Yin/ERT          11/2015         Changed convPtr from * to **     *
 ***********************************************************************/
{
    int len, ire;
    char line[MAX_CHAR_PER_LINE + 1];

    /*
     * Read one line from the conversion table
     */
    cfl_rdln( *convPtr, sizeof(line), line, &ire );
        
    if ( ire == 0 ) {
        /*
         * Parse the line and put the parameters into input 
         */
        gdparseparm( input, line, &ire );
        if ( ire != 0 ) {
            *iret = -2;
        }  
        else {
            *iret = 0;
        }
    }
    else if ( ire == 4 ) {
        /*
         * Reach the end of the conversion table      
         */ 
        cfl_clos( *convPtr, &ire );
        *convPtr = 0;
        *iret = 1;

    }
    else if ( ire != 0 ) {
        /*
         * Error reading the conversion table      
         */ 
        cfl_clos( *convPtr, &ire );
        *convPtr = 0;
        *iret = -35;
    }
 
    return;
}
