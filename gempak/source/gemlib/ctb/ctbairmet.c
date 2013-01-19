#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

#define AIRMET_TABLE 		"airmetcycle.tbl"


/************************************************************************
 * ctbgetairmet.c                                                       *
 *                                                                      *
 * This module contains the subroutines to read in and retrieve airmet  *
 * cycle information from the airmetcycle table.    		        *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *   public functions:                                                  *
 *	ctb_getAirmetIssueTm	get the issue time for a given cycle	*
 *	ctb_getAirmetCycleTms	get all available cycle times          	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/05	initial coding                          *
 * S. Chiswell/Unidata	06/06	Added cfl_clos calls			*
 ***********************************************************************/

   /*
    * Private functions
    */

   /*
    * Global variables
    */

/*======================================================================*/


void ctb_airmetGetIssueTm( char *cycleTm, char *issueTm, int *iret )
/************************************************************************
 * ctb_airmetGetIssueTm                                                 *
 *                                                                      *
 * This routine reads the airmetcycle.tbl and finds the issue time that	*
 * corresponds to the input cycle time.					*
 *                                                                      *
 * void ctb_airmetGetIssueTm( *cycleTm, *issueTm, *iret )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*cycleTm	char		cycle time			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*issueTm	char		issue time			*
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                      = -1: no table/data             *
 *					= -2: no match on cycleTm	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/05	Initial coding 	                        *
 ***********************************************************************/
{
    int 	ii, bufsize; 
    int 	row = 0, ier = 0, cycleSz = LLBSIZ, issueSz = LLBSIZ;

    char        oneLine [ LLBSIZ ], field1[ LLBSIZ ];
    char	field2[ LLBSIZ ], def[5]="";
    char	cycle[] = "cycle", cycleVal[ LLBSIZ ];
    char	issue[] = "issue", issueVal[ LLBSIZ ];
    FILE        *tblFile = NULL;
/*----------------------------------------------------------------------*/

    *iret = -2;
    issueTm[0] = '\0';

    /*
     * Open the gfa gui table file
     */
    if ( ( tblFile = cfl_tbop ( AIRMET_TABLE, "pgen", &ier ) ) != NULL ) {
       cfl_tbnr ( tblFile, &row, &ier );
    }

    if ( !tblFile || row == 0 || ier != 0 ) {
       if ( tblFile ) cfl_clos ( tblFile, &ier );
       *iret = -1;
       return;
    }

    /*
     * Read each line until we match the cycleTm or run out of lines.
     */
    bufsize = sizeof ( oneLine );

    for ( ii = 0; ii < row; ii++ ) {

        cfl_trln ( tblFile, bufsize, oneLine, &ier );

	cst_gtag( cycle, oneLine, def, field1, &ier );
        cst_rmbl( field1, cycleVal, &cycleSz, &ier );

	/*
	 *  If we match the cycleTm then get the issue time from the row.
	 */
	if( strcmp( cycleVal, cycleTm ) == 0 ) {
	    cst_gtag( issue, oneLine, def, field2, &ier );
            cst_rmbl( field2, issueVal, &issueSz, &ier );
	    strcpy( issueTm, issueVal );
            *iret = 0;
	}

    }

    cfl_clos ( tblFile, &ier );
}

/*======================================================================*/

void ctb_airmetGetCycleTms( Boolean isDST, int *ntimes, char ***times, int *iret )
/************************************************************************
 * ctb_airmetGetCycleTms                                                *
 *                                                                      *
 * This routine returns all available cycle times for a given time      *
 * setting (CST or CDT).                      				*
 *                                                                      *
 * void ctb_airmetGetCycleTms ( isDST, *ntimes, **times, *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	isDST		Boolean	CDT (true) or CST (false)		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ntimes		int	number of times returned       		*
 *	**times		char	allocated array of returned times	*
 *	*iret		int	return code                             *
 *                             	  0 = normal				*
 *				 -1 = unable to open/find table		* 
 *				 -2 = malloc failed			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/05	Initial coding 	                        *
 ***********************************************************************/
{ 
    int 	ii, bufsize; 
    int 	row = 0, ier = 0, cycleSz = LLBSIZ;

    char        oneLine [ LLBSIZ ], field1[ LLBSIZ ];
    char	field2[ LLBSIZ ], def[5]="";
    char	timeSet[4];
    char	setting[] = "setting", settingVal[ LLBSIZ ];
    char	cycle[] = "cycle", cycleVal[ LLBSIZ ];

    FILE        *tblFile = NULL;
/*----------------------------------------------------------------------*/

    *iret = 0;
    *times = NULL;
    *ntimes = 0;

    if( isDST ) {
       strcpy( timeSet, "CDT" );
    }
    else {
       strcpy( timeSet, "CST" );
    }


    /*
     * Open the gfa gui table file
     */
    if ( ( tblFile = cfl_tbop ( AIRMET_TABLE, "pgen", &ier ) ) != NULL ) {
       cfl_tbnr ( tblFile, &row, &ier );
    }

    if ( !tblFile || row == 0 || ier != 0 ) {
       if ( tblFile ) cfl_clos ( tblFile, &ier );
       *iret = -1;
       return;
    }

    /*
     * Read each line.  Look for all matches on the setting value.
     * The *ntimes param is used to count the number of matches.
     *
     * Note that (*times) is allocated to the max possible value (row),
     * rather than the actual number of matches (likely row/2).  Since
     * the number of rows will likely be less than 10 this isn't a big 
     * inefficiency.  The alternative is to read the table twice. 
     */
    bufsize = sizeof ( oneLine );

    for ( ii = 0; ii < row; ii++ ) {

        cfl_trln ( tblFile, bufsize, oneLine, &ier );

	cst_gtag( setting, oneLine, def, field1, &ier );
        cst_rmbl( field1, settingVal, &cycleSz, &ier );

	if( strcmp( settingVal, timeSet ) == 0 ) {

	    if( (*times) == NULL ) {
		(*times) = (char **)malloc( row * sizeof( char * ) );
                                      
		if( (*times) == NULL ) {
                    cfl_clos ( tblFile, &ier );
		    *iret = -2;
		    return;
		}
	    }

	    cst_gtag( cycle, oneLine, def, field2, &ier );
            cst_rmbl( field2, cycleVal, &cycleSz, &ier );

	    (*times)[*ntimes] = (char *)malloc( ( strlen( cycleVal ) + 1 ) * 
	    						sizeof( char ) );
	    if( (*times)[*ntimes] == NULL ) {
		*iret = -2;
		return;
	    }

            sprintf( (*times)[ *ntimes ], "%s", cycleVal );
	    *ntimes = *ntimes + 1;
	}
    }

    cfl_clos ( tblFile, &ier );
}

/*======================================================================*/

