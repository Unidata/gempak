#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"


int main ( int argc, char *argv[] )
/************************************************************************
 * tcacancel                                                            *
 *                                                                      *
 * This program cancels all tropical cyclone watches and warnings, and	*
 * write the final cancellation TCV("Tropical Cyclone VTEC") message.	*
 *                                                                      *
 * command line:                                                        *
 *      tcacancel bbss aaa st dattim timezone                        	*
 *		bbss	    bb = basin, ss = storm number		*
 *		aaa	    advisory number for cancellation 		*
 *		st	    storm type - HU, TS, TD, SS, or SD		*
 *		dattim	    GEMPAK date/time string                     *
 *		timezone    local time zone for cancellation		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC	    03/05	Created					*	
 * M. Li/SAIC	    05/05	errgrp[8] -> errgrp[10]			*
 * S. Gilbert/NCEP    01/06	Modified to account for intermediate    *
 *                              advisories                              *
 ***********************************************************************/
{
    int 	iadv, iaflag, ipos, leverr, timarr[5], found;
    int 	ier, numerr, pagflg;
    char        *STlist[] = { "HU", "TS", "TD", "SS","SD" };
    char	*time_zone[] = { "AST", "EST", "EDT", "CST", "CDT", "PST", "PDT" };
    char 	dattim[40], advstr [10], stmtyp[10], timezn[10], bbss[10];
    char	fname[256], errgrp[10], cc[50];
    Boolean     readflg; 
    FILE 	*ifpout;

/*---------------------------------------------------------------------*/

    leverr  = 0;
    readflg = True;
    ifpout  = NULL;
    strcpy ( errgrp, "TCACANCEL" );
    strcpy ( cc, " " );

   /*
    * Check the command line arguments, print help and exit if too few arguments.
    */

    if ( argc < 6 ) {
        pagflg = G_FALSE;
        ip_help ( errgrp, &pagflg, &ier, 
                  strlen(errgrp) );
        numerr = -1;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /*
    * Check the advisory number.
    */

    strcpy ( advstr, argv[2] );
    gh_advn ( advstr, &iadv, &iaflag, &ier );
    if ( iadv < 2 ) {
	numerr = -2;
 	er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /* 
    * Check for valid GEMPAK time.
    */

    strcpy ( dattim, argv[4] );
    ti_ctoi ( dattim, timarr, &ier, strlen(dattim) );
    if ( ier != 0 ) {
   	numerr = -3;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /*
    * Check for valid storm type.
    */

    strcpy ( stmtyp, argv[3] );
    cst_lcuc ( stmtyp, stmtyp, &ier );
    cst_find ( stmtyp, (const char **)STlist, 5, &ipos, &ier );
    if ( ipos < 0 ) {
	numerr = -4;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /*
    * Check for valid time zone.
    */

    strcpy ( timezn, argv[5] );
    cst_lcuc ( timezn, timezn, &ier );
    cst_find ( timezn, (const char **)time_zone, 7, &ipos, &ier );
    if ( ipos < 0 ) {
        numerr = -5;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /*
    * Construct the breakpoint VG file name for the previous advisory.
    */

    if ( argv[1] != NULL ) {
	cst_uclc ( argv[1], bbss, &ier );

        found = 0;
        switch ( iaflag ) {
 	
        case 0:       /*   Regular advisory  */
	    sprintf ( fname, "tca_%s%d_%03db.vgf", bbss, timarr[0], iadv-1 );
            if ( access ( fname, F_OK ) == 0 ) found = 1;
            if ( found == 0 ) {
	        sprintf ( fname, "tca_%s%d_%03da.vgf", bbss, timarr[0], iadv-1);
                if ( access ( fname, F_OK ) == 0 ) found = 1;
            }
            if ( found == 0 ) {
	        sprintf ( fname, "tca_%s%d_%03d.vgf", bbss, timarr[0], iadv-1 );
                if ( access ( fname, F_OK ) == 0 ) found = 1;
            }
            break;
                                                                                
        case 1:       /*   1st intermediate advisory  */
	    sprintf ( fname, "tca_%s%d_%03d.vgf", bbss, timarr[0], iadv );
            if ( access ( fname, F_OK ) == 0 ) found = 1;
            break;
                                                                                
        case 2:       /*   2nd intermediate advisory  */
	    sprintf ( fname, "tca_%s%d_%03da.vgf", bbss, timarr[0], iadv );
            if ( access ( fname, F_OK ) == 0 ) found = 1;
            if ( found == 0 ) {
	        sprintf ( fname, "tca_%s%d_%03d.vgf", bbss, timarr[0], iadv );
                if ( access ( fname, F_OK ) == 0 ) found = 1;
            }
            break;
                                                                                
        default:
            found = 0;
            break;
        }

    }
    else {
	numerr = -2;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

    if ( found == 0 ) {
        numerr = -2;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }
	
   /*
    * Open the VG file.
    */

    ifpout = cas_open ( fname, readflg, &ier );
    if ( ier != 0 ) {
	numerr = -2;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

   /*
    * Create cancellation text message.
    */

    gh_tctx ( "", fname, stmtyp, dattim, timezn ); 

   /*
    * Close the VF file.
    */

    cas_clos ( ifpout, &ier );

    return(0);

}
