#include "gdgrib2.h"
#include "proto_dg.h"

int main ( void )
/************************************************************************
 * GDGRIB2                                                              *
 *                                                                      *
 * This program encodes a selected GEMPAK grid into a GRIB2 message.    *
 *                                                                      *
 *  Command line:                                                       *
 *  gdgrib2                                                             *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           5/2005   Orig                              *
 * S. Gilbert/NCEP           3/2006   Replaced dg_clal with dg_nend     *
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * B. Yin/ERT           09/15   Added bulk processing from table        *
 ***********************************************************************/
{

    int    respond, ret, ier, mode=1, done, skip, j;

    int    g2len;                     /* Length of GRIB2 message  */
    unsigned char  *g2msg;            /* GRIB2 message            */
    char  cur_gbfile[LLMXLN]="";
    char  chdr[22];          /* WMO Header               */
    FILE  *gbfptr=0;

    GDG2_input    input;                  /* user input variables */
    GDG2_gemgrid  gemgrid;                /* GEMPAK grid and info */
/*---------------------------------------------------------------------*/
/*
 *  Initialize TAE. 
 */
    ip_init ( &respond, &ret );
    if ( ret == 0 ) {
	ip_idnt ( "GDGRIB2", &done, 7 );

/*
 *  Initialize GEMPLT.
 */
	gg_init ( &mode, &ret );
	if ( ret == 0 ) {

/*
 *  Initialize grid library common area grdcmn.cmn
 */
	    gd_init ( &ier );

/*
 *  Initialize the DG library
 */
	    dg_intl ( &ret );
	    done = 0;
	    skip = 1;
	}
	else {
	    done = 1;
	}
    }
    else {
	done = 1;
    }

/*
 *  Process next request, if user has one.
 */
    while ( done == 0 ) {

/*
 *  Wait for user input, if not first time through this loop
 */
        if ( skip == 0 ) ip_dynm( &done, &ret );
        skip = 0;
        if ( done != 0 ) break;          /*  Exit out of interactive loop  */

/*
 *  Get user input info
 */
        gdg2in( &input, &ret );

        if ( ret != 0 ) {
            er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
            continue;
        }
        else if ((strlen(input.g2conv) != (size_t)0) && (ret == 0)) {
/*
 *  Skip program prompt and get input from conversion table 
 */
           skip = 1;
        }

        if ( strlen(input.g2file) == (size_t)0 ) {
            ret=-28;
            er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
            continue;
        }

/*
 *  Get requested grid
 */
        gdgetgrid( &input, &gemgrid, &ret );
        if ( ret != 0 ) {
            er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
            continue;
        }

/*
 *  Make GRIB2 field
 */
        gdmakeg2( &input, &gemgrid, &g2msg, &g2len, &ret );
        if ( ret != 0 ) {
            er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
            continue;
        }

/*
 *  Open GRIB2 file, if not already open.
 */
        if ( strncmp(input.g2file, cur_gbfile, LLMXLN) != 0 ) {

/*
 *  If output GRIB file is different, must close previous one first
 */
            if ( strlen(cur_gbfile) != (size_t)0 ) {
                cfl_clos( gbfptr, &ret);
                gbfptr=0;
            }

/*
 *  Open GRIB file
 */
            gbfptr = cfl_aopn( input.g2file, &ret);
            if ( ret == 0 ) {
                strncpy( cur_gbfile, input.g2file, LLMXLN);
            }
            else {
                er_wmsg("CFL",&ret,input.g2file,&ier,7,strlen(input.g2file));
                ret = -24;
                er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
                gbfptr = 0;
                continue;
            }
        }

/*
 *  Write out WMO Header, if requested
 */
        gdmakewmo( &input, &gemgrid, chdr, &ret );
        if ( strlen(chdr) == (size_t)21 ) {
           cfl_writ( gbfptr, strlen(chdr), (unsigned char*)chdr, &ret );
        }

/*
 *  Write out GRIB2 message.
 */
        cfl_writ( gbfptr, g2len, g2msg, &ret );
        if ( ret != 0 ) {
            er_wmsg("CFL",&ret,input.g2file,&ier,7,strlen(input.g2file));
            ret = -25;
            er_wmsg("GDGRIB2",&ret," ",&ier,7,1);
            continue;
        }

/*
 *  Free no longer needed allocated space
 */
        if ( gemgrid.grid != 0 ) free(gemgrid.grid);
        if ( g2msg != 0 ) free(g2msg);

    }

/*
 *  Clean up files
 */
    dg_nend( &ret );
    if ( gbfptr != 0 ) cfl_clos( gbfptr, &ret);

/*
 *  Exit the GEMPAK user interface
 */
    ip_exit( &ret );
 
    return(0);
}
