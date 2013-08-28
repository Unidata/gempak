#include "gb2def.h"

void gb2_setftime( char *dattim, int tint, int pdtnum, int *pdtmpl, int *iret )
/************************************************************************
 * gb2_setftime								*
 *									*
 * This routine sets forecast times and intervals in the appropriate    *
 * GRIB2 Product Definition Template.                                   *
 *									*
 * gb2_setftime ( dattim, tint, pdtnum, pdtmpl, iret )                  *
 *									*
 * Input parameters:							*
 *      *dattim         char            GEMPAK date/time                *
 *	tint  	        int             Time interval (in minutes)      *
 *	pdtnum  	int             Product Definition Template No. *
 *	pdtmpl  	int             Product Definition Template     *
 *                                      values                          *
 *									*
 * Output parameters:    						*
 *	pdtmpl  	int             Product Definition Template     *
 *                                      values                          *
 *	*iret		int		return code			*
 *                                          0 = Normal return           *
 *                                        -27 = unrecognized PDT        *
 **									*
 * Log:									*
 * S. Gilbert/NCEP      08/05                                           *
 ***********************************************************************/
{
    int     ret, idx;
    int     dtf[3], fhour, fmin, vtarr[5], ctarr[5], diffmin;
    int     pos[20]={0,0,0,0,0,0,0,0,15,22,16,18,17,31,30,0,0,0,0,0};
    char    cnum[10], vdattim[20];

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  If 
     */
    if ( pdtnum >= 0 && pdtnum <= 7 ) {
        /* 
         *   Set forecast time for PDTs 4.0 through 4.7
         */
        tg_ctoi( dattim, dtf, &ret, 20 );
        dtf[2] %= 100000;
        fhour = dtf[2] / 100;
        fmin = dtf[2] % 100;
        if ( fmin == 0 ) {
            /*   encode forecast time in hours   */
            pdtmpl[7] = 1;
            pdtmpl[8] = fhour;
        }
        else {
            /*   encode forecast time in minutes   */
            fmin += 60 * fhour;
            pdtmpl[7] = 0;
            pdtmpl[8] = fmin;
        }
    }
    else if ( pdtnum >= 8 && pdtnum <= 15 ) {
        /* 
         *   Set starting fcst hr, end date/time, and interval 
         *   for PDTs 4.8 through 4.14
         */
        /*  end date/time   */
        tg_vald( dattim, vdattim, &ret, 20, 20 );
        ti_ctoi( dattim, ctarr, &ret, 20 );
        ti_ctoi( vdattim, vtarr, &ret, 20 );
        idx = pos[pdtnum];
        pdtmpl[ idx++ ] = vtarr[0];
        pdtmpl[ idx++ ] = vtarr[1];
        pdtmpl[ idx++ ] = vtarr[2];
        pdtmpl[ idx++ ] = vtarr[3];
        pdtmpl[ idx++ ] = vtarr[4];
        pdtmpl[ idx++ ] = 0;
        pdtmpl[ idx ] = 1;
        /*  set interval  */
        idx += 4;
        if (  (tint%60 ) == 0 ) {
            pdtmpl[ idx++ ] = 1;      /*  hours */
            pdtmpl[ idx ] = tint/60;
        }
        else {
            pdtmpl[ idx++ ] = 0;      /*  minutes */
            pdtmpl[ idx ] = tint;
        }
        /*  start  fcst time   */
        ti_mdif( vtarr, ctarr, &diffmin, &ret );
        diffmin -= tint;
        if ( ( diffmin%60 ) == 0 ) {
            pdtmpl[7] = 1;      /*  hours */
            pdtmpl[8] = diffmin/60;
        }
        else {
            pdtmpl[7] = 0;      /*  minutes */
            pdtmpl[8] = diffmin;
        }
    }
    else if ( pdtnum != 65535 ) {
        *iret = -27;
        snprintf( cnum, 10, "4.%d", pdtnum);
        er_wmsg( "GB", iret, cnum, &ret, 2, strlen(cnum) );
    }

}
