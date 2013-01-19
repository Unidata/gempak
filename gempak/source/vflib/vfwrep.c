#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t   newinfo;
extern  SpcInfo_t   spcinfo;

void vfwrep ( int *iret )
/************************************************************************
 * vfwrep                                                               *
 *                                                                      *
 * This program creates WOU update text message.			*
 *                                                                      *
 * vfwrep ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *					== 0	normal			*
 *					!= 0	error(s)		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		09/04	initial coding				*
 * M. Li/SAIC           10/04   Replaced ctb_rdwou with ctb_rdprf	*
 * A. Hardy/NCEP	 3/05   Added irmzn to gg_wcck call seq.	*
 * G. Grosshans/SPC	 6/05	Updated gg_wwtp for CSC;improved err chk*
 * F. J. Yen/NCEP	 8/05	Return error codes to invoking routine	*
 ***********************************************************************/
{
    char    wtype[10], tzone[4], strtim[20], stptim[20], cnties[5];
    char    dattim[20], srchtim[20], systim[20], newname[132];
    char    attnln[180], stzstr[256], prdcod[2], offid[5], sigcd[2];
    char    tag[32], value[120], tblnam[72], dirsym[160];
    int     ii, ier, hhmm, ncnty, inumb, iznflg, icancl; 
    int	    itest, lun, vtecln, time_arry[5], irmzn, callby;
    size_t  jj;
/*-------------------------------------------------------------------*/

    *iret = 0;

    if ( strcmp ( spcinfo.replcnm[0], "NONE") == 0 )  return;

    /*
     *  Look through the array of replacement numbers.
     */
    for(ii = 0; ii <= spcinfo.wwrepnm; ii++) {

	strcpy ( newname, "ww");
	strcat ( newname, spcinfo.replcnm[ii] );
	strcat ( newname, ".txt" );
	strcpy ( newinfo.file_info.filnam,newname);

  	vfrptxt (newname, &ier); 
        if ( ier != 0 ) continue;

	/*
         * set all parameter values for calling GG_WWTP
         */
        if ( strcasecmp ( newinfo.wtype, "TORNADO" ) == 0 ) {

	    strcpy ( wtype, "TOR");
        }
        else {

	    strcpy ( wtype, "TSM");
        }

        time_arry[0] = newinfo.vtime.year;
        time_arry[1] = newinfo.vtime.month;
        time_arry[2] = newinfo.vtime.day;
        sscanf (newinfo.vtime.hour, "%d", &hhmm);
	time_arry[3] = hhmm / 100;
        time_arry[4] = hhmm % 100;
        for (jj=0;jj<sizeof(strtim);jj++) strtim[jj] = '\0';
        ti_itoc ( time_arry, strtim, &ier, 19 );

        time_arry[0] = newinfo.etime.year;
        time_arry[1] = newinfo.etime.month;
        time_arry[2] = newinfo.etime.day;
        sscanf (newinfo.etime.hour, "%d", &hhmm);
	time_arry[3] = hhmm / 100;
        time_arry[4] = hhmm % 100;
        for (jj=0;jj<sizeof(stptim);jj++) stptim[jj] = '\0';
        ti_itoc ( time_arry, stptim, &ier, 19 );

        strcpy (tzone, newinfo.timzone);

        time_arry[0] = spcinfo.vtime.year;
        time_arry[1] = spcinfo.vtime.month;
        time_arry[2] = spcinfo.vtime.day;
        sscanf (spcinfo.vtime.hour, "%d", &hhmm);
	time_arry[3] = hhmm / 100;
        time_arry[4] = hhmm % 100;
        for (jj=0;jj<sizeof(dattim);jj++) dattim[jj] = '\0';
        ti_itoc ( time_arry, dattim, &ier, 19 );

        strcpy (srchtim, dattim);
	strcpy (systim,  dattim);

        irmzn = 0;
	ncnty = 0;
	cnties[0] = '\0';

        sscanf (spcinfo.replcnm[ii], "%d", &inumb);
        gg_wcck ( &inumb, dattim, systim, &irmzn, &iznflg, &icancl, 
                  attnln, stzstr, &ier, strlen(dattim), strlen(systim), 
                  179, 255 );

        if ( ier != 0 ) continue;

    /*	Check return value from each all to ctb_rdprf and
     *	if not 0 then send error to NMAP button and set a 
     *	default value.
     */
        strcpy ( tblnam, "woudef.tbl" );
        strcpy ( dirsym, "txtprd" );
        strcpy (tag, "UPDT_WOU_USE_VTEC");
        ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
        if ( ier == 0 ) {
	    sscanf (value, "%d",  &vtecln);
        }
        else {
	    vtecln = 2;
	    *iret = *iret + 1;
        }

        strcpy (tag, "UPDT_WOU_PROD_CODE");
	ctb_rdprf ( tblnam, dirsym, tag, prdcod, &ier );
        if ( ier != 0 ) {
            strcpy ( prdcod, "E");
	    *iret = *iret + 2;
        }

        strcpy (tag, "UPDT_WOU_OFFID");
	ctb_rdprf ( tblnam, dirsym, tag, offid, &ier );
        if ( ier != 0 ) {
            strcpy ( offid, "KWNS");
	    *iret = *iret + 4;
        }

        strcpy (tag, "UPDT_WOU_SIGCD");
	ctb_rdprf ( tblnam, dirsym, tag, sigcd, &ier );
        if ( ier != 0 ) {
            strcpy ( sigcd, "A");
	    *iret = *iret + 8;
        }

        if ( strcasecmp ( spcinfo.status, "TEST" ) == 0 ) {

	    itest = 3;
        }
        else {

	    itest = 0;
        }	

	lun = 0;
	callby = 1;

        gg_wwtp (&lun, &inumb, dattim, srchtim, systim, wtype, strtim, stptim,
		 &ncnty, cnties, tzone, attnln, &vtecln, prdcod, offid, sigcd,
		 stzstr, &iznflg, &itest, &callby, &ier,  strlen(dattim), 
		 strlen(srchtim), strlen(systim), strlen(wtype), strlen(strtim),
		 strlen(stptim), strlen(cnties), strlen(tzone), strlen(attnln), 
		 strlen(prdcod), strlen(offid),  strlen(sigcd), strlen(stzstr));
        if ( ier == -25 ) {
	    *iret = *iret + 16;
        }
	else if ( ier == -24 ) {
	    *iret = *iret + 32;
	}
    }
}
