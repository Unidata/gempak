#include "geminc.h" 
#include "gemprm.h"

#define  EOL  "\n"
#define  LINE_LEN   66

int main ( void )
/************************************************************************
 * TESTUTL								*
 *									*
 * This program tests the TEXTLIB "UTL" functions.			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 7/03						*
 * A. Hardy/NCEP	 8/03	Initialize plat/plon option 12		*
 * A. Hardy/NCEP	 4/05	Added UTL_GDAT				*
 * H. Zeng/SAIC		06/06	modified calling sequence for utl_ivet	*
 ***********************************************************************/
{
	int	cont, iret, numsub, len, ii;
	int	time, newtime, dtmonth, daywk, vhour, ehour, emin, datwk;
	int	itmarr[5], vtime[5], etime[5], iarr[5], varr[5], 
		earr[5], inewtm, vnewtm, enewtm, num;	
	float	plat, plon, newlat, newlon; 
	char	ampm[3], locnam[15], str[12], 
	        curtim[7], pmm[4], pdwk[4], hr[5],
		iampm[3], vampm[3], eampm[3], genday[50];
        char	acstn1[4], acnam1[33], acst1[3], chmon[4], 
		lclzn[4], hrmn[5], chdwk[4];
	char	wfoid[4], wname[33], wstate[3], disdir[20], stn[5];
	char	select[LLSCRN], **wfoarr, wfostr[180], ugcstr[500];
/*---------------------------------------------------------------------*/
	cont = G_FALSE;
	iret = 0;

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( "   1 = UTL_AMPM    2 = UTL_CTIM   3 = UTL_GMON\n" );
	    printf ( "   4 = UTL_GNAME   5 = UTL_GTOD   6 = UTL_IVET\n" );
	    printf ( "   7 = UTL_TOMIN   8 = UTL_WNMST  9 = UTL_GDWK\n" );
	    printf ( "  10 = UTL_WFOS   11 = UTL_UGCP  12 = UTL_AVCD\n" );
	    printf ( "  13 = UTL_GDAT\n\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_TRUE;
		default:
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		printf ( "Calculate AM or PM\n" );
		printf ( "\nEnter hours (0-24):\n" );
		scanf ( " %d", &time);

		utl_ampm ( time, &newtime, ampm, &iret );

		if ( iret == 0 ) {
		    printf ( "\nLocal time: %d %s ", newtime, ampm );
		}
		printf ( "\nUTL_AMPM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( "Get system time - put into string(DDHHNN).\n" );
		
		len = sizeof(curtim);
		utl_ctim ( len, curtim, &iret );

		if ( iret == 0 ) {
		    printf ( "\nSystem time: %s\n", curtim );
		}

		printf ( "\nUTL_CTIM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Get 3 char abbrev. for a month:\n" );
		printf ( "Enter month (1-12) :\n" );
		scanf ( " %d", &dtmonth );

		utl_gmon ( dtmonth, pmm, &iret );

		if ( iret == 0 ) {
		    printf ( "Month: %s\n", pmm);
		}
		printf ( "\nUTL_GDAT: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		printf ( "Get station info. from anchor station id.\n\n" );

		printf ( "Enter anchor point station id (eg. SPI):\n" );
		scanf ( " %s", acstn1 );

		utl_gname ( acstn1, acnam1, acst1, &iret );
	
		if ( iret == 0 ) {
		    printf ( "\nAnchor point:  %-22s %s\n", acnam1, acst1 );
		}

		printf ( "\nUTL_GNAME: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		printf ( "Get general time of day string.\n\n" );

		printf ( "Enter valid hour (1-12):\n" );
		scanf ( " %d", &vhour);
		printf ( "Enter ending hour (1-12):\n" );
		scanf ( " %d", &ehour );
		printf ( "Enter ending minutes(0-60):\n" );
		scanf ( " %d", &emin );
		printf ( "Enter valid AM or PM designation:\n" );
		scanf ( " %s", vampm );
		printf ( "Enter ending AM or PM designation:\n" );
		scanf ( " %s", eampm );
		printf ( "Enter numerical day of the week(1-SUN, 2-MON, etc):\n" );
		scanf ( " %d", &daywk );

		len = sizeof(genday);
		utl_gtod (vhour, ehour, emin, vampm, eampm, daywk, 
		          len, genday, &iret );

		if ( iret == 0 ) {
		    printf ( "\n GTOD string: %s \n", genday );
		}
		printf ( "\nUTL_GTOD: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		printf ( "Generate issue, valid and ending times\n\n" );

		printf ( "Enter local time zone (Eg. CDT):\n" );
		scanf ( " %s", lclzn);
		printf ( "Enter INITIAL time (Eg. 2003 5 6 1430):\n" );
		scanf ( " %d %d %d %s", &itmarr[0], &itmarr[1], &itmarr[2],
		          hrmn);
		time  = atoi(hrmn);
		itmarr[3] = time / 100;
		itmarr[4] = time % 100;

		printf ( "Enter VALID time (Eg. 2003 5 6 1500):\n" );
		scanf ( " %d %d %d %s", &vtime[0], &vtime[1], &vtime[2],
		          hrmn);
		time  = atoi(hrmn);
		vtime[3] = time / 100;
		vtime[4] = time % 100;

		printf ( "Enter ENDING time (Eg. 2003 5 6 2030):\n" );
		scanf ( " %d %d %d %s", &etime[0], &etime[1], &etime[2],
		          hrmn);
		time  = atoi(hrmn);
		etime[3] = time / 100;
		etime[4] = time % 100;

		utl_ivet ( lclzn, itmarr, vtime, etime, iarr, 
		           &inewtm, iampm, chmon, chdwk, varr, &vnewtm, 
			   vampm, earr, &enewtm, eampm, &datwk, 
			   &iret );

		if ( iret == 0 ) {
	            printf("\nMonth : %s   Day : %s   Day number : %d\n", 
		       chmon, chdwk, datwk);
		    printf("Issue Time  : %2d %s %2d %2d %2d %2d %2d\n", inewtm, 
		       iampm, iarr[0], iarr[1], iarr[2], iarr[3], iarr[4]); 
		    printf("Valid Time  : %2d %s %2d %2d %2d %2d %2d\n", vnewtm, 
		       vampm, varr[0], varr[1], varr[2], varr[3], varr[4]); 
		    printf("Ending Time : %2d %s %2d %2d %2d %2d %2d\n", enewtm, 
		       eampm, earr[0], earr[1], earr[2], earr[3], earr[4]); 
		}
		printf ( "\nUTL_IVET: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		printf ( "Convert lat/lon pairs into minutes.\n\n" );

		printf ( "Enter test lat/lon pair:\n" );
		scanf ( " %f %f", &plat, &plon );

		utl_tomin ( &plat, &plon, &newlat, &newlon, &iret );

		if ( iret == 0 ) {
		    printf("newlat:  %f  newlon: %f\n", newlat, newlon);
		}
		    printf ( "\nUTL_TOMIN: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		printf ( "WFO names and states.\n\n" );
		printf ( "Enter WFO ID:\n" );
		scanf ( " %s", wfoid );

		utl_wnmst ( wfoid, wname, wstate, &iret );

		if ( iret == 0 ) {
		    printf ( "Name:  %s     State: %s\n", wname, wstate );
		}

		printf ( "\nUTL_WNMST: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

		printf ( "Get 3 char abbrev. day of the week:\n" );
		printf ( "Enter numerical day of the week(1-SUN, 2-MON, etc):\n" );
		scanf ( " %d", &daywk );

		utl_gdwk ( daywk, pdwk, &iret );

		if ( iret == 0 ) {
		    printf ( "Day: %s\n", pdwk );
		}
		printf ( "\nUTL_GDWK: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {

                printf ( "Create WFO string.\n");
                printf ( "Enter number of WFO ids.\n");
                scanf ( " %d", &num);

                wfoarr = (char **)malloc(num * sizeof(char *));
                for ( ii = 0; ii < num; ii++ ) {
                    printf ( "Enter WFO id  #%d :\n", ii+1 );
                    scanf ( " %s", str);
                    len = strlen( str );
                    wfoarr[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( wfoarr[ii], str );

                }
                utl_wfos ( wfoarr, num, wfostr, &iret );

                printf ( "\n WFO string:\n%s", wfostr );
                printf ( "\n UTL_DWFO: iret = %d\n\n", iret );

                for( ii = 0; ii < num; ii++ )
                    free( wfoarr[ii] );

                if ( wfoarr )
                   free( (char **) wfoarr );

            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

                printf ( "Create UGC string.\n");
                printf ( "Enter number of UG codes.\n");
                scanf ( " %d", &num);

                wfoarr = (char **)malloc(num * sizeof(char *));
                for ( ii = 0; ii < num; ii++ ) {
                    printf ( "Enter UG code #%d :\n", ii+1 );
                    scanf ( " %s", str);
                    len = strlen( str );
                    wfoarr[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( wfoarr[ii], str );

                }
		printf ( "Enter day (DD).\n");
                scanf ( " %s", str);

                printf ( "Enter hour (HHNN).\n");
                scanf ( " %s", hr);

		len = sizeof ( ugcstr );
                utl_ugcp ( wfoarr, &num, str, hr, &len, ugcstr, &iret );

                printf ( "\n WFO string:\n%s", ugcstr );
                printf ( "\n UTL_DWFO: iret = %d\n\n", iret );

                for( ii = 0; ii < num; ii++ )
                    free( wfoarr[ii] );

                if ( wfoarr )
                   free( (char **) wfoarr );

            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 12 ) {

		plat = -9999.9;
		plon = -9999.9;
		printf ( "Create the dist. direc. and station text string:\n" );
		printf ( "\nEnter locator type (VOR, ANCHOR):\n" );
		scanf ( " %s", locnam);
		printf ( "Enter test lat/lon point separated by a space: \n" );
		scanf ( " %f %f", &plat, &plon );

		utl_avcd ( locnam, &plat, &plon, disdir, stn, 
		          &iret );

		if ( iret == 0 ) {
		    printf ( "\nDistance/direction: %s     Station: %s\n", 
		           disdir, stn);
		}
		printf ( "\nUTL_AVCD: iret = %d\n\n", iret );
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 13 ) {
		printf ( "Enter the month number (1-12). \n");
		scanf ( " %d", &dtmonth);
		printf ( "Enter the day of the week number (1-7). \n");
		scanf ( " %d", &daywk);

                utl_gdat ( dtmonth, daywk, chmon, chdwk, &iret );

		if ( iret == 0 ) {
		    printf ( "\nMon.: %s     Day: %s\n", chmon, chdwk); 
                }
		printf ( "\nUTL_GDAT: iret = %d\n\n", iret );
            }
/*---------------------------------------------------------------------*/
	}
	return (0);
}
