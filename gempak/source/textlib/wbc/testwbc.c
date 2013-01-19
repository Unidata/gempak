#include "geminc.h" 
#include "gemprm.h"

#define  EOL  "\n"
#define  LINE_LEN   66

int  main ( void )
/************************************************************************
 * TESTWBC								*
 *									*
 * This program tests the TEXTLIB "WBC" functions.			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 7/03						*
 * A. Hardy/NCEP	11/03 	Added 'status' to wbc_dhdl call seq. 	*
 * A. Hardy/NCEP	 1/04   Added VTEC parameters to wbc_dcty	* 
 * A. Hardy/NCEP	 3/04   Added wbc_mzhv and wbc_mzrm		*
 * A. Hardy/NCEP	 3/05   Added irmzn to wbc_dsts			*
 * A. Hardy/NCEP	 4/05   Added WBC_WCP				*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 ***********************************************************************/
{
	int	cont, ier, iret, numsub, len, ii, num;
	int	vnewtm, enewtm, typnum, vpdst1, vpdst2;
	int     maxgust, maxtops, degree, speed;
	int	len1, len2, wnum, vmins, emins;
	float	hailsz, wclat1, wclon1, wclat2, wclon2,wclat3, wclon3, 
	        wclat4, wclon4, vorlat1, vorlon1, vorlat2, vorlon2;
	char	locnam[15], vorstr[400], areastr[450],vampm[3], eampm[3];
        char	lclzn[4], sep[3], ans[2], dy[3], vpdir1[4], vpdir2[4];
	char	select[LLSCRN], wtype[32], vpstn1[4], vpstn2[4]; 
	char    avnstr[500], constr[500], perstr[180], status[7],
		hdlstr[500], states[256], stzstr[256], sttstr[256],
		hwmstr[500], efen[12], efst[12], str[12], hr[5],hrmn[5];
	char	**ugc_arr, **cnam_arr, **st_arr, **ind_arr, cntystr[5000];
	char	**artyp, **arstr, **arend, **arnum, **arltln;
	char    prdcod[2], actn[4], offid[5], phen[3], sigcd[2], etn[5];
        char    blank[2]={' '}, cstr[256], systim[20];
	int	ugcln, vtecln, time, vtime[5], etime[5], irmzn, one;
	Boolean	hvmz;
/*---------------------------------------------------------------------*/
	cont = G_FALSE;
	iret = 0;
	ier  = 0;

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( "  1 = WBC_DAVN   2 = WBC_DCON  3 = WBC_DCTY\n" );
	    printf ( "  4 = WBC_DEFL   5 = WBC_DHDL  6 = WBC_DHWM\n" );
	    printf ( "  7 = WBC_DREP   8 = WBC_DSTS  9 = WBC_AREA\n" );
	    printf ( " 10 = WBC_VORS  11 = WBC_MZRM 12 = WBC_MZHV\n" );
	    printf ( " 13 = WBC_WCP\n\n" );
	    printf ( " \n" );
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
	
                printf ( "Create AVIATION string:\n\n" );

		printf ( "Enter watch type (SEVERE THUNDERSTORM or " );
		printf ( "TORNADO).\n" );
		scanf ( " %[^\n\r]s", wtype );
		printf ( "Enter hail size. \n");
		scanf ( " %f", &hailsz);
		printf ( "Enter maximum wind gusts. \n");
		scanf ( " %d", &maxgust);
		printf ( "Enter maximum cloud tops (100's of feet). \n");
		scanf ( " %d", &maxtops);
		printf ( "Enter storm motion vector (eg. 240). \n");
		scanf ( " %d", &degree);
		printf ( "Enter storm speed (eg. 35). \n");
		scanf ( " %d", &speed);

		len1 = sizeof(avnstr);
		wbc_davn ( wtype, &hailsz, &maxgust, &maxtops, &degree, 
		           &speed, len1, avnstr, &iret );

                strcat (avnstr, EOL );
	        strcat (avnstr, EOL );
	        strcat (avnstr, EOL );
                len = LINE_LEN;
                cst_wrap ( avnstr, blank, &len, EOL, (char *)NULL, avnstr, &ier );
                printf ( "Aviation string: \n%s\n",avnstr);
		printf ( "\nWBC_DAVN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( "Create CONDITIONS strings.\n\n" );

	        printf ( "Enter watch type (SEVERE THUNDERSTORM or " );
		printf ( "TORNADO).\n" );
		scanf ( "  %[^\n\r]s", wtype );

		len1 = sizeof(constr);
		len2 = sizeof(perstr);
		wbc_dcon ( wtype, len1, len2, constr, perstr, &iret );

		strcat (constr, EOL ); 
		len = LINE_LEN;
		cst_wrap ( constr, blank, &len, EOL, (char *)NULL, constr, &ier );
                printf ( "\nConditions string: \n\n%s\n", constr);

		strcat (perstr, EOL );
		cst_wrap ( perstr, blank, &len, EOL, (char *)NULL, perstr, &ier );
                printf ( "\nPersons string: \n\n%s\n", perstr);

		printf ( "\nWBC_DCON: iret = %d\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Create COUNTY TEXT.\n\n" );

		printf ( "Enter number of counties.\n");
		scanf ( " %d", &num);

		printf ( "Enter day (DD).\n");
		scanf ( " %s", dy);

		printf ( "Enter hour (HHNN).\n");
		scanf ( " %s", hr);

		printf ( "Print UGC line? (y/n).\n");
		scanf ( " %s", ans);

		ugcln = 0;
		if ( ( strcmp (ans,"Y")== 0 )  || 
		       ( strcmp( ans, "y") == 0  ) ) ugcln = 1; 

		vtecln = 0;
		printf ( "Print VTEC line? (y/n).\n");
		scanf ( " %s", ans);

		if ( ( strcmp (ans,"Y")== 0 )  || 
		       ( strcmp( ans, "y") == 0  ) ) {

		    printf ( "Use product code? (y/n).\n");
		    scanf ( " %s", ans);

		    vtecln = 1;
		    if ( ( strcmp (ans,"Y")== 0 )  || 
		       ( strcmp( ans, "y") == 0  ) ) {
			vtecln = 2;
		        printf ( "Enter product code(O, T, E, X).\n");
		        scanf ( " %s", prdcod );
		    }
		    else {
			strcpy (prdcod, " ");
		    }
		    printf ( "Enter action (NEW, CON, CAN...)\n");
		    scanf ( " %s", actn);
		    printf ( "Enter office id (KWNS)\n");
		    scanf ( " %s", offid);
		    printf ( "Enter phenomena type (2 chars - TO, SV)\n");
		    scanf ( " %s", phen);
		    printf ( "Enter significance code ( W, A, Y...)\n");
		    scanf ( " %s", sigcd);
		    printf ( "Enter Event Tracking number\n");
		    scanf ( " %s", etn);
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
	        }
		else {
		    strcpy (prdcod, " ");
		    strcpy (actn, " ");
		    strcpy (offid, " ");
		    strcpy (phen, " ");
		    strcpy (sigcd, " ");
		    strcpy (etn, " ");
		    for ( ii = 0; ii < 5; ii++ ) {
			vtime[ii] = 0;
			etime[ii] = 0;
		    }
		}

		ugc_arr = (char **)malloc(num * sizeof(char *));
		cnam_arr = (char **)malloc(num * sizeof(char *));
		st_arr = (char **)malloc(num * sizeof(char *));
		ind_arr = (char **)malloc(num * sizeof(char *));

		for ( ii = 0; ii < num; ii++ ) {
		    printf ( "Enter UGC id  #%d :\n", ii+1 );
		    scanf ( " %s", str);
		    len = strlen( str );
		    ugc_arr[ii] = (char *)malloc((len+1) * sizeof(char));
		    strcpy( ugc_arr[ii], str );

		    printf ( "Enter county name #%d :\n", ii+1 );
		    scanf ( " %s", cstr);
		    len = strlen( cstr );
		    cnam_arr[ii] = (char *)malloc((len+1) * sizeof(char));
		    strcpy( cnam_arr[ii], cstr );

		    printf ( "Enter state id #%d :\n", ii+1 );
		    scanf ( " %s", str);
		    len = strlen( str );
		    st_arr[ii] = (char *)malloc((len+1) * sizeof(char));
		    strcpy( st_arr[ii], str );

		    len = 33;
		    ind_arr[ii] = (char *)malloc((len+1) * sizeof(char));
		    strcpy( ind_arr[ii], str );
		}

		len2 = sizeof ( cntystr );
		wbc_dcty ( ugc_arr, cnam_arr, st_arr, &num, dy, hr, 
			   &len2, &ugcln, &vtecln, prdcod, actn, offid,
			   phen, sigcd, etn, vtime, etime, ind_arr, 
			   cntystr, &iret );

		printf ( "\n county list string: \n\n%s\n", cntystr );
		printf ( "\nWBC_DCTY: iret = %d\n\n", iret );

                for( ii = 0; ii < num; ii++ ) {
		    free( ugc_arr[ii] );
		    free( cnam_arr[ii] );
		    free( st_arr[ii] );
		    free( ind_arr[ii] );
		}

		if ( ugc_arr ) {
		   free( (char **) ugc_arr );
		   free( (char **) cnam_arr );
		   free( (char **) st_arr );
		   free( (char **) ind_arr );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		printf ( "\nFind effective local time start/end times.\n\n" );

		printf ( "Enter local time zone (Eg. CDT):\n" );
		scanf ( " %s", lclzn);

		printf ( "Enter starting local hour, minutes and AM/PM:\n" );
		scanf ( " %d %d %s", &vnewtm, &vmins, vampm);

		printf ( "Enter ending local hour, minutes and AM/PM:\n" );
		scanf ( " %d %d %s", &enewtm, &emins, eampm);

		len1 = sizeof (efst);
		len2 = sizeof (efen);
                wbc_defl ( vnewtm, vmins, vampm, enewtm, emins,
		           eampm, lclzn, len1, len2, efst, efen, &iret );

		printf ( "\n Local Starting time: %s \n", efst);
		printf ( " Local Ending time: %s \n\n", efen);
		printf ( "WBC_DEFL: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		printf ( "Create Headline Text.\n\n" );
	        printf ( "Enter watch type (SEVERE THUNDERSTORM or " );
		printf ( "TORNADO).\n" );
		scanf ( "  %[^\n\r]s", wtype );
		printf ( "Enter watch status (TEST or ACTIVE). \n");
		scanf ( " %s", status);
		printf ( "Enter watch number.\n" );
		scanf ( "  %d", &wnum);

                len1 = sizeof ( hdlstr );
		cst_lcuc ( status, status, &ier);
		wbc_dhdl ( wtype, status, &wnum, len1, hdlstr, &iret );

		strcat (hdlstr, EOL );
                printf ( "\n Headline string: \n\n%s\n", hdlstr);
		printf ( "WBC_DHDL: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		printf ( "Create the HAIL WIND MOTION string.\n\n");

		printf ( "Enter cloud level separator (eg. TO or ..) \n");
		scanf ( " %s", sep);
		printf ( "Enter hail size. \n");
		scanf ( " %f", &hailsz);
		printf ( "Enter maximum wind gusts. \n");
		scanf ( " %d", &maxgust);
		printf ( "Enter maximum cloud tops (100's of feet). \n");
		scanf ( " %d", &maxtops);
		printf ( "Enter storm motion vector (eg. 240). \n");
		scanf ( " %d", &degree);
		printf ( "Enter storm speed (eg. 35). \n");
		scanf ( " %d", &speed);

		len1 = sizeof(hwmstr);
		wbc_dhwm ( sep, &hailsz, &maxgust, &maxtops, &degree, 
		           &speed, len1, hwmstr, &iret );

                strcat (hwmstr, EOL );
	        strcat (hwmstr, EOL );
	        strcat (hwmstr, EOL );
                len = LINE_LEN;
                cst_wrap ( hwmstr, blank, &len, EOL, (char *)NULL, hwmstr, &ier );
                printf ( "Hail, wind, motion string: \n\n%s\n",hwmstr);
		printf ( "WBC_DHWM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		printf ( "REPLACEMENT TEXT:\n" );
		printf ( "WBC_DREP: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		printf ( "STATE Zones string \n\n" );

		printf ( "Enter state zone string (Eg. AR MO IL)\n");
		scanf ( "  %[^\n\r]s", states );

		printf ( "Do you want the marine zones removed: 1-yes  0-no)\n");
		scanf ( "  %d", &irmzn);

		stzstr[0] = '\0';
		sttstr[0] = '\0';
		len1 = sizeof (stzstr);
		cst_lcuc ( states, states, &ier );
		wbc_dsts ( states, &len1, &irmzn, stzstr, sttstr, &iret );

		strcat (stzstr, EOL );
		len = LINE_LEN;
		cst_wrap ( stzstr, blank, &len, EOL, (char *)NULL, stzstr, &ier );
                printf ( "\nState zones string: \n\n%s\n", stzstr);

		strcat (sttstr, EOL );
		cst_wrap ( sttstr, blank, &len, EOL, (char *)NULL, sttstr, &ier );
                printf ( "\nState names string: \n%s\n", sttstr);

		printf ( "WBC_DSTS: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

		printf ( "Create VOR watch area string\n" );

		strcpy ( locnam, "VOR");
		printf ( "\nEnter VOR string.\n"); 
		scanf ( " %[^\n\r]s", vorstr);

		len = sizeof(areastr);
		wbc_area ( locnam, vorstr, len, areastr, &iret );

		if ( iret == 0 ) {
		    printf ( "\n\nArea string: %s\n", areastr);
		}

		printf ( "\nWBC_AREA: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 10 ) {

		strcpy ( locnam, "VOR" );
		printf ( "Enter text product type:\n" );
		printf ( "1 = PWN   2 = WCP   3 = AWN \n" );
		scanf ( " %d", &typnum );
		
		printf ( "Enter VOR point 1 info (Eg. 47 ENE UIN):\n" );
		scanf ( " %d %s %s", &vpdst1, vpdir1, vpstn1);

		printf ( "\nEnter vor point 2 info (Eg. 26 WNW FAM):\n" );
		scanf ( " %d %s %s", &vpdst2, vpdir2, vpstn2);

		printf ( "\nEnter watch corner point 1:\n" );
		scanf ( " %f %f", &wclat1, &wclon1);
		printf ( "\nEnter watch corner point 2:\n" );
		scanf ( " %f %f", &wclat2, &wclon2);
		printf ( "\nEnter watch corner point 3:\n" );
		scanf ( " %f %f", &wclat3, &wclon3);
		printf ( "\nEnter watch corner point 4:\n" );
		scanf ( " %f %f", &wclat4, &wclon4);

		len = sizeof(vorstr);
		wbc_vors ( locnam, typnum, &vpdst1, vpdir1, vpstn1, 
		           &vpdst2, vpdir2, vpstn2, &wclat1, &wclon1, 
			   &wclat2, &wclon2, &wclat3, &wclon3, &wclat4, 
			   &wclon4, len, vorstr, &vorlat1, &vorlon1, 
			   &vorlat2, &vorlon2, &iret );

		if ( iret == 0 ) {
		   printf ( "\nVOR point 1: %6.2f %6.2f\n", vorlat1, vorlon1 );
		   printf ( "VOR point 2: %6.2f %6.2f\n", vorlat2, vorlon2 );
		   printf ( "VOR string: %s\n", vorstr);
		}

		printf ( "\nWBC_VORS: iret = %d\n\n", iret );
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

                printf ( "STATE Zones string without marine zones\n\n" );

		printf ( "Enter state zone string (Eg. AR MO IL)\n");
		scanf ( "  %[^\n\r]s", states );

		stzstr[0] = '\0';

		cst_lcuc ( states, states, &ier );
		wbc_mzrm ( states, stzstr, &len1, &iret );

                printf ( "\nState id string: %s\n\n", stzstr);

		printf ( "WBC_MZRM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 12 ) {

                printf ( "Check if there is a marine zone\n\n" );

		printf ( "Enter state zone string (Eg. AR MO IL)\n");
		scanf ( "  %[^\n\r]s", states );

		stzstr[0] = '\0';

		cst_lcuc ( states, states, &ier );
		wbc_mzhv ( states, &hvmz, &iret );

                printf ( "\nHave marine zone: %d\n", hvmz);

		printf ( "WBC_MZHV: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
 	    if ( numsub == 13 ) {
                printf ( "Create the WCP product.\n\n");
		printf ( "Enter number of watches.\n");
		scanf ( " %d", &num);

		artyp = (char **)malloc(num * sizeof(char *));
		arstr = (char **)malloc(num * sizeof(char *));
		arend = (char **)malloc(num * sizeof(char *));
		arnum = (char **)malloc(num * sizeof(char *));
		arltln = (char **)malloc(num * sizeof(char *));


		for (ii = 0; ii < num; ii++ ) {
		    printf ( "Enter Watch type (TSM or TOR)#%d :\n", ii+1 );
                    scanf ( " %s", str);
                    len = strlen( str );
                    artyp[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( artyp[ii], str );

		    printf ( "Enter Watch start time  #%d :\n", ii+1 );
                    scanf ( " %s", cstr);
                    len = strlen( cstr );
                    arstr[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( arstr[ii], cstr );

		    printf ( "Enter Watch end time #%d :\n", ii+1 );
                    scanf ( " %s", cstr);
                    len = strlen( cstr );
                    arend[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( arend[ii], cstr );

		    printf ( "Enter Watch number #%d :\n", ii+1 );
                    scanf ( " %s", str);
                    len = strlen( str );
                    arnum[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( arnum[ii], str );

		    printf ( "Enter Watch lat/lon #%d :\n", ii+1 );
		    printf ( "Eg. 35.849,-82.070;33.660,-79.550;33.450,-79.760;\n");
                    scanf ( "%s", cstr);
                    len = strlen( cstr );
                    arltln[ii] = (char *)malloc((len+1) * sizeof(char));
                    strcpy( arltln[ii], cstr );
		}
                one = 1;
		css_gtim (&one, systim, &ier );


		wbc_wcp ( &num, systim, artyp, arstr, arend, arnum, 
			 arltln, &iret );

 		for( ii = 0; ii < num; ii++ ) {
		    free( artyp[ii] );
		    free( arstr[ii] );
		    free( arend[ii] );
		    free( arnum[ii] );
		    free( arltln[ii] );
		}

		if ( num > 0) {
		   free( (char **) artyp);
		   free( (char **) arstr);
		   free( (char **) arend);
		   free( (char **) arnum);
		   free( (char **) arltln);
		}
                sprintf( cstr, " cat KWNSWCPSPC ");
                printf("\n");
                system ( cstr);


	}
/*---------------------------------------------------------------------*/
	}
	return (0);
}
