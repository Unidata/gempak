#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t  spcinfo;

int main ( void )
/************************************************************************
 * TESTVF								*
 *									*
 * This program tests the "VF" functions.				*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		02/00   Created					*
 * A. Hardy/GSC		03/00   Added VFWWCL                            *
 * A. Hardy/GSC		03/00   Added VFWOUI, VFWAWN and VFWPWN         *
 * A. Hardy/GSC		05/00   Added VFWWCP                            *
 * A. Hardy/GSC		12/00   Removed unnecessary parameter in VFWAWN *
 * R. Tian/SAIC		06/02	Changed test for VFWWCL			*
 * R. Tian/SAIC		06/02	Changed icpfip size to be MAXCNTY	*
 * A. Hardy/NCEP	 4/03   Initialized 'strin' to null		*
 * A. Hardy/NCEP	 5/03   Added VFGTOD;nulled 'strin' in vfspc	*
 * T. Piper/SAIC	02/04	Removed unused variable type_prompt	*
 * A. Hardy/NCEP	03/04	Cleaned up display for VFWWCL		*
 * H. Zeng/SAIC		09/04	Added VFWREP				*
 * A. Hardy/NCEP	11/04	Added ctb_pfread			*
 * T. Piper/SAIC	07/05	Removed VFWAWN and VFWPWN		*
 ***********************************************************************/
{
    int		   i;
    int            iret, numsub, cont,daywk;
    int		   ncyfip, icyfip[MAXCNTY], vhour, ehour, emin;
    char           fname[256], ifname[256], command[256];
    char           select[4], strin[100], ans[2];
    char	   preid[128], wtype[128], etime[13], fcstr[128];
    char           vampm[3], eampm[3], genday[50];
    char	   filnam[128], outstr[15000];
    char	   numc[4];
    char 	   newline[2];
/*---------------------------------------------------------------------*/
    in_bdta(&iret);

    cont = G_FALSE;
    ctb_pfread ( &iret );

    while ( cont == G_FALSE ) {
	printf ( "\n\n" );
	printf ( "   1 = VFGTTXT      2 = VFWSAW       3 = VFWSEL\n" );
	printf ( "   4 = VFWSEV       5 = VFCNSAW      6 = VFCNSEL\n" );
	printf ( "   7 = VFWWCL       8 = VFWOUI       9 =\n" );
	printf ( "  10 =             11 = VFWWCP      12 = VFSPC\n" );
	printf ( "  13 = VFGTOD      14 = VFWREP \n\n" );
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

		printf("Enter the ww.txt filename. \n");
		scanf( " %s", fname );

                vfgttxt ( fname, &iret );  
		printf ( "\nVFGTTXT : iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		vfwsaw  ( &iret );

		sprintf ( ifname, "WW%04d.SAW", spcinfo.wnum );
		sprintf( command, " cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		strin[0]  = '\0';
		printf ( "Are there CONTINUING watches (y/n) ? \n");
		scanf  ("%s",ans);

		if ( (strcmp ( ans,"y") == 0 ) || 
		                   (strcmp ( ans,"Y") == 0 ) ) {
		    printf("Enter the watch numbers separated by spaces.\n");
		    scanf  ("\n%[^\n]s",strin);
		}

		vfwsel  ( strin, &iret );

		sprintf ( ifname, "WW%04d.SEL", spcinfo.wnum );
		sprintf( command, "cat %s",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		vfwsev  ( &iret );

		sprintf ( ifname, "WW%04d.SEV", spcinfo.wnum );
		sprintf( command, " cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		vfcnsaw  ( &iret );

		sprintf ( ifname, "WW%04d.SAW.CNL", spcinfo.wnum );
		sprintf( command, " cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		vfcnsel  ( &iret );

		sprintf ( ifname, "WW%04d.SEL.CNL", spcinfo.wnum );
		sprintf( command, "cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {
		/*
		 * Get rid of any newline left previously.
 		 */
		fgets  ( newline, sizeof(newline), stdin);

		printf ( "Preliminary ID:" );
		fgets  ( preid, sizeof(preid), stdin );
		preid[strlen(preid)-1] = '\0';

		printf ( "Watch type:" );
		fgets  ( wtype, sizeof(wtype), stdin );
		wtype[strlen(wtype)-1] = '\0';

		printf ( "Expriation time (YYMMDD/HHMM):" );
		fgets  ( etime, sizeof(etime), stdin );
		etime[strlen(etime)-1] = '\0';

		printf ( "Forecaster name:" );
		fgets  ( fcstr, sizeof(fcstr), stdin );
		fcstr[strlen(fcstr)-1] = '\0';


		printf ( "Number of counties:" );
		fgets  ( numc, sizeof(numc), stdin );
		ncyfip = atoi(numc);

		for ( i = 0; i < ncyfip; i++ ) { 
		    printf ( "County %d FIPS codes:",i );
		    scanf ( "%d", &icyfip[i] );
		}

		vfsval ( wtype, etime, fcstr, ncyfip, icyfip, &iret );
		vfwwcl  ( preid, filnam, outstr, &iret );

		printf ( "\nFilnam:\n %s\n", filnam );
		printf ( "outstr:\n %s\n", outstr );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		vfwoui  ( &iret );

		sprintf ( ifname, "WW%04d.WOU", spcinfo.wnum );
		sprintf( command, " cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

		vfwwcp  ( &iret );

		sprintf ( ifname, "WW%04d.WCP", spcinfo.wnum );
		sprintf( command, " cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 12 ) {
		
		printf("Enter the ww.txt filename. \n");
		scanf( " %s", fname );

	        printf ( "Are there CONTINUING watches (y/n) ? \n");
	        scanf  ("%s",ans);

		strin[0] = '\0';
		if ( (strcmp ( ans,"y") == 0 ) || 
		                   (strcmp ( ans,"Y") == 0 ) ) {
		    printf("Enter the watch numbers separated by spaces.\n");
		    scanf  ("\n%[^\n]s",strin);
		    printf("\n");
		}

		vfspc  ( fname, strin, &iret );

		sprintf ( ifname, "WW%04d.*", spcinfo.wnum );
		sprintf( command, "cat %s ",ifname);
		printf("\n");
		system ( command );

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 13 ) {

		printf ( "Get general time of day string.\n\n" );

		printf ( "Enter VALID hour (1-12):\n" );
		scanf ( " %d", &vhour);
		printf ( "Enter ENDING hour (1-12):\n" );
		scanf ( " %d", &ehour );
		printf ( "Enter ENDING minutes(0-60):\n" );
		scanf ( " %d", &emin );
		printf ( "Enter VALID AM or PM designation:\n" );
		scanf ( " %s", vampm );
		printf ( "Enter ENDING AM or PM designation:\n" );
		scanf ( " %s", eampm );
		printf ( "Enter numerical VALID day of the week(1-SUN, 2-MON, etc):\n" );
		scanf ( " %d", &daywk );

		printf("%s %s\n",vampm, eampm);
		vfgtod (vhour, ehour, emin, vampm, eampm, daywk, 
		          genday, &iret );

		printf ( "\n %s \n", genday );
		printf ( "\nVFGTOD: iret = %d\n\n", iret );
	    }

/*---------------------------------------------------------------------*/
            if ( numsub == 14 ) {

		vfwrep (&iret );

		printf ( "\nVFWREP: iret = %d\n\n", iret );
	    }

/*---------------------------------------------------------------------*/
	}
	return(0);
}
