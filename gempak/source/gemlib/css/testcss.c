#include "geminc.h"
#include "gemprm.h"

int main ( void )
/************************************************************************
 * TESTCSS								*
 *									*
 * This program tests the GEMLIB "CSS" functions.			*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 9/97						*
 * D. Kidwell/NCEP	 2/99	Julian -> days since 1 January		*
 * S. Jacobs/NCEP	10/99	Added CSS_GTIM				*
 * S. Jacobs/NCEP	11/99	Added CSS_MTYP				*
 * A. Hardy/NCEP	 6/03   Added time zone string to CSS_DATE	*
 * B. Yin/SAIC  	03/04	Changed css_gtim, css_date calling seq  *
 * B. Yin/SAIC  	03/04	Added event clock funtions		*
 ***********************************************************************/
{
	int		iyear, imon, iday, ihour, imin, isec, julian,
			cont, iret, numsub, itype, mchtyp;

	char		select[LLSCRN], filnam[LLSCRN], file[LLSCRN];
	char		zone[4], evttm[LLSCRN];

	dattm_t		dattim;

	float		evtrate, sysrate;

/*---------------------------------------------------------------------*/
	cont = G_FALSE;

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( " 1 = CSS_ENVR  2 = CSS_DATE  3 = CSS_GTIM\n" );
	    printf ( " 4 = CSS_MTYP  5 = CSS_EVTSETINITTM  6 = CSS_EVTGETINITTM\n" );
	    printf ( " 7 = CSS_EVTSETSPEED  8 = CSS_EVTGETSPEED  9 = CSS_EVTSETCURTM\n" );
	    printf ( "10 = CSS_EVTPAUSE    11 = CSS_EVTRESUME   12 = CSS_EVTCLEAR\n\n" );
	    printf ( "13 = CSS_EVTDUMPVARS\n\n" );
	    printf ( "\n" );
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

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );

		css_envr ( filnam, file, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCSS_ENVR: file = %s\n", file );
		}

		printf ( "\nCSS_ENVR: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( "Enter the type of time (0=Local, 1=GMT):\n" );
		scanf ( " %d", &itype );

		css_date ( &itype, &iyear, &imon, &iday, &ihour, &imin,
			   &isec, &julian, zone, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCSS_DATE:  iyear, imon, iday:  %d/%d/%d\n",
			     iyear, imon, iday );
		    printf ( "CSS_DATE:  ihour, imin, isec:  %d:%d:%d\n",
			     ihour, imin, isec );
		    printf ( "CSS_DATE:  days since 1 January:  %d\n", 
			     julian );

		    printf ( "CSS_DATE:  time zone :  %s\n", zone);
		}

		printf ( "\nCSS_DATE:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Enter the type of time (0=Local, 1=GMT):\n" );
		scanf ( " %d", &itype );

		css_gtim ( &itype, dattim, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCSS_GTIM:  dattim:  %s\n", dattim );
		}

		printf ( "\nCSS_GTIM:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		css_mtyp ( &mchtyp, &iret );

		if  ( iret >= 0 )  {
		    if  ( mchtyp )  {
			printf ( "\nCSS_MTYP:  Big Endian\n" );
		    }
		    else {
			printf ( "\nCSS_MTYP:  Little Endian\n" );
		    }
		}

		printf ( "\nCSS_MTYP:  iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		printf ( "Enter the initial event clock time in GEMPAK format:\n" );
		scanf ( " %s", evttm );

		css_evtsetinittm( evttm, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nInitial event clock time set!\n" );
		}

		printf ( "\nCSS_EVTSETINITTM:  iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		css_evtgetinittm( evttm, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nInitial event clock time: %s\n", evttm );
		}

		printf ( "\nCSS_EVTGETINITTM:  iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		printf ( "Enter the event clock rate:\n" );
		scanf ( " %f", &evtrate );
		printf ( "Enter the system clock rate:\n" );
		scanf ( " %f", &sysrate );

		css_evtsetspeed( &evtrate, &sysrate, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nInitial event clock time set!\n" );
		}

		printf ( "\nCSS_EVTSETSPEED:  iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		css_evtgetspeed( &evtrate, &sysrate, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nEvent clock rate: %f\n", evtrate );
		    printf ( "\nSystem clock rate: %f\n", sysrate );
		}

		printf ( "\nCSS_EVTGETSPEED:  iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

		printf ( "Enter the current event clock time in GEMPAK format:\n" );
		scanf ( "%s", evttm );

		css_evtsetcurtm( evttm, &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCurrent event clock time set!\n" );
		}

		printf ( "\nCSS_EVTSETCURTM:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {

		css_evtpause( &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCurrent event clock has been paused!\n" );
		}

		printf ( "\nCSS_EVTPAUSE:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

		css_evtresume( &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nCurrent event clock has been resumed!\n" );
		}

		printf ( "\nCSS_EVTRESUME:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 12 ) {

		css_evtclear( &iret );

		if  ( iret >= 0 )  {
		    printf ( "\nThe event clock has been turned off!\n" );
		}

		printf ( "\nCSS_EVTCLEAR:  iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 13 ) {
		css_evtdumpvars();
	    }
/*---------------------------------------------------------------------*/
	}
	return(0);
}
