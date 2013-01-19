#include "geminc.h"
#include "gemprm.h"

#define MAXSTR 1024

/************************************************************************
 * datetime								*
 *									*
 * This program computes an output date/time and formats the string	*
 * of information. The user must input base date/time in the standard	*
 * GEMPAK format (YYMMDD/HHNN). With no other input, the program	*
 * returns the input string. The number of hours and minutes is the 	*
 * amount of time to add to, or subtract from, the base time. The	*
 * output format is a string of code sequences the same as used by the	*
 * UNIX date command. For example, %Y is the 4-digit year.		*
 * (See "man date" for a full list of codes.)				*
 *									*
 *  Command line:							*
 *    getdate [options] input_time [[hours[:minutes]] [output_format]] 	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/02						*
 * S. Jacobs/NCEP	 2/02	Added check for invalid date/time	*
 * S. Jacobs/NCEP	 2/02	Changed to use a help file		*
 ***********************************************************************/

int main ( int argc, char **argv )
{

	char		*prog = NULL, *format = NULL, outstr[MAXSTR],
			deffmt[] = "%y%m%d/%H%M";

	int		itarr[5], jtarr[5], iarr[2], minute,
			nprm, status, num, pagflg, ier, iret;

	time_t		now;
	struct tm	newtime;

	Boolean		sflg;

	/*
	 * These variables are used by getopt. Unset the error reporting.
	 */
	int		ch, errflg;

/*---------------------------------------------------------------------*/

	/*
	 * Set the time zone to Greenwich Mean Time.
	 */
	putenv ( "TZ=GMT" );

	/*
	 * Initialize the subtraction and help page flags.
	 */
	sflg   = False;
	pagflg = G_FALSE;

	/*
	 * Save the program name.
	 */
	prog = (char *) malloc ( strlen(argv[0]) );
	strcpy ( prog, argv[0] );

	/*
	 * Get the options and set the appropriate flags.
	 */
	opterr = 1;
	errflg = 0;
	while ( ( ch = getopt ( argc, argv, "sh" ) ) != EOF ) {
	    switch ( ch ) {
		case 's':
			sflg = True;
			break;
		case 'h':
			ip_help ( prog, &pagflg, &ier, strlen(prog) );
			exit (0);
			break;
		case '?':
			errflg++;
			break;
	    }
	}

	/*
	 * Adjust the number of arguments left on the command line.
	 */
	argc -= optind;
	argv += optind;

	/*
	 * If there are too many or not enough parameters, display the
	 * usage message and exit.
	 */
	if  ( errflg > 0 )  {
	    ip_help ( prog, &pagflg, &ier, strlen(prog) );
	    exit (1);
	}

	/*
	 * Process the user input.
	 */
	if  ( argc < 1 || argc > 3 )  {
	    ip_help ( prog, &pagflg, &ier, strlen(prog) );
	    exit (1);
	}
	nprm = 3;

	if  ( argc == nprm )  {
	    /*
	     * If the user entered all parameters, get the format string
	     * and convert the hours and minutes..
	     *
	     * If the format string is zero length, then use the
	     * default format.
	     */
	    if  ( strlen(argv[nprm-1]) < 1 )  {
		format = (char *) malloc ( strlen(deffmt) );
		strcpy ( format, deffmt );
	    }
	    else {
		format = (char *) malloc ( strlen(argv[nprm-1]) );
		strcpy ( format, argv[nprm-1] );
	    }
	    cst_ilst ( argv[nprm-2], ':', 0, 2, iarr, &num, &iret );
	}
	else if ( argc == nprm-1 ) {
	    /*
	     * If the user entered 1 less than the max parameters,
	     * convert the hours and minutes and set the output format
	     * to the default.
	     */
	    format = (char *) malloc ( strlen(deffmt) );
	    strcpy ( format, deffmt );
	    cst_ilst ( argv[nprm-2], ':', 0, 2, iarr, &num, &iret );
	}
	else if ( argc == nprm-2 ) {
	    /*
	     * If the user entered 2 less than the max parameters, set
	     * both the hours and the minutes to 0 and set the output
	     * format to the default.
	     */
	    format = (char *) malloc ( strlen(deffmt) );
	    strcpy ( format, deffmt );
	    iarr[0] = 0;
	    iarr[1] = 0;
	}

	/*
	 * Compute the number of minutes to add.
	 */
	minute = iarr[0] * 60 + iarr[1];

	/*
	 * Convert the input time to an standard integer array.
	 */
	ti_ctoi ( argv[0], itarr, &iret, strlen(argv[0]) );

	if  ( iret != 0 )  {
	    ip_help ( prog, &pagflg, &ier, strlen(prog) );
	    exit (1);
	}

	if  ( ! sflg )  {
	    /*
	     * Add the number of minutes.
	     */
	    ti_addm ( itarr, &minute, jtarr, &iret );
	}
	else {
	    /*
	     * Subtract the number of minutes.
	     */
	    ti_subm ( itarr, &minute, jtarr, &iret );
	}

	/*
	 * Set the values in the time structure.
	 */
	newtime.tm_year  = jtarr[0] - 1900;
	newtime.tm_mon   = jtarr[1] - 1;
	newtime.tm_mday  = jtarr[2];
	newtime.tm_hour  = jtarr[3];
	newtime.tm_min   = jtarr[4];
	newtime.tm_sec   = 0;
	newtime.tm_isdst = -1;

	/*
	 * Make a "C" time from the computed time.
	 */
	now = mktime ( &newtime );

	status = strftime ( outstr, MAXSTR, format, localtime(&now) );

	if  ( status > 0 )  {
	    printf ( "%s\n", outstr );
	}
	else {
	    ip_help ( prog, &pagflg, &ier, strlen(prog) );
	    exit (1);
	}

	/*
	 * Free the allocated memory.
 	 */
	if  ( format )  {
	    free ( format );
	}

	if  ( prog )  {
	    free ( prog );
	}

	return (0);

}
