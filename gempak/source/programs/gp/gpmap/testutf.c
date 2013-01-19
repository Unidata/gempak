#include "geminc.h"
#include "gemprm.h"

int main ( void )
/************************************************************************
 * TESTUTF								*
 *									*
 * This program tests the CGEMLIB "UTF" functions.			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * D. Keiser/GSC	 2/97	Rm option of entrng fnam to read	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*	
 ***********************************************************************/
{
	int		cont, iret, numsub, gs, pi, add, ier;
	int		day, month, year, time, pdc, shift_x, shift_y;
	int		zm, num, start, flag, level, elevel, bufflg;
	int		dttmfkg;
	unsigned int	imax, jmax, imaxad, jmaxad;
	long		size, bytesr, newsiz;
	char		filnam[LLSCRN], select[LLSCRN], ans[LLSCRN];
	char		grp[4], string[2];
	unsigned char	*file;
	FILE		*filptr;
/*---------------------------------------------------------------------*/
	elevel = 0;
	bufflg = 0;
	dttmfkg = 1;
	iret = 0;
	size = 0;
	bytesr = 0;
	newsiz = 0;
	num = 0;
	filptr = 0;
	cont = G_FALSE;
	file = CHNULL;
	strcpy(grp, "UTF");
	strcpy(string, " ");
	level = 0;

	in_bdta(&ier);
	er_stat(&elevel, &bufflg, &dttmfkg, &ier);

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( "   1 = UTF_OPEN   2 = UTF_SIZE   3 = UTF_READ\n" );
	    printf ( "   4 = UTF_STRIP  5 = UTF_GPHGD  6 = UTF_DUMP\n" );
	    printf ( "   7 = UTF_CLOS\n" );
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

		if ( filptr != NULL ) {
		    printf("\n\nOnly one file at a time can be open!\n");
		    printf("Please close currently open file before");
		    printf(" attempting to open another.\n");
		}
		else {
		    printf ( "Enter the UTF file name to open:\n" );
		    scanf ( " %s", filnam );
		    utf_open ( filnam, (int *)filptr, &iret );
		    flag = G_FALSE;

		    printf ( "\nUTF_OPEN: iret = %d\n\n", iret );
		    er_lmsg ( &level, grp, &iret, filnam, &ier,
				strlen(grp), strlen(filnam) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( "Enter the UTF file name to size:\n" );
		scanf ( " %s", filnam );
		utf_size ( filnam, &size, &iret );

		printf ( "\nUTF_SIZE: iret = %d\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
			  strlen(filnam) );
		if ( iret == 0 )
		    printf ( "          size = %ld bytes\n\n", size );
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		if ( bytesr == 0 ) {
		    file = (unsigned char *) malloc(size);

		    utf_read ( filptr, size, file, &bytesr, &iret );

		    printf ( "\nUTF_READ: iret = %d\n\n", iret );
		    er_lmsg ( &level, grp, &iret, filnam, &ier,
				strlen(grp), strlen(filnam) );
		    if ( iret == 0 )
			printf ( "       # of bytes read = %ld\n\n",
								bytesr );
		}
		else {
		    printf ( "Buffer has already been loaded.\n" );
		    printf ( "If you wish to read another file, " );
		    printf ( "close current file first.\n" );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		utf_strip ( file, bytesr, &newsiz, &iret );

		printf ( "\nUTF_STRIP: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
			  strlen(filnam) );
		if ( iret == 0 )
		    printf ( "          new size = %ld bytes\n\n",
								newsiz );
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		utf_gphgd ( file, newsiz, &shift_x, &shift_y, &pi, &gs,
			    &imax, &jmax, &imaxad, &jmaxad, &day, &month,
			    &year, &time, &pdc, &add, &iret );

		printf ( "\nUTF_GPHGD: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, string, &ier, strlen(grp),
			  strlen(string) );

		if ( iret == 0 ) {
		    if ( pi == 1 )
			printf ( "Requires map background: YES\n" );
		    else
			printf ( "Requires map background: NO\n" );
		    printf( "Geography scale: %i\n", gs );
		    printf( "Horizontal graphic width: %i pixels\n", 
								imax );
		    printf( "Vertical graphic width: %i pixels\n",
								jmax );
		    printf( "Adjusted horizontal graphic width: " );
		    printf( "%i pixels\n", imaxad );
		    printf( "Adjusted vertical graphic width: " );
		    printf( "%i pixels\n", jmaxad );
		    printf( "Horizontal shift factor: %i \n", shift_x );
		    printf( "Vertical shift factor: %i \n", shift_y );
		    printf( "Product Valid: \n" );
		    printf( "Day: %i Month: %i Year: %i Time: %i \n",
						day, month, year, time );
		    printf( "Product display code: %i \n", pdc );
		    flag = G_TRUE;
		}
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		printf ( "Output decoded information?(y/n)\n");
		scanf ( " %s", ans );
		printf ( "Enter the byte number to start the dump:\n" );
		scanf ( " %d", &start );
		printf ( "Enter number of bytes to dump:\n" );
		printf ( "If outputting decoded information, range " );
		printf ( "must include record header(s)\n" );
		scanf ( " %d", &num );
		zm = 1;
		utf_dump ( file, newsiz, start, num, ans, zm, shift_x,
			   shift_y, flag, &iret );

		printf ( "\nUTF_DUMP: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, string, &ier, strlen(grp),
			  strlen(string) );
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		utf_clos ( filptr, &iret );
		filptr = 0;
		bytesr = 0;
		size = 0;
		free(file);

		printf ( "\nUTF_CLOS: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
			  strlen(filnam) );

	    }

/*---------------------------------------------------------------------*/
	}
	free(file);

	return 0;

}
