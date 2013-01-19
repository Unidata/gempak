#include "geminc.h"
#include "gemprm.h"

/************************************************************************
 * naminfo.c                                                            *
 *                                                                      *
 * This module contains the main program of naminfo.                    *
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of naminfo.                       *
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc , char **argv)
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of naminfo.                                           	*
 *									*
 * This program will create an information file (table) about the	*
 * cities table which has been sorted alphabetically.  This information	*
 * is printed out as standard output (via printf) and must be 		*
 * re-directed to the proper information file within the script.	*
 *                                                                      *
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/98						*
 ***********************************************************************/

{
char	filename[128], buffer[80], string[512], str[32];
char	alphabet[64], ab1, ab2;
char	c2[3], c2test[3];
int	nstart, nlines, reclen, hdrlen, ier;
size_t  ii, jj;

FILE	*fptr;
/*---------------------------------------------------------------------*/

	/*
	 *  Print out the name of the (alphabetically sorted) city file.
	 */
	strcpy ( filename, argv[1] );
	printf("!\n!    CITY FILENAME (sorted alphabetically)\n%s\n!\n",
		filename );

	/*
	 *  Alphabet characters must be in increasing ASCII order.
	 *  Print it out.
	 */
	strcpy ( alphabet, "'.1ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
	printf("!    CHARACTER COMBINATIONS\n%s\n!\n", alphabet );

	fptr = (FILE *)cfl_tbop ( filename, "cities", &ier );

	cfl_trln ( fptr, sizeof(buffer), buffer, &ier );

	reclen = strlen(buffer) + 1;
	hdrlen = (int)ftell( fptr ) - reclen;

	/*
	 *  Print out header length and (fixed) record length.
	 */
	printf("!    HEADER LENGTH (bytes)\n%d\n!\n", hdrlen );
	printf("!    RECORD LENGTH (bytes)\n%d\n!\n", reclen );

	/*
	 *  Scan the file looking for two-character combinations of
	 *  the chars in the alphabet string, ie., AA, AB, AC, AD, etc.
	 *  Print out, in sequence, the combination, starting record number
	 *  and number of records in format combo/strec/nrec, eg.,
	 *  AA/strecAA/nrecAA AB/strecAB/nrecAB AC/strecAC/nrecAC  etc...
	 */
	nstart = 0;
	nlines = 0;
	for ( ii = 0; ii < strlen(alphabet); ii++ )  {

		ab1 = alphabet[ii];
		string[0] = '\0';

		for ( jj = 0; jj < strlen(alphabet); jj++ )  {

			ab2 = alphabet[jj];
			sprintf( c2, "%c%c", ab1, ab2 );

			strncpy ( c2test, buffer, 2 );

			nstart = nstart + nlines;
			nlines = 0;

			if ( strcmp(c2,c2test) == 0 )  {

			    ier = 0;
			    while ( strcmp(c2,c2test) == 0 && ier == 0 )  {
				nlines++;
				cfl_trln ( fptr, sizeof(buffer), buffer, &ier );
				if ( ier == 0 )  strncpy ( c2test, buffer, 2 );
			    }
			}

			if ( strlen(string) > 0 )  strcat( string, " " );
			sprintf ( str, "%s/%d/%d", c2, nstart, nlines );
			strcat( string, str );

			if ( strlen(string) > 68 )  {
		                printf("%s\n", string );
				string[0] = '\0';
			}
		}

		if ( strlen(string) > 0 )  {
			printf("%s\n!\n", string );
		}
		else  {
			printf("!\n");
		}
	}
	return(0);
}
