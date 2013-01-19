#include "geminc.h"
#include "gemprm.h"

void cst_unp2 ( char *string, char *outstr, int *iret );

int main ( int argc, char **argv )
/************************************************************************
 * TRAN2FOS								*
 * 									*
 * This program converts a file in the TRAN format to a file in a 	*
 * format suitable for use in the GEMPAK decoders. The decoders use	*
 * files that have Family of Services (FOS) control characters. The 	*
 * TRAN start-of-product, end-of-product and end-of-line are converted	*
 * to the appropriate control characters for FOS.			*
 * 									*
 * Log:									*
 * S. Jacobs/NCEP	10/03	Created					*
 ***********************************************************************/
{

    int		nread, iret;
    char	newfil[256], srch[256], rep[256];
    long	flen;
    char	*buffer;
    FILE	*fptr, *fout;

/*---------------------------------------------------------------------*/

    /*
     * Get input and output file names from the command line input.
     */
    if  ( argc < 2 )  {
	printf ( "\nUsage: %s input_file output_file\n\n", argv[0] );
	return ( 1 );
    }

    /*
     * Get the file size and allocate enough space to hold
     * the entire file.
     */
    cfl_inqr ( argv[1], "", &flen, newfil, &iret );
    buffer = (char *) malloc ( flen * sizeof(char) );

    /*
     * Open the input file, read the entire contents into the 
     * buffer, and close the file.
     */
    fptr = cfl_ropn ( argv[1], "", &iret );
    cfl_read ( fptr, (int)flen, (unsigned char *)buffer, &nread, &iret );
    cfl_clos ( fptr, &iret );

    /*
     * Remove unprintable characters.
     */
/*     printf ( "Removing unprintable characters\n" ); */
    cst_unp2 ( buffer, buffer, &iret );

    /*
     * Construct the replacement string.
     * Replace the search string with the replacement string.
     */
    iret = 0;
    rep[0] = CHCTLA;
    rep[1] = CHCR;
    rep[2] = CHCR;
    rep[3] = CHLF;
    rep[4] = '0';
    rep[5] = '0';
    rep[6] = '0';
    rep[7] = CHCR;
    rep[8] = CHCR;
    rep[9] = CHLF;
    rep[10] = CHNULL;
    strcpy ( srch, "'100000                                 " );
/*     printf ( "Starting search for CNTRL-A\n" ); */
    while ( strstr ( buffer, srch ) != NULL )  {
	cst_rpst ( buffer, srch, rep, buffer, &iret );
    }

    /*
     * Construct the replacement string.
     * Replace the search string with the replacement string.
     */
    iret = 0;
    rep[0] = CHCR;
    rep[1] = CHCR;
    rep[2] = CHLF;
    rep[3] = CHNULL;
    strcpy ( srch, "<<@" );
/*     printf ( "Starting search for EOL\n" ); */
    while ( strstr ( buffer, srch ) != NULL )  {
	cst_rpst ( buffer, srch, rep, buffer, &iret );
    }

    /*
     * Construct the replacement string.
     * Replace the search string with the replacement string.
     */
    iret = 0;
    rep[0] = CHCR;
    rep[1] = CHCR;
    rep[2] = CHLF;
    rep[3] = CHCTLC;
    rep[4] = CHNULL;
    strcpy ( srch, "%" );
/*     printf ( "Starting search for CNTRL-C\n" ); */
    while ( strstr ( buffer, srch ) != NULL )  {
	cst_rpst ( buffer, srch, rep, buffer, &iret );
    }

    /*
     * Open the output file, write the buffer and close the file.
     */
    fout = cfl_wopn ( argv[2], &iret );
    fprintf ( fout, "%s", buffer );
    cfl_clos ( fout, &iret );

    return (0);
}
