#include "geminc.h"
#include "gemprm.h"

int main ( int argc, char **argv )
/************************************************************************
 * wmostrip								*
 *									*
 * This program strips the wmo header from a GIF or FAX/G3 graphics	*
 * file. The header is considered to be anything before the keywords	*
 * "GIF" and "DFAX". The program takes two command line entries:	*
 *									*
 * 	wmostrip input_file output_file					*
 *									*
 * Where input_file is the file with the WMO header before the graphic	*
 * data, and output_file is the file to be created without the header.	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/02	Created					*
 ***********************************************************************/
{

char	newfil[256];
long	flen;
int	i, ihstrt, nread, nbytes, iret;
FILE	*fptr;
unsigned char	*buffer;

/*---------------------------------------------------------------------*/

    /*
     * Check the user input.
     */
    if  ( argc < 3 )  {
    	printf ( "\nUsage: %s input_file output_file\n\n", argv[0] );

	printf ( "This program removes the WMO header from a GIF or FAX/G3\n" );
	printf ( "graphic data file. The graphic data starts with either\n" );
	printf ( "\"GIF\" or \"DFAX\", respectively. This program will remove\n" );
	printf ( "everything before these keywords and write the remaining\n" );
	printf ( "data to the output file. The graphic data can then be\n" );
	printf ( "displayed using a standard plotting tool such as \"display\",\n" );
	printf ( "which is part of the ImageMagick package.\n\n" );

	printf ( "Examples:\n" );
	printf ( "wmostrip PGEE07_KKCI_20020420_0000 pgee07.fax\n" );
	printf ( "wmostrip QBBE00_PAWU_20020514_2105 qbbe00.gif\n\n" );

	return (1); 
    }

    /*
     * Check for a valid input file.
     */
    cfl_inqr ( argv[1], "", &flen, newfil, &iret );
    if  ( ( iret != 0 ) || ( flen == 0 ) )  {
    	printf ( "\n%s: Cannot find the input file\n\n", argv[0] );
	return (1); 
    }

    /*
     * Open the input file.
     */
    fptr = cfl_ropn ( newfil, "", &iret );
    if  ( ( iret != 0 ) || ( fptr == NULL ) )  {
    	printf ( "\n%s: Cannot open the input file\n\n", argv[0] );
	return (1); 
    }
    
    /*
     * Read the entire input file into the buffer.
     */
    buffer = (unsigned char *) malloc ( (size_t)flen * sizeof(unsigned char) );

    cfl_read ( fptr, (int)flen, buffer, &nread, &iret );
    if  ( ( iret != 0 ) || ( nread == 0 ) )  {
    	printf ( "\n%s: Cannot read the input file\n\n", argv[0] );
	cfl_clos ( fptr, &iret );
	return (1); 
    }

    /*
     * Close the input file.
     */
    cfl_clos ( fptr, &iret );

    /*
     * Open the output file.
     */
    fptr = cfl_wopn ( argv[2], &iret );
    if  ( ( iret != 0 ) || ( fptr == NULL ) )  {
    	printf ( "\n%s: Cannot open the output file\n\n", argv[0] );
	return (1); 
    }

    /*
     * Determine where the actual data starts.
     * Find either the "GIF" or "DFAX" header and cut everything
     * before that.
     */
    for ( i = 0; i < (int)flen; i++ ) {
    	if  ( ( buffer[i]   == 'G' &&
	        buffer[i+1] == 'I' &&
	        buffer[i+2] == 'F' )
	        ||
    	      ( buffer[i]   == 'D' &&
	        buffer[i+1] == 'F' &&
	        buffer[i+2] == 'A' &&
	        buffer[i+3] == 'X' ) )  {
	    ihstrt = i;
	    break;
	}
    }

    nbytes = (int)flen - ihstrt;
    if  ( nbytes <= 0 ) {
    	printf ( "\n%s: The output file size is incorrect\n\n", argv[0] );
	cfl_clos ( fptr, &iret );
	return (1); 
    }

    /*
     * Write the output data to the output file.
     */
    cfl_writ ( fptr, nbytes, &buffer[ihstrt], &iret );
    if  ( iret != 0 ) {
    	printf ( "\n%s: Cannot write to the output file\n\n", argv[0] );
	cfl_clos ( fptr, &iret );
	return (1); 
    }

    /*
     * Close the output file.
     */
    cfl_clos ( fptr, &iret );

    return (0); 
}
