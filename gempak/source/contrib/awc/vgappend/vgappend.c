#include "geminc.h"
#include "gemprm.h"

/************************************************************************
 * VGFAPPEND								*
 *									*
 * This program copies the contents of one or more existing VG files	*
 * and appends the elements to an existing output file.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/02	Modified AWC original program		*
 ***********************************************************************/

int main ( int argc , char **argv)

{
    int		i, ier;

/*---------------------------------------------------------------------*/

    if  ( argc >= 3 )  {

        for ( i = 2; i < argc; i++ )  {

	    cvg_cp ( argv[i], argv[1], G_FALSE, &ier );

        }
 
    }
    else {

	printf ( "\n\n" );
	printf ( " Usage: %s file1 file2 [...fileN]\n", argv[0] );
	printf ( "        Where file1 is the existing output file,\n" );
	printf ( "        and file2...fileN are the existing input files.\n" );
	printf ( "\n\n" );
	return ( 1 );

    }

    return ( 0 );

}
