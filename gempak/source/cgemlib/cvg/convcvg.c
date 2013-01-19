#include "cvgcmn.h"

int main ( int argc, char *argv[] )
/************************************************************************
 * CONVCVG								*
 *									*
 * This program converts differing versions of VGF files		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/97	Created w/ conv  5.4.3.e -> 5.4.3.f     *
 * D.W.Plummer/NCEP	 6/98	Added conversion 5.4.3.i -> 5.4.3.h	*
 * F. J. Yen/NCEP	 1/99	Added conversion 5.4.3.k -> 5.4.3.j	*
 * F. J. Yen/NCEP	 2/99   Added conversion 5.4.3.l -> 5.4.3.j	*
 * S. Jacobs/NCEP	 3/99   Added conversion 5.4.3.m -> 5.4.3.j	*
 * A. Hardy/GSC		 4/99   Added conversion 5.4.3.n -> 5.4.3.j	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
int	iret;
char	ifname[256], ofname[256], ctype[16];

/*---------------------------------------------------------------------*/

    if ( argc < 2 )  {
	printf("Usage: convcvg type ... \n" );
        printf("       type = 3e3f, 3i3h, 3k3j, 3l3j, 3m3j, etc.\n" );
    }
    else  {

	/*
	 *  Argument #1 indicates the type of conversion.
	 */

	strcpy( ctype, argv[1] );

	if ( strcmp( ctype, "3e3f" ) == 0 )  {

	    /*
	     *  Convert version 5.4.3.e to version 5.4.3.f
	     */
	    if ( argc < 3 )  {
	        printf("Usage: convcvg 3e3f filename\n" );
                printf("   filename = input/output filename   \n" );
	    }
	    else  {

	        strcpy( ifname, argv[2] );
	        cvg_c3e3f( ifname, &iret );

	    }

	}
	else if ( strcmp( ctype, "3i3h" ) == 0 )  {

	    /*
	     *  Convert version 5.4.3.i to version 5.4.3.h
	     */
	    if ( argc < 4 )  {
	        printf("Usage: convcvg 3i3h infile outfile\n" );
                printf("     infile = input filename          \n" );
                printf("    outfile = output filename         \n" );
	    }
	    else  {

	        strcpy( ifname, argv[2] );
	        strcpy( ofname, argv[3] );
	        cvg_c3i3h( ifname, ofname, &iret );

	    }

	}
	else if ( strcmp( ctype, "3k3j" ) == 0 ||
		  strcmp( ctype, "3l3j" ) == 0 ||
		  strcmp( ctype, "3m3j" ) == 0 ||
		  strcmp( ctype, "3n3j" ) == 0 )  {

	    /*
	     *  Convert version 5.4.3.k to version 5.4.3.j ...
	     *  Also, version 5.4.3.l to version 5.4.3.j   ...
	     *  Also, version 5.4.3.m to version 5.4.3.j 
	     *  Also, version 5.4.3.n to version 5.4.3.j 
	     */
	    if ( argc < 4 )  {
	        printf("Usage: convcvg %s infile outfile\n", ctype );
                printf("     infile = input filename          \n" );
                printf("    outfile = output filename         \n" );
	    }
	    else  {

	        strcpy( ifname, argv[2] );
	        strcpy( ofname, argv[3] );
	        cvg_c3k3j( ifname, ofname, &iret );

	    }

	}

    }
    return(0);
}
