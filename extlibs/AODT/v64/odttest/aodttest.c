#include	<stdlib.h>
#include	<stdio.h>
#include	"../inc/odtapi.h"

#define	AODT_TRUE	1
#define AODT_FALSE	0

int main (void)
/************************************************************************
 * TESTAODT								*
 *									*
 * This program tests the AODT library functions.			*
 *									*
 **									*
 * Log:									*
 ***********************************************************************/
{
    int		cont, numsub;
    char	select[8], histfile[128], topofile[128];

/*---------------------------------------------------------------------*/

    cont = AODT_FALSE;

    while ( cont == AODT_FALSE ) {
	printf ( "\n\n" );
	printf ( "   1 = AODT_SETHISTORYFILE	2 = AODT_GETHISTOYRFILE\n" );
	printf ( "   3 = AODT_SETTOPOFILE   	4 = AODT_GETTOPOFILE   \n" );
	printf ( "\n" );
	printf ( "Select a subroutine number or type EXIT: " );
	scanf ( " %s", select );
	switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = AODT_TRUE;
		default:
			numsub = atoi ( select );
			break;
	}

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		printf ( "Enter history filename: ");
		scanf ( " %s", histfile );

		aodt_sethistoryfile ( histfile );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		aodt_gethistoryfile ( histfile );

		printf("History file is %s\n", histfile );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Enter topography filename: ");
		scanf ( " %s", topofile );

		aodt_settopofile ( topofile );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		aodt_gettopofile ( topofile );

		printf("Topography file is %s\n", topofile );

	    }
/*---------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
	}

    return ( 0 );
}
