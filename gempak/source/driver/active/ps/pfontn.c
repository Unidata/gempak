#include "pscmn.h"

void pfontn ( int itxfn, char *fntnam )
/************************************************************************
 * PFONTN 								*
 * 									*
 * This subroutine takes the font number requested and returns a	*
 * PostScript font name.						*
 * 									*
 * PFONTN  ( ITXFN, FNTNAM )						*
 *									*
 * Input parameters:							*
 * 	ITXFN		INT		Text font			*
 *									*
 * Output parameters:							*
 *	*FNTNAM		CHAR		Font name			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	12/90						*
 * A. Chang/EAI		 2/94						*
 ***********************************************************************/
{

	char	fonts[12][24];

/*---------------------------------------------------------------------*/

/*
 *	Get font name from list.
 */
	strcpy ( fonts[ 0] , "Courier" );
	strcpy ( fonts[ 1] , "Helvetica" );
	strcpy ( fonts[ 2] , "Times-Roman" );
     	strcpy ( fonts[ 3] , "Courier-Oblique" ); 
     	strcpy ( fonts[ 4] , "Helvetica-Oblique" );
    	strcpy ( fonts[ 5] , "Times-Italic" ); 
 	strcpy ( fonts[ 6] , "Courier-Bold" );
	strcpy ( fonts[ 7] , "Helvetica-Bold" ); 
	strcpy ( fonts[ 8] , "Times-Bold" );
	strcpy ( fonts[ 9] , "Courier-BoldOblique" );
	strcpy ( fonts[10] , "Helvetica-BoldOblique" ); 
	strcpy ( fonts[11] , "Times-BoldItalic" );

	if  ( ( itxfn < 1 ) || ( itxfn > 12 ) )  {
	    strcpy ( fntnam, fonts[0] );
	}
	else {
	    strcpy ( fntnam, fonts[(itxfn) - 1] );
	}
}
