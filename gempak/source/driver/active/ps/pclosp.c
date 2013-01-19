#include "pscmn.h"

void pclosp ( int *iret )
/************************************************************************
 * PCLOSP                                                               *
 *                                                                      *
 * This subroutine closes the plot file.  Before closing the file, all  *
 * buffered data must be flushed.                                       *
 *                                                                      *
 * PCLOSP  ( IRET )                                                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *IRET            INT	        Return code                     *
 **                                                                     *
 * Log                                                                  *
 * A. Chang/EAI		 2/94						*
 * S. Jacobs/NCEP	 1/97	Added a free for the fontus array	*
 ***********************************************************************/
{

	char 		trail[1024], buff[88];
	unsigned 	i;
	int		lent, ier;

/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Restore printer to print mode and close plot file.
 */
	if  ( opnfil )  {

	    strcpy ( trail, " gsave showpage grestore\n" );
	    strcat ( trail, " restore\n" );
	    strcat ( trail, " %%Trailer\n" );

	    sprintf ( buff, " %%Pages: %5d\n", nnpage );
	    strcat ( trail, buff );

/*
 *	    Write out the fonts used.
 */
	    strcat ( trail, " %%DocumentsFont:\n" );


	    for ( i = 0; i < nfontu; i++ ) {
		sprintf ( buff, " %%%%+ %s\n", fontus[i] );
		strcat ( trail, buff );
		free ( fontus[i] );
	    }

	    lent = strlen ( trail );
	    cfl_writ ( flun, lent, (unsigned char *)trail, &ier );
		
	    cfl_clos ( flun, &ier );

	    opnfil = G_FALSE; 
	    iawdth = 0;
	    nfontu = 0;
	    txsizs = 0.;
	    isfont = 0;
	}

}
