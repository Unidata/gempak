#include "pscmn.h"

void pclear ( int *iret )
/************************************************************************
 * PCLEAR								*
 *									*
 * This subroutine clears the PostScript device by advancing to a new	*
 * page.								*
 *									*
 * PCLEAR  ( IRET )							*
 *									*
 * Output parameters:							*
 *      *IRET            INT         Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 ***********************************************************************/
{

	char	buff[88];
	int	lenb, ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Return if no file has been opened.
 */
	if  ( ( ! opnfil ) || ( ! psplot ) ) return ;

/*
 *	Force output to file.
 *	Go to next page and increment counter in common.
 */
	strcpy ( buff, " gsave showpage grestore\n" );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

	nnpage++; 
	sprintf ( buff, "%%%%Page:%5d  ?\n", nnpage );
	lenb = strlen ( buff );
	cfl_writ ( flun, lenb, (unsigned char *)buff, &ier );

}
