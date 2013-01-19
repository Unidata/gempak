#include "xwcmn.h"

void xdwtbl ( int *iret )
/************************************************************************
 * xdwtbl								*
 *									*
 * This subroutine reads the dwell table file "dwell.tbl". 		*
 *									*
 * xdwtbl ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *		G_NORMAL = return code					*
 *		G_NDWTBL = cannot find dwell table, use defaults	*
 **									*
 * Log:									*
 * S. Jacobs/NMC         7/94           xsattbl.c                	*
 * C. Lin/EAI	 	 7/94   modified for reading dwell rate table   *
 * C. Lin/EAI	 	 2/95   restructure; add error code	   	*
 *				use sprintf				*
 * D. Keiser/GSC	12/95	added path for new directory structure	*
 * T. Piper/GSC		11/00	removed satimg.h - not used		*
 ***********************************************************************/
{
	FILE	*fp;
	char    filnam[40], newfil[120], *dirsym;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	strcpy ( filnam, "loop/dwell.tbl" );

	/*
 	 * Try to open the table file in the current directory
 	 * first. If that fails try the GEMTBL directory.
 	 */
	if ( ( fp=fopen(filnam, "r" ) ) == NULL ) {

	    dirsym = getenv("GEMTBL");
	    sprintf( newfil, "%s/%s", dirsym, filnam);

	    if ( ( fp=fopen(newfil, "r" ) ) == NULL ) {

		/*
		 * set defaults
		 */
		dwell[0] = 2.00;
		dwell[1] = 1.00;
		dwell[2] = 0.50;
		dwell[3] = 0.10;
		dwell[4] = 0.05;

		*iret = G_NDWTBL;
		return;
	    }
	}

	fscanf( fp, "%f %f %f %f %f", 
			&(dwell[0]), &(dwell[1]),
			&(dwell[2]), &(dwell[3]), &(dwell[4]) );

	/*
 	 * Close the table file.
 	 */
	fclose(fp);

}
