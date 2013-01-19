#define GB_DEF
#include "gbcmn.h"

void gb_open ( char *filnam, int *lenfil, char *idxnam, 
						int *lenidx, int *iret )
/************************************************************************
 * gb_open                                                              *
 *                                                                      *
 * This function will open a GRIB file, and its INDEX file, for         *
 * read only access.                                                    *
 *									*
 * gb_open ( filnam, lenfil, idxnam, lenidx, iret )			*
 *									*
 * Input parameters:							*
 *	*filnam		char		GRIB file name			*
 *	*lenfil		int		GRIB length			*
 *	*idxnam		char		INDEX file name			*
 *	*lenidx		int		INDEX length			*
 *                                                                      *
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * J. Chou/EAI           7/93						*
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 7/94		Added init of indxfd = 0	*
 * S. Jacobs/NCEP	 1/96		Changed DA_OPEN to CFL_DOPN	*
 * D.W.Plummer/NCEP	 3/96		Changed cfl_ calling sequence	*
 ***********************************************************************/
{
char	defdir[1], tname[40];
/*---------------------------------------------------------------------*/

	*iret = 0;
	defdir[0] = CHNULL;

	/*
	 *	Set the end of the filename strings to NULL.
	 */

	filnam[*lenfil] = CHNULL;
	idxnam[*lenidx] = CHNULL;
	strcpy(gbfile.name, filnam);
	strcpy(infile.name, idxnam);

	/*
	 *	Try to open the INDEX file. If the open is successful,
	 *	call routine to read entire contents into structure.
	 */

	infile.fptr = NULL;

	if  ( strlen ( infile.name ) > (size_t)0 ) {
	    infile.fptr = (FILE*)cfl_ropn(infile.name, defdir, iret);
	    if ( *iret != 0 )  {
		printf("WARNING : Unable to open index file %s\n", infile.name);
	    }
	    else  {
		gb_rindx(infile.fptr, iret);
		strcpy(tname, &(strrchr(gbfile.name, '/')[1]));
		if ( strcmp(infile.basename, tname) != 0 ) {
		  printf("WARNING : GBFILE basename (%s) does not match INDXFL internal basename (%s).\n", tname, infile.basename);
		}
	    }
	}

	    /*
	     *	Open the GRIB file.
	     */

        gbfile.fptr = (FILE*)cfl_ropn(gbfile.name, defdir, iret);

	if  ( *iret != 0 )
	    *iret = -15;

	cursor  = 0;
	cursor1 = 0;

}
