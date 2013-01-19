#include "geminc.h"
#include "gemprm.h"

void cfl_dopn ( char *filnam, int *ifdes, int *iret )
/************************************************************************
 * cfl_dopn								*
 *                                                                      *
 * This function will open a file for read only access.			*
 *                                                                      *
 * cfl_dopn ( filnam, ifdes, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filnam 	char 		File name			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ifdes		int		File number                     *
 *	*iret		int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI		 2/93						*
 * J. Chou/EAI		 7/93						*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Jacobs/NCEP	 1/96	Renamed to CFL_DOPN			*
 * G. Krueger/EAI	 8/96	Match with FL library; Use ANSI form	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL; 

/*
 *	Open the file and check the status.
 */
	if ( ( *ifdes = open ( filnam, O_RDONLY ) ) < 0  ) {
	    printf ( "\n No such file: >> %s << \n", filnam );
	    *ifdes = 0;
	    *iret = -1;
	}
}
