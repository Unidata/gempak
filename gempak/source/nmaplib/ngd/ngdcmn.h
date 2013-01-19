/************************************************************************
 * ngdcmn								*
 *									*
 * This header file contains the global varibles and structures for	*
 * the NGD library.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Law/GSC		06/00	MAXGRID->MAXTMPLT and MXTIME to gemprm.h*
 * M. Li/GSC		07/00	Added grddtSave				*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * T. Piper/SAIC        06/03   Removed ngrd                            *
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "proto_nmaplib.h"
#include "proto_xw.h"


typedef struct {
	char	alias[81];
	char	cycle[81];
	char	rstfil[81];
	int	isbcat;
} ngdcmn_t;

#ifdef NGD_GLOBAL

	int		indgrd[MAXTMPLT];
				/* Indices for structure array */

	ngdcmn_t	grddt[MAXTMPLT];
				/* Array of structures of
				   GRID attributes */

	ngdcmn_t	grddtSave[MAXTMPLT];
				/* Save array of structure */

#else

	extern int		indgrd[MAXTMPLT];
	extern ngdcmn_t		grddt[MAXTMPLT];
	extern ngdcmn_t        	grddtSave[MAXTMPLT];

#endif
