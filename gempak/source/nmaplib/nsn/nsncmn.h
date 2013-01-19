/************************************************************************
 * nsncmn								*
 *									*
 * This header file contains the global varibles and structures for	*
 * the NSN library.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSND->MAXTMPLT and MXTIME to gemprm.h	*
 * M. Li/GSC		07/00	Added snddtSave				*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * T. Piper/SAIC        06/03   Removed nsnd                            *
 * E. Safford/SAIC	01/08	include proto_nmaplib.h, proto_xw.h	*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "proto_nmaplib.h"
#include "proto_xw.h"


typedef struct {
	char	alias[81];
	char	cycle[81];
	char	parms[81];
	char	color[81];
	char	level[81];
	char	vcord[81];
	char	filter[81];
	char	txtatt[81];
	int	isbcat;
} nsncmn_t;

#ifdef NSN_GLOBAL

	int		indsnd[MAXTMPLT];
				/* Indices for structure array */

	nsncmn_t	snddt[MAXTMPLT];
				/* Array of structures of
				   SND attributes */

	nsncmn_t        snddtSave[MAXTMPLT];
				/* Save array of structure */

#else

	extern int		indsnd[MAXTMPLT];
	extern nsncmn_t		snddt[MAXTMPLT];
	extern nsncmn_t        	snddtSave[MAXTMPLT];

#endif
