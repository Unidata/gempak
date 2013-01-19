/************************************************************************
 * nsfcmn								*
 *									*
 * This header file contains the global varibles and structures for	*
 * the NSF library.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSFC->MAXTMPLT and MXTIME to gemprm.h	*
 * M. Li/GSC		07/00	Added sfcdtSave				*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * T. Piper/SAIC        06/03   Removed nsfc                            *
 * E. Safford/SAIC	01/08	included proto_nmaplib.h, proto_xw.h	*
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
	char	filter[81];
	char	txtatt[81];
	int	isbcat;
} nsfcmn_t;

#ifdef NSF_GLOBAL

	int		indsfc[MAXTMPLT];
				/* Indices for structure array */

	nsfcmn_t	sfcdt[MAXTMPLT];
				/* Array of structures of
				   SFC attributes */

	nsfcmn_t	sfcdtSave[MAXTMPLT];
				/* Saved array of structure  */

#else

	extern int		indsfc[MAXTMPLT];
	extern nsfcmn_t		sfcdt[MAXTMPLT];
	extern nsfcmn_t		sfcdtSave[MAXTMPLT];

#endif
