/************************************************************************
 * nmscmn								*
 *									*
 * This header file contains the global varibles and structures for	*
 * the NMS library.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Jacobs/NCEP	11/99	Increased MAXPLT from 10 to 20		*
 * S. Jacobs/NCEP	11/99	Changed arrays of info to structures	*
 * S. Jacobs/NCEP	 1/00	Added numtyp and numflg to structure	*
 * S. Law/GSC		06/00	MAXMISC->MAXTMPLT and MXTIME to gemprm.h*
 * M. Li/GSC		07/00	Added mscdtSave				*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * T. Piper/SAIC	12/02	Increased filnam from 81 to FILE_FULLSZ	*
 * T. Piper/SAIC        06/03   Removed nmsc                            *
 * S. Gilbert/NCEP	05/06	Increased MAXPLT from 20 to 25		*
 * E. Safford/SAIC	01/08	include proto_nmaplib.h	and proto_xw.h	*
 * L. Hinson/AWC         5/12   Increased MAXPLT to 35                  *
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "nmsdef.h"
#include "proto_nmaplib.h"
#include "proto_xw.h"


/* Maximum number of plotting attributes (colors, label flags, etc.) */
#define MAXPLT	35

typedef struct {
	char	    alias[81];
	char	    filnam[FILE_FULLSZ];
	int	    numtyp;
	NMS_types   msctyp[MAXPLT];
	int	    numflg;
	NMS_flags   mscflg[MAXPLT];
	int	    isbcat;
} nmscmn_t;

#ifdef NMS_GLOBAL

	int		indmsc[MAXTMPLT];
				/* Indices for structure array */

	nmscmn_t	mscdt[MAXTMPLT];
				/* Array of structures of
				   MISC attributes */

	nmscmn_t	mscdtSave[MAXTMPLT];
				/* Save array of structure */

#else

	extern int		indmsc[MAXTMPLT];
	extern nmscmn_t		mscdt[MAXTMPLT];
	extern nmscmn_t        	mscdtSave[MAXTMPLT];

#endif
