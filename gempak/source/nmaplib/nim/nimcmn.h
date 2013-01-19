/************************************************************************
 * nimcmn								*
 *									*
 * This header file contains the global varibles and structures for	*
 * the NIM library.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Jacobs/NCEP	10/99	Added nim_gfln_ to UNDERSCORE		*
 * S. Law/GSC		06/00	MAXIMG->MAXTMPLT and MXTIME to gemprm.h	*
 * M. Li/GSC		07/00	Added imgSave				*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * T. Piper/SAIC	06/03	Removed nimg				*
 * E. Safford/SAIC	01/08	include proto_xw.h and proto_nmaplib.h	*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "proto_xw.h"
#include "proto_nmaplib.h"


typedef struct {
	char	type[4];
	char	info[81];
	char	lutf[81];
} nimcmn_t;

#ifdef NIM_GLOBAL

	int		indimg[MAXTMPLT];
				/* Indices for structure array */

	nimcmn_t	image[MAXTMPLT];
				/* Array of structures of
				   image attributes */

	nimcmn_t	imgSave[MAXTMPLT];
				/* Save array of structure */

#else

	extern int		indimg[MAXTMPLT];
	extern nimcmn_t		image[MAXTMPLT];
	extern nimcmn_t        	imgSave[MAXTMPLT];

#endif
