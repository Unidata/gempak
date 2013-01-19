#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

#define	EXACT	1
#define	FIRST	2
#define	INDEX	3

extern	CLO_t	clo;

void clo_findmatch ( char *locnam, char *name, char *state, int itype, 
		int srchtyp, int maxlen, int *nret, char *info, int *iret )
/************************************************************************
 * clo_findmatch							*
 *									*
 * This function finds station information based on the type and name.	*
 * A state PO abbreviation may also be given to	to narrow the choices.	*
 *									*
 * clo_findmatch  ( locnam, name, state, itype, srchtyp, maxlen, 	*
 * 			nret, info, iret )				*
 *									*
 * Input parameters:							*
 *      *locnam         char       	Data location name		*
 *	*name		char		Full name or substring to match	*
 *	*state		char		State name (optional)		*
 *	itype		int		Type of data to match		*
 *                                      = 0 - column1			*
 *                                      = 1 - column3			*
 *	srchtyp		int		Search type			*
 *					= 1 - EXACT			*
 *					= 2 - FIRST			*
 *					= 3 - INDEX			*
 *	maxlen		int		Max length of info string	*
 *									*
 * Output parameters:							*
 *	*nret		int		Number of matches returned	*
 *	*info		char		String w/ match information	*
 *	*iret		int		Return code			*
 *					=  0: normal			*
 *					= >0: > maxret available	*
 *					= -2: unable to find match	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/99	initial coding				*
 * T. Piper/GSC	 	 3/99	Corrected prolog			*
 * D.W.Plummer/NCEP	 4/99	added MARINE and COASTAL types		*
 * D.W.Plummer/NCEP	 8/00	changes for modified CLO library	*
 * D.W.Plummer/NCEP	 8/00	calling seq change - added itype	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch ( itype )  {
      case 0:
	/*
	 *  Search based on station ID
	 */
	clo_findstn ( locnam, name, state, srchtyp, maxlen, 
			nret, info, iret );
	break;
      case 1:
	/*
	 *  Search based on station description (name)
	 */
	clo_finddesc ( locnam, name, state, srchtyp, maxlen, 
			nret, info, iret );
	break;
    }
}
