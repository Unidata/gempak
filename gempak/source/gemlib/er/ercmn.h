/************************************************************************
 * ercmn.h								*
 *									*
 * This file contains global variables for use in the error message	*
 * routines.								*
 *									*
 **									*
 * Log:									*
 * K. Tyle/GSC		12/96						*
 * K. Tyle/GSC		 1/97	Change nstr to nermsg; MXNSTR to MXERST	*
 * C. Lin/EAI		 1/97	Change msgstr to errmsg			*
 * T. Piper/GSC		01/01	Removed geminc.h & gemprm.h		*
 * J. Wu/SAIC		01/04	move MXERST to gemprm.h			*
 * T. Piper/SAIC	10/06	Increased errmsg to 513			*
 ***********************************************************************/

/* Global variables */

#ifdef ERCMN_GLOBAL

	char		errmsg[MXERST][513];
				/* Error message string array */

	int		nermsg;
				/* Number of error messages */
#else

	extern char	errmsg[MXERST][513];

	extern int	nermsg;

#endif
