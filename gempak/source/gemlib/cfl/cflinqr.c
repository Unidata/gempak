#include "geminc.h"
#include "gemprm.h"

void cfl_inqr ( const char *filnam, const char *defdir, long *flen,
						char *newfil, int *iret )
/************************************************************************
 * cfl_inqr								*
 *									*
 * This function determines whether a file exists and the file size.	*
 * If there is a leading ^, the file name is case sensitive. Otherwise	*
 * if the file is not found, the file name is changed to all lower case *
 * and test again.							*
 *									*
 * The file is located by searching in the following order (environment	*
 * variables may be used as part of the paths):				*
 *									*
 *	1. filnam (as given)						*
 *	2. defdir/filnam						*
 *	3. filenam (lower case)						*
 *	4. defdir/filnam (lower case)					*
 *									*
 * cfl_inqr ( filnam, defdir, flen, newfil, iret )			*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *	*defdir		char		Default directory		*
 *									*
 * Output parameters:							*
 *	*flen		long		File size			*
 *	*newfil		char		Expanded file name		*
 *	*iret		int		Return code			*
 *					  0 = Normal, file exists	*
 *					 -1 = File does not exist	*
 **									*
 * G. Krueger/EAI	 3/96						*
 * G. Krueger/EAI        8/96	Match with FL library			*
 * T. Lee/SAIC		12/02	Initialize flen				*
 * B. Yin/SAIC		 4/07	Check ^ and try the lower case		*
 ***********************************************************************/
{
	int		ier, ier1;
	char		t_defdir[LLPATH], t_filnam[FILE_FULLSZ];
	struct stat	stbuf;
	Boolean		caseFlag;
/*---------------------------------------------------------------------*/
/*
 *  Set the case flag.
 */
    if ( filnam[0] == '^' ) {
	caseFlag = False;
	css_envr ( &filnam[1], t_filnam, &ier );
    }
    else {
	caseFlag = True;	
	css_envr ( filnam, t_filnam, &ier );
    }
    strcpy(newfil, t_filnam);
/*
 *  Check the file status.
 */
    if ( (ier1 = stat( newfil, &stbuf) ) != 0 ) {
	if ( defdir != NULL ) {
/*
 *  Try the DEFDIR directory.
 */
	    css_envr ( defdir, t_defdir, &ier );
	    sprintf(newfil, "%s/%s", t_defdir, t_filnam);
	    ier1 = stat ( newfil, &stbuf );
	}
    }

/*
 * If the file is not found and the case flag is true,
 * change the file name to lower case and try.
 */
    if ( ier1 != 0 && caseFlag ) {
	cst_uclc( t_filnam, newfil, &ier );
/*
 *  Check the file status as all lower case characters.
 */
	if ( (ier1 = stat ( newfil, &stbuf )) != 0 ) {
	    if ( defdir != NULL ) {
/*
 *  Try the DEFDIR directory with all lower case characters.
 */
		css_envr ( defdir, t_defdir, &ier );
		sprintf(newfil, "%s/%s", t_defdir, t_filnam);
		ier1 = stat ( newfil, &stbuf );
	    }
	}
    }

/*
 *  Return outputs based upon status.
 */
    if ( ier1 == 0 ) {
	*flen = (long)stbuf.st_size;
	*iret = G_NORMAL;
    } 
    else {
	*flen = 0;
	if ( defdir == NULL ) 
	    strcpy(newfil, t_filnam);
	else
	    sprintf(newfil, "%s/%s", t_defdir, t_filnam);
	cfl_iret ( errno, iret, &ier );
    }
}
