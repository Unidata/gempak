#include "geminc.h"
#include "gemprm.h"

void cfl_iret ( int ierrno, int *iflerr, int *iret )
/************************************************************************
 * cfl_iret								*
 *									*
 * This function translates a non-zero error value from a C I/O call	*
 * into a GEMPAK "CFL" error code.					*
 *									*
 * cfl_iret ( ierrno, iflerr, iret )					*
 *									*
 * Input parameters:							*
 *	 ierrno		int		Status from I/O operation	*
 *									*
 * Output parameters:							*
 *	*iflerr		int		GEMPAK "CFL" error number	*
 *					 -1 = File does not exist	*
 *					 -2 = Cannot open file		*
 *					 -3 = Cannot read file		*
 *					 -4 = Cannot write file		*
 *					 -5 = File already exists	*
 *					 -6 = No file has been opened	*
 *					 -7 = Cannot write / read	*
 *					 -8 = Permission denied		*
 *					 -9 = Invalid type of I/O	*
 *					-10 = Is a directory		*
 *					-11 = Is not a directory	*
 *	*iret		int		Return code			*
 *					   0 = Normal			*
 **									*
 * G. Krueger/EAI	3/96						*
 * G. Krueger/EAI	8/96	Removed error check; Added not a dir.	*
 * T. Piper/GSC	       10/98	Corrected prolog for ierrno             *
 ***********************************************************************/
{
	char	*osname;
/*---------------------------------------------------------------------*/
	*iret = 0;
	*iflerr = ierrno;

	if ( ierrno == 0 ) return;
/*
 *	Determine operating system from environment variable.
 *	Translate ierrno into CFL-error.
 */
	if      (  ierrno ==  2 )			*iflerr =  -1;
	else if ( (ierrno == 24) || (ierrno == 63) )	*iflerr =  -2;
	else if ( (ierrno == 26) || (ierrno == 28) )	*iflerr =  -4;
	else if (  ierrno == 17 )			*iflerr =  -5;
	else if (  ierrno ==  9 )			*iflerr =  -6;
	else if (  ierrno ==  5 )			*iflerr =  -7;
	else if ( (ierrno ==  1) || (ierrno == 13) ||
		  (ierrno == 30) )			*iflerr =  -8;
	else if (  ierrno == 60 )			*iflerr =  -9;
	else if (  ierrno == 21 )			*iflerr = -10;
	else if (  ierrno == 20 )			*iflerr = -11;
	else {
	    osname = getenv ( "OS" );
	    if ( memcmp (osname, "HPUX", 4) == 0 ) {
		if      ( ierrno == 53 ) *iflerr = -2;
		else if ( ierrno == 51 ) *iflerr = -3;
		else if ( ierrno == 50 ) *iflerr = -9;
	    } else if ( memcmp (osname, "IRIX", 4) == 0 ) {
		if ( ierrno == 61 ) *iflerr = -3;
	    } else if ( memcmp (osname, "SunOS", 5) == 0 ) {
		if ( ierrno == 61 ) *iflerr =  -3;
	    }
	}
}
