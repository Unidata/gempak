/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"

/*
 * Prototype for Fortran decoder driver.
 */

void nc_dcod ( char *curtim, char *gemfil, char *stntbl, char *prmfil, 
		  int *iadstn, int *maxtim, int *nhours, int *iret, 
                  size_t, size_t, size_t, size_t ); 

int main ( int argc, char *argv[] )
/************************************************************************
 * DCNCON								*
 *									*
 * This program decodes non-convective sigmet reports and writes the    *
 * output to an ASCII file.                                             *
 *									*
 *  Command line:							*
 *  dcncon [options] filename						*
 *	filename	output file name/template			*
 **									*
 * Log:									*
 * D. Kidwell/NCEP	 8/00                                           *
 * D. Kidwell/NCEP	 9/00 	Added call to clo_init                  *
 * A. Hardy/GSC		 1/01   Added full prototype			*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 * m.gamazaychikov/SAIC 07/05   Added parameters to CS of dc_gopt       *
 * H. Zeng/SAIC		08/05	Added parameters to CS of dc_gopt	*
 * L. Hinson/AWC        06/08   Added circflg parameter to dc_gopt      *
 * SGuan-BHebbard/NCEP  06/22   Changed strlen in protos int->size_t    *
 ***********************************************************************/
{

/*
**	Change the values of these default variables for the
**	specific decoder.
**
**	These variables are the number of expected command line
**	parameters; the program name; the packing and station tables;
**	values for the the number of additional stations and the
**	number of times; and the number of hours, prior to the
**	"current" time, to decode.
*/
#define NUMEXP 1
	int	nexp    = NUMEXP;
	char	*prgnam = "DCNCON";

  	char	*defprm = " ";
	char	*defstn = " ";
	char	*dfstn2 = " ";
	int	idfadd  = 25;
	int	idfmax  = 24;
	int	ndfhr1  =  5;
	int	ndfhr2  = 24;
        int     idfwdh  =  0;

/*
**	Do not change these variables. These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN], curtim[DCMXLN];
	int	num, iret;

	char	gemfil[DCMXLN], stntbl[DCMXLN], stntb2[DCMXLN], 
		prmfil[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, circflg, iwndht;

/*---------------------------------------------------------------------*/

/*
**	Initialize the output logs, set the time out and 
**	parse the command line parameters.
*/
	dc_init ( prgnam, argc, argv, nexp, parms, &num, &iret );

/*
**	Check for an initialization error.
**	On an error, exit gracefully.
*/
	if  ( iret < 0 )  {
	    dc_exit ( &iret );
	}

/*
**	Set the decoder parameters to the command line entries or
**	default values.
*/
	dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,
                  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim,
                  &nhours, &txtflg, &circflg, &iwndht, &iret );

/*
**	The output file name must be present.
**
**	Change this section for the specific decoder.
*/
	strcpy ( gemfil, parms[0] );

/*
**	Initialize the clo library values.  This call is required for
**	this decoder because it uses the vor station table and
**	associated clo library routines.
*/
	clo_init ( &iret );

/*
**	Call the decoding routine.
**
**	Change this function call, and the define command,
**	for the specific decoder.
*/

	nc_dcod ( curtim, gemfil, stntbl, prmfil, 
		  &iadstn, &maxtim, &nhours, &iret, strlen(curtim), 
		  strlen(gemfil), strlen(stntbl), strlen(prmfil) );
/*
**	Send a shut down message to the logs and close the log files.
*/
	dc_exit ( &iret );
	return 0;
}
