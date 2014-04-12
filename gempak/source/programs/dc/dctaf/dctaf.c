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

void tf_dcod ( char *curtim, char *gemfil, char *stntbl, char *stntb2,
	       char *prmfil, int *iadstn, int *maxtim, int *nhours,
	       int *circflg, int *iret, 
                  int, int, int, int, int );

int main ( int argc, char *argv[] )
/************************************************************************
 * DCTAF 								*
 *									*
 * This program decodes TAF reports and writes the output to a GEMPAK	*
 * surface forecast file.						*
 *									*
 *  Command line:							*
 *  dctaf [options] filename						*
 *	filename	output file name/template			*
 **									*
 * Log:									*
 * D. Kidwell/NCEP 	 9/02	Based on DCMETR                         *
 * A. Hardy/NCEP        12/02   Changed stn tbl sfstns.tbl->tafstn.tbl  *
 * m.gamazaychikov/SAIC 07/05   Added parameters to CS of dc_gopt       *
 * H. Zeng/SAIC		08/05	Added parameters to CS of dc_gopt	*
 * L. Lin/NCEP 		04/08	Modified fcst times from 24 to 30	*
 * L. Hinson/AWC        06/08   Added circflg to dc_gopt and call to    *
 *                              tf_dcod                                 *
 * S. Jacobs/NCEP	 3/14	Added black list station table		*
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
	char	*prgnam = "DCTAF";

	char	*defprm = "taf.pack";
	char	*defstn = "tafstn.tbl";
	char	*dfstn2 = "tafblacklist.tbl";
	int	idfadd  = 25;
/*      idfmax was set to 24 - default number of forecast time
**      now it has been changed to 30
*/
	int	idfmax  = 30;
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
**	Call the decoding routine.
**
**	Change this function call, and the define command,
**	for the specific decoder.
*/

	tf_dcod ( curtim, gemfil, stntbl, stntb2, prmfil, 
		  &iadstn, &maxtim, &nhours, &circflg, &iret, 
                  strlen(curtim), strlen(gemfil), strlen(stntbl), 
                  strlen(stntb2), strlen(prmfil) );
/*
**	Send a shut down message to the logs and close the log files.
*/
	dc_exit ( &iret );
}
