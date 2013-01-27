/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"

int main ( int argc, char *argv[] )

/************************************************************************
 * DCSTORM								*
 *									*
 * This program decodes SPC WWUS60 storm reports			*
 *									*
 *  Command line:							*
 *  dcsels [options] filename						*
 *      filename        output file name/template                       *
 **									*
 * Log:									*
 * Chiz/Unidata		8/96	Created from selstogem			*
 ***********************************************************************/

{

/*
**      Change the values of these default variables for the
**      specific decoder.
**
**      These variables are the number of expected command line
**      parameters; the program name; the packing and station tables;
**      values for the the number of additional stations and the
**      number of times; and the number of hours, prior to the
**      "current" time, to decode.
*/
#define NUMEXP	1
	int	nexp    = NUMEXP;
	char	*prgnam = "DCSTORM";

	char	*defprm = "sels.pack";
	char	*defstn = " ";
	char	*dfstn2 = " ";
	int	idfadd  = 0;
	int	idfmax  = 9999;
	int	ndfhr1  = 0;
	int	ndfhr2  = 0;
	int     idfwdh  = 0;

/*
**	Do not change these variables. These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN], curtim[DCMXLN];
	int	num, iret, ier;

	char	gemfil[DCMXLN], stntbl[DCMXLN], stntb2[DCMXLN],
		prmfil[DCMXLN], errstr[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, crcflg, iwndht;

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
	    sprintf ( errstr, "Error initializing\0" );
	    dc_wclg ( 0, "DC", iret, errstr, &ier );
	    dc_exit ( &iret );
	}

/*
**	Set the decoder parameters to the command line entries or
**	default values.
*/
	dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,
		  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim, &nhours,
		  &txtflg, &crcflg, &iwndht, &iret );

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
#ifdef UNDERSCORE
#define dcsels	dcsels_
#endif
        dcsels(gemfil, prmfil, &idfmax, curtim, stntbl, &iret, 
                  strlen(gemfil), strlen(prmfil),strlen(curtim),strlen(stntbl));

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );

}
