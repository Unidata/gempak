/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"
#include "dcnldn.h"

int main ( int argc, char *argv[] )
/************************************************************************
 * DCNLDN								*
 *									*
 * This program decodes NLDN lightning reports and stores them in a	*
 *   GEMPAK ship file. 			                                *
 *									*
 *  Command line:							*
 *  dcnldn [options] filename						*
 *      filename        output file name/template                       *
 **									*
 * Log:									*
 * Chiz/Unidata		8/96	Created from nldntogem 			*
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
	char	*prgnam = "DCNLDN";

	char	*defprm = "nldn.pack";
	char	*defstn = "hour";
	char	*dfstn2 = " ";
	int	idfadd  = 0;
	int	idfmax  = 25000;
	int	ndfhr1  = 60; /* default -b file NN bins */
	int	ndfhr2  = 24;
	int	idfwdh  = 0;

/*
**	Do not change these variables. These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN], curtim[DCMXLN];
	int	num, iret;

	char	gemfil[DCMXLN], stntbl[DCMXLN], stntb2[DCMXLN],
		prmfil[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, crcflg, iwndht;

/*---------------------------------------------------------------------*/

/*
**	Initialize the output logs, set the time out and 
**	parse the command line parameters. dc_init calls in_bdta in 5.4.3+. 
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
		  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim, &nhours,
		  &txtflg, &crcflg, &iwndht, &iret );

/*
**	The output file name must be present.
**
**	Change this section for the specific decoder.
*/
	strcpy ( gemfil, parms[0] );
        /* add %% template for file sequence if not @@ */
        if(strstr(gemfil,"@@") == NULL)
           strncat(gemfil,"_%%",3);

/*
**	Call the decoding routine.
**
**	Change this function call, and the define command,
**	for the specific decoder.
*/
	decode_nldn (curtim, gemfil, prmfil, stntbl, maxtim,
		nhours, &iret);

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );

}
