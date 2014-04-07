/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"
#include "dccmn.h"

void	decode_redbook(char *device, char *rawfile, int *iret);

int main ( int argc, char *argv[] )
/************************************************************************
 * DCREDBOOK								*
 *									*
 * This program decodes Redbook graphics files and outputs them as 	*
 * standard graphics format files from GEMPAK device drivers.		*
 *									*
 *  Command line:							*
 *  dcredbook [options] device						*
 *      device		output file device                       	*
 **									*
 * Log:									*
 * Chiz/Unidata		 8/00						*
 * M. James/Unidata	04/14	Updated					*
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
	char	*prgnam = "DCREDBOOK";

	char	*defprm = " ";
	char	*defstn = " ";
	char	*dfstn2 = " ";
	int	idfadd  = 0;
	int	idfmax  = 1;
	int	ndfhr1  = 0;
	int	ndfhr2  = 0;
	int     idfwdh  = 0;

/*
**	Do not change these variables. These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN], curtim[DCMXLN];
	int	num, iret, ier;

	char	gemdev[DCMXLN], stntbl[DCMXLN], stntb2[DCMXLN],
		prmfil[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, crcflg, iwndht;

	char    errstr[DCMXLN];

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
	if  (( iret < 0 )&&(iret != -11))  {
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
**	Change this section for the specific decoder.
*/
	strcpy ( gemdev, parms[0] );

/*
**	Call the decoding routine.
**
**	Change this function call, and the define command,
**	for the specific decoder.
*/
	decode_redbook (gemdev, stntbl, &iret);

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );

}
