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
 * DCTAMA                                                               *
 *									*
 * This program decodes bulletins containing TAMDAR BUFR messages       *
 * received directly from AirDat into GEMPAK sounding format.           *
 *                                                                      *
 *      Command line:                                                   *
 *          dctama [options] gemfil                                  	*
 *									*
 *	    gemfil = Output GEMPAK Soundings file name/template		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Caruso Magee/NCEP 08/06                                           *
 * m.gamazaychikov/SAIC 03/08   Adapted for SIB/GEMPAK use              *
 * m.gamazaychikov/SAIC 07/08   Changed the command line parameters     *
 * L. Hinson/AWC        06/08   Added circflg parameter to dc_gopt      *
 * T. Piper/SAIC	08/08	Changed NUMEXP to 1, comment updates	*
 * T. Piper/SAIC	12/08	Added capability to process new format	*
 ***********************************************************************/
{

#define NUMEXP	1

	int	nexp	= NUMEXP;
	char	*prgnam = "DCTAMA";

/*
**	Define the standard decoder parameters.
*/
	char	stntbl[DCMXLN], stntb2[DCMXLN];
	char	prmfil[DCMXLN];
	int	iadstn;
	int	maxtim;
	int	nhours;
	int	txtflg;
        int     circflg;
	int	iwndht;

/*
**	Define default values for the standard decoder parameters.
*/
	char	*defprm	= " ";
	char	*defstn	= " ";
	char	*dfstn2	= " ";
	int	idfadd	= 0;
	int	idfmax	= 0;
	int	ndfhr1	= 240;
	int	ndfhr2	= 240;
	int	idfwdh	= 0;

/*
**	Define the additional decoder parameters.
*/
	char	bufrta_new[DCMXLN], bufrta_old[DCMXLN], gemfil[DCMXLN];

/*
**	Do not change these variables.  These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN];
	char	curtim[DCMXLN];
	int	num;
	int	iret;

/*---------------------------------------------------------------------*/

/*
**	Initialize the output logs, set the time out, and 
**	parse the command line parameters.
*/
	dc_init ( prgnam, argc, argv, nexp, parms, &num, &iret );

/*
**	Check for an initialization error.
*/
	if  ( iret < 0 )  {
	    dc_exit ( &iret );
	}

/*
**	Set each standard decoder parameter to the value from the
**	command line options (if defined) or else to the default
**	value.
*/
	dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, 
		  ndfhr2, idfwdh, prmfil, stntbl, stntb2, &iadstn, 
		  &maxtim, curtim, &nhours, &txtflg, &circflg, 
		  &iwndht, &iret );

/*
**	Set default values for the additional decoder parameters.
*/
	strcpy ( bufrta_new, "bufrtab.AIRDAT_TAMDAR" );
	strcpy ( bufrta_old, "bufrtab.AIRDAT_TAMDAR.pre-Oct08" );

/*
**      The output file name must be present.
*/
	strcpy ( gemfil, parms[0] );

/*
** 	Call the decoding routine.
*/
	ta_dcod ( curtim, bufrta_old, bufrta_new, gemfil, &nhours, &iret, 
		  strlen(curtim), strlen(bufrta_old), strlen(bufrta_new), 
		  strlen(gemfil)+1 );

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );
}
