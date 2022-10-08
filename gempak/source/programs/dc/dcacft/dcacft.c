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

void af_dcod ( char *curtim, char *gemfil, char *pnvtbl, char *awptbl, 
                  char *prmfil, int *iadstn, int *maxtim, int *nhours, 
                  int *iret, size_t, size_t, size_t, size_t, size_t );

int main ( int argc, char *argv[] )
/************************************************************************
 * DCACFT								*
 *									*
 * This program decodes aircraft bulletins into GEMPAK format.		*
 *									*
 *	Command line:							*
 *	    dcacft [options] filename [pnvtbl] [awptbl]			*
 *									*
 *	    filename	  template for GEMPAK output file names 	*
 *	    pnvtbl	  PIREP navaids table 				*
 *	    awptbl	  AIREP waypoints table 			*
 *									*
 **									*
 * Log:									*
 * J. Ator/NP12		08/97						*
 * I. Durham/GSC	05/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC         01/01   Added full prototype 			*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 * D. Kidwell/NCEP	 1/03	Increased idfmax to MMHDRS - 1          *
 * m.gamazaychikov/SAIC	07/05	Added parameters to CS of dc_gopt	*
 * H. Zeng/SAIC		07/05	Added parameters to CS of dc_gopt	*
 * L. Hinson/AWC        06/08   Added circflg parameter to dc_gopt      *
 * SGuan-BHebbard/NCEP  06/22   Changed strlen in protos int->size_t    *	
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

#define NUMEXP	3

	int	nexp	= NUMEXP;
	char	*prgnam = "DCACFT";

	char	*defprm	= "acft.pack";
	char	*defstn	= " ";
	char	*dfstn2	= " ";
	int	idfadd	= 25;
	int	idfmax	= MMHDRS - 1;
	int	ndfhr1	= 5;
	int	ndfhr2	= 24;
        int     idfwdh  =  0;

/*
**	Define the standard decoder parameters.
*/
	char	gemfil[DCMXLN], stntbl[DCMXLN], stntb2[DCMXLN], prmfil[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, circflg, iwndht;

/*
**	Define the additional decoder parameters.
*/
	char	pnvtbl[DCMXLN], awptbl[DCMXLN];

/*
**	Do not change these variables.  These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN], curtim[DCMXLN];
	int	num, iret;


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
	dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,
		  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim,
		  &nhours, &txtflg, &circflg, &iwndht, &iret );

/*
**	The template for GEMPAK output file names must be present
**	on the command line.
*/
	strcpy ( gemfil, parms[0] );

/*
**	Set default values for the additional decoder parameters.
*/
	strcpy ( pnvtbl, "pirep_navaids.tbl" );
	strcpy ( awptbl, "airep_waypnts.tbl" );

/*
**	Now, for each additional decoder parameter, if a value was
**	defined on the command line, then use that value instead of
**	the default value.
*/
	if  ( num > 1 )  {
	    strcpy ( pnvtbl, parms[1] );
	}

	if  ( num > 2 )  {
	    strcpy ( awptbl, parms[2] );
	}

/*
** Call the decoding routine.
**
** Change this function call and define command for the
** specific decoder.
*/

	af_dcod ( curtim, gemfil, pnvtbl, awptbl, prmfil,
		  &iadstn, &maxtim, &nhours, &iret, 
		  strlen(curtim), strlen(gemfil),
		  strlen(pnvtbl), strlen(awptbl),
		  strlen(prmfil) );

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );
	return(0);
}
