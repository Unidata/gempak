/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/
#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"
#include "dccmn.h"

#define _FGRID_
#include "dcgrib.h"

int MAXGOPN=MMFILE; /* MMFILE defined in gemprm.h should be same as MXFILE
                       defined in dccmn.cmn */

int ensext=-1;
int pkmeth=MDGRB2; /* Default GRIB2 packing method */

int main ( int argc, char *argv[] )
/************************************************************************
 * DCGRIB2								*
 *									*
 * This program decodes GRIB reports and stores them in GEMPAK grid	*
 * files.								*
 *									*
 *  Command line:							*
 *  dcgrib [options] filename						*
 *      filename        output file name/template                       *
 **									*
 * Log:									*
 * Chiz/Unidata		11/99	Created from dcgrib/gribtonc 		*
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
	char	*prgnam = "DCGRIB2";

	char	*defprm = " ";
	char	*defstn = " ";
	char    *dfstn2 = " ";
	int	idfadd  = 0;
	int	idfmax  = 5000;
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
		prmfil[DCMXLN];
	int	iadstn, maxtim, nhours, txtflg, crcflg, iwndht;

	char	errstr[DCMXLN], *extptr, *mdmeth;

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
**	Initialize the structure for number of open grid files
*/
	dcgfint(&MAXGOPN);

	gd_init ( &iret );

/*
**	Set the decoder parameters to the command line entries or
**	default values.
*/
	dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,
		  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim, &nhours,
		  &txtflg, &crcflg, &iwndht, &iret );

/*
**	The if the output file name is not present, use gribkey table
**
**	Change this section for the specific decoder.
*/
        if(num < 1)
            {
	    tbglist();
            gemfil[0] = '\0';
            }
        else
	    strcpy ( gemfil, parms[0] );

/*
**      Check environmental variable for adding ensemble extensions to parameter names
**	If ENSEXT is not set, ensext=-1  defaults to yes for grib1 and no for grib2.
**	If ENSEXT is set, and equal to 1, then the extension will be added.
**
*/
        if ( ( extptr = getenv ("ENSEXT")) != NULL )
	   {
	   ensext = atoi ( extptr );
	   if ( ensext != 1 ) ensext = 0;
	   }

/*
**	Default storage method is MDGRB2 for grib2 data, which is not backward compatible
**	with older GEMPAK distributions. If environmental variable MDMETH=MDGDEC,
**	use backward compatibility - which will be much slower for GRIB2 decoding.
**
*/
	if ( ( mdmeth = getenv ("MDMETH")) != NULL )
	   {
	   if ( strcmp(mdmeth, "MDGDEC") == 0 )
		pkmeth = MDGDEC;
	   }

/*
**	Call the decoding routine.
**
**	Change this function call, and the define command,
**	for the specific decoder.
*/
	decode_grib (gemfil, maxtim, &iret);

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );

}
