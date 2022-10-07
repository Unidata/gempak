/*
** Do not change these include commands.
** All necessary header files are included in "geminc.h".
** All macros and constants are in "gemprm.h" and "bridge.h".
*/

#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"

void shn_dcod ( char *curtim, char *shefpm, char *sheftb, char *bufrtb, char *gemfil,  
                char *prmfil, char *pestr, int *npe, int *iadstn, int *maxtim,  
                int *nhours, int *iflag, int *iret,
		size_t, size_t, size_t, size_t, size_t, size_t, size_t );

void    dc_wclg  (  int     loglev,
		    char    *errgrp,
		    int     numerr,
		    char    *errstr,
		    int     *iret );


int main ( int argc, char *argv[] )

/************************************************************************
 * DCSHEF								*
 *									*
 * This program decodes SHEF bulletins into GEMPAK or BUFR format with	*
 * or without an ASCI file containing the user defined OHSHEF PECODES   *
 *									*
 *	Command line: (5 arguments)					*
 *	    dcshef [options] [shefpm] [sheftb] [bufrtb]	[filename]      *
 *                   [iflag]	< input_file                            *
 *									*
 *	    shefpm	= SHEFPARM parameter file used in the libOHSHEF *
 *	    sheftb	= SHEF station table (shef_COOPx.tbl) x=1,2,3,4	*
 *	    bufrtb	= BUFR tables file (bufrtab.xxx) xxx=000,001,255*
 *          filename    = template for GEMPAK output file names         *
 *          iflag       = FLAG to run either GEMPAK or BUFR output 2or1 *
 *	    pecodes	= string of concatenated two-character PE codes	*
 *			  for which to additionally create ASCII text	*
 *			  output, and where a "." as the second		*
 *			  character of a PE code matches any single	*
 *			  character. This is not a default and if     	*
 *                        required, provide it after the [iflag]        *
 *									*
 **									*
 * Log:									*
 * J. Ator/NCEP		04/05						*
 * C. Caruso Magee/NCEP 02/06   Added new args to calling sequence of   *
 *                              dc_gopt                                 *
 * J. Ator/NCEP		10/06	Added pecodes argument			*
 * V.Krishna Kumar/NCEP 12/06   Modified for NAWIPS to create a dual    *
 *                              output (GEMPAK or BUFR) decoder with or *
 *                              an ASCI output containing the user      *
 *                              defined OHSHEF library PECODES          *
 * L. Hinson/AWC        06/08   Added circflg parameter to dc_gopt      *
 * S. Jacobs/NCEP	 4/11	Fixed to remove compiler warning/errors	*
 * SGuan-BHebbard/NCEP  06/22   Changed strlen in protos int->size_t    *
 ***********************************************************************/

{

#define NUMEXP	5

	int	nexp	= NUMEXP;
	char	*prgnam = "DCSHEF";

/*
**	Define the standard decoder parameters.
*/
	char	stntbl[DCMXLN], stntb2[DCMXLN];
	char	prmfil[DCMXLN], gemfil[DCMXLN];
	int	iadstn;
	int	maxtim;
	int	nhours;
	int	txtflg;
        int     circflg;
	int	iwndht;
        int     iflag;

/*
**	Define default values for the standard decoder parameters.
*/
	char	*defprm	= "shef.pack";
	char	*defstn	= " ";
	char	*dfstn2	= " ";
	int	idfadd	= 0;
	int	idfmax	= 24;
	int	ndfhr1	= 5;
	int	ndfhr2	= 24;
	int	idfwdh	= 0;

/*
**	Define the additional decoder parameters.
*/
	char	shefpm[DCMXLN];
	char	sheftb[DCMXLN];
	char	bufrtb[DCMXLN];

	char	pestr[DCMXLN];
	int	lpestr;
	int	npe;
	char	*pemsg = "Length of PE codes string not divisible by 2";

/*
**	Do not change these variables.  These variables are used by all
**	decoders for getting the command line parameters.
*/
	char	parms[NUMEXP][DCMXLN];
	char	curtim[DCMXLN];
	int	num;
	int	iret;
	int	ier;


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
**	Set default values for the additional decoder parameters.
**      Default value for iflag is  2 to generate a GEMPAK output
**      file. iflag = 1 will generate a BUFR output after setting
**      the proper export variables. See the help provided in
**      $GEMPAK/help/hlp/dcshef.hlp for further details.
*/
	strcpy ( shefpm, "shef.prm" );
	strcpy ( sheftb, "shef_COOP.tbl" );
	strcpy ( bufrtb, "bufrtab.000" );
        iflag = 2 ;

/*
**	Now, for each additional decoder parameter, if a value was
**	defined on the command line, then use that value instead of
**	the default value.
*/
	if  ( num > 0 )  {
	    strcpy ( shefpm, parms[0] );
	}

	if  ( num > 1 )  {
	    strcpy ( sheftb, parms[1] );
	}

	if  ( num > 2 )  {
	    strcpy ( bufrtb, parms[2] );
	}

        if  ( num > 3 )  {
            strcpy ( gemfil, parms[3] );
        }

        if  ( num > 4 )  {
            iflag = atoi(parms[4] );
        }


/*
**	Check whether any pecodes were defined on the command line.
*/
	if  ( num > 5 )  {
	    strcpy( pestr, argv[argc-1] );
	    lpestr = strlen( pestr );
	    if ( G_NINT( (float) fmod( (double) lpestr, (double) 2 ) ) != 0 ) {
		dc_wclg( 2, "DC", 2, pemsg, &ier);
		dc_exit( &iret );
	    }
	}
	else {
	    pestr[0] = '\0';
	    lpestr = 0;
	}
	npe = lpestr / 2;

/*
** Call the decoding routine.
**
** Change this function call and define command for the
** specific decoder.
*/

	shn_dcod ( curtim, shefpm, sheftb, bufrtb, gemfil,  
                  prmfil, pestr, &npe, &iadstn, &maxtim,  
                  &nhours, &iflag, &iret,
		  strlen(curtim), strlen(shefpm), strlen(sheftb),
		  strlen(bufrtb), strlen(gemfil), strlen(prmfil), 
                  lpestr );

/*
**	Send shut down message and close the log files.
*/
	dc_exit ( &iret );

}
