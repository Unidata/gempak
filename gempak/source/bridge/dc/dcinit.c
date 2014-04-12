#define DCCMN_GLOBAL
#include "dccmn.h"

typedef struct envlist {
	char		*env;
	struct envlist	*next;
} envlist;

envlist *envhead = NULL, *envobj;

void dc_init ( char *prgnam, int argc, char **argv, int numexp, 
				char parms[][DCMXLN], int *num, int *iret )
/************************************************************************
 * dc_init								*
 *                                                                      *
 * This routine initializes the bridge and decoder parameters and	*
 * processes the command line options.					*
 *                                                                      *
 * dc_init ( prgnam, argc, argv, numexp, parms, num, iret )		*
 *                                                                      *
 * Input parameters:							*
 *	*prgnam		char		Program name			*
 *	argc		int		Number of command line args	*
 *	**argv		char		Command line arguments		*
 *	numexp		int		Number of expected parameters	*
 *                                                                      *
 * Output parameters:							*
 *	parms[][DCMXLN]	char		Parameters found on command line*
 *	*num		int		Number of parameters found	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					 -11 = no command line args	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi		 5/95						*
 * S. Jacobs/NMC	 7/95	Update and clean up			*
 * S. Jacobs/NCEP	 6/96	Updated documentation; Changed atoi to	*
 *				cst_numb; Removed the +3 return	code;	*
 *				Changed ldfd to a FILE stream - fplog	*
 * S. Jacobs/NCEP	 7/96	Reorganized the source code		*
 * K. Tyle/GSC		 7/96	NT_HELP --> IP_HELP			*
 * S. Jacobs/NCEP	 7/96	Removed log file open			*
 * K. Tyle/GSC		 1/97	Added calls to IN_BDTA and ER_STAT;	*
 *				changed numerr in startup dc_wclg call	*
 * K. Tyle/GSC		 1/97	Use iflg in call to ER_STAT		*
 * D. Kidwell/NCEP	 9/97	Added version number to help option     *
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 1/00	Added command line input of env vars	*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 * R. Tian/SAIC		 8/02	Added version to log file		*
 * m.gamazaychikov/SAIC	07/05	Added -w input parameter		*
 * H. Zeng/SAIC		08/05	Added second station table		*
 * L. Hinson/AWC        06/08   Add -r circular flag switch             *
 * S. Jacobs/NCEP	 3/12	Add $HOME to the logs directory		*
 * S. Jacobs/NCEP	12/13	Added more options for log location	*
 ***********************************************************************/
{
	int	ch, i, errflg, ier;
	int	pagflg = G_FALSE;
	int	iflg   = G_TRUE;

/*
**	These variables are used for logging errors and notices.
*/
	char	tdclog[DCMXLN];
	int	logflg, ibuf;
	char	errstr[DCMXLN];
	char	version[128];

/*
**	These variables are used by getopt. Unset the error reporting.
*/
	char		optver[20];

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Set the process ID for log messages.
*/
	ipid = (int) getpid ();

/*
**	Initialize the bulletin counter.
*/
	nbull = 0;

/*
**	Save the program name as a global variable.
*/
	strcpy ( cprgnm, prgnam );

/*
**	Set up the signal handlers.
*/
	dc_sgnl ( );
    
/*
**	Get and process the command line options.
**
**	Set the default values for parsing the command line.
*/
	strcpy ( curtim, "SYSTEM" );
	cst_uclc ( cprgnm, tdclog, &ier );
	strcat ( tdclog, ".log" );
	logflg    = G_FALSE;
	itmout    = DCDFTM;
	irltim    = G_TRUE;
	txtflg    = G_TRUE;
        circflg   = G_FALSE;
	ivrblv    = 0;
	prmfil[0] = CHNULL;
	stntbl[0] = CHNULL;
	iadstn    = IMISSD;
	maxtim    = IMISSD;
	nhours    = IMISSD;
	iwndht    = IMISSD;

/*
**	Get the valid options from the command line.
**	The valid options are:
**		-v	Set the level of verbosity for the logs
**		-c	Set the "current" time
**		-b	Number of hours to decode prior to "current" time
**		-d	Set the decoder log file name
**		-t	Set the interval for the time out
**		-p	Set the parameter packing table
**		-s	Set the station table
**		-S	Set the second station table
**		-a	Set the number of additional stations
**		-m	Set the max number of times
**		-e	Set an environment variable=value
**		-n	Set a flag to NOT save the text data
**              -r      Set a flag to force circular files
**		-w	Set the cutoff "close-to-the-surface" height
**		-h	Print the help file, then exit the program
*/
	opterr = 1;
	errflg = 0;
	while ( ( ch = getopt ( argc, argv,
				"v:c:b:d:t:p:s:S:a:m:e:w:nhr" ) ) != EOF ) {
	    switch ( ch ) {
		case 'v':
			cst_numb ( optarg, &ivrblv, &ier );
			if  ( ivrblv < 0 )  ivrblv = 0;
			break;
		case 'c':
			strcpy ( curtim, optarg );
			irltim = G_FALSE;
			break;
		case 'b':
			cst_numb ( optarg, &nhours, &ier );
			break;
		case 'd':
			strcpy ( tdclog, optarg );
			logflg = G_TRUE;
			break;
		case 't':
			cst_numb ( optarg, &itmout, &ier );
			if  ( itmout < 1 )  itmout = DCDFTM;
			break;
		case 'p':
			strcpy ( prmfil, optarg );
			break;
		case 's':
			strcpy ( stntbl, optarg );
			break;
		case 'S':
			strcpy ( stntb2, optarg );
			break;
		case 'a':
			cst_numb ( optarg, &iadstn, &ier );
			break;
		case 'm':
			cst_numb ( optarg, &maxtim, &ier );
			break;
		case 'e':
			envobj = (envlist *) malloc ( sizeof(envlist) );
			envobj->env = malloc ( strlen(optarg)+1 );
			strcpy ( envobj->env, optarg );
			envobj->env[strlen(optarg)] = '\0';
			envobj->next = envhead;
			envhead = envobj;
			break;
		case 'n':
			txtflg = G_FALSE;
			break;
		case 'w':
			cst_numb ( optarg, &iwndht, &ier );
			break;
                case 'r':
                        circflg = G_TRUE;
                        break;
		case 'h':
			ip_help ( cprgnm, &pagflg, &ier,
				  strlen(cprgnm) );
			strcpy ( cprgnm, "DECODE" );
			ip_help ( cprgnm, &pagflg, &ier,
				  strlen(cprgnm) );
			ss_vers ( optver, &ier,
				  sizeof (optver) );
			printf ( ">%s<\n", optver );
			exit ( 0 );
			break;
		case '?':
			errflg++;
			break;
	    }
	}

/*
**	Initialize GEMPAK and set error reporting parameters.
*/
	in_bdta ( &ier );
	ibuf = 1;
	er_stat ( &ivrblv, &ibuf, &iflg, &ier );

/*
**	Open the decoder log.
**
**	If the processing is in real-time add the directory to the 
**	file name.
*/
	if  ( !logflg && irltim )
	{
	    if ( tdclog[0] == '/' ) {
		strcpy ( dcdlog, tdclog );
	    }
	    else if ( getenv("GEMPAK_DECODER_LOGS") ) {
		strcpy ( dcdlog, "$GEMPAK_DECODER_LOGS/" );
		strcat ( dcdlog, tdclog );
	    }
	    else {
		strcpy ( dcdlog, "$HOME/" );
		strcat ( dcdlog, tdclog );
	    }
	}
	else
	{
	    strcpy ( dcdlog, tdclog );
	}

/*
**	Send a start up message to the decoder log.
*/
	ss_vers ( version, &ier, sizeof(version) );
	dc_wclg ( 0, "DCINIT", 3, version, &ier );

/*
**	Set all of the environment variables.
*/
	envobj = envhead;
	while ( envobj != NULL ) {
	    if  ( putenv ( envobj->env ) != 0 )  {
		strcpy ( errstr, envobj->env );
		dc_wclg ( 0, "DCINIT", -17, errstr, &ier );
	    }
	    envobj = envobj->next;
	}

/*
**	Adjust argc and argv by the option index.
*/
	argc -= optind;
	argv += optind;

/*
**	Initialize the output string array.
*/
	for ( i = 0; i < numexp; i++ )
	    strcpy ( parms[i], " " );

/*
**	Get the decoder specific parameters.
*/
	*num = argc;

	if  ( *num == 0 )
	{
/*
**	    If there are no parameters write a message and return
**	    with an error.
*/
	    *iret = -11;
	    dc_wclg ( 0, "DCINIT", *iret, " ", &ier );
	}
	else
	{
/*
**	    Otherwise, set the parameters to be returned.
*/
	    for ( i = 0; i < *num; i++ )
		if  ( i < numexp )  strcpy ( parms[i], argv[i] );
	}
}
