#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"

typedef  struct {
    Boolean 	loadSPF;
    char    	*spFileName;
    Boolean 	loadVGF;
    char    	*vgFileName;
    Boolean 	autoLoadData;
    Boolean	showHelp;
} _cmdlinetbl;

static 	_cmdlinetbl 	_cmdLineTbl = { False, NULL, False, NULL, False, False };
	
/*
 *  Private functions
 */
static void cmdln_showHelp ( void );


/************************************************************************
 * nmap_cmdln.c                                                         *
 *                                                                      *
 * This module reads the command line options and parse to nmap2 to     *
 * support automatically reading in an SPF file and loading the data	*
 * without any user interaction.					*
 *                                                                      *
 * CONTENTS:                                                            *
 *      cmdln_parse()         parse the command line          		*
 *      cmdln_getLoadSPF()    whether or not to load SPF      		*
 *      cmdln_getLoadVGF()    whether or not to load VGF      		*
 *      cmdln_getShowHelp()   whether or not to show help     		*
 *      cmdln_getSPF()        get SPF file name               		*
 *      cmdln_getVGF()        get VGF file name               		*
 *      cmdln_getAutoLoadData()  whether or not to load data   		*
 *      cmdln_cleanUp()       free spFileName                 		*
 *      cmdln_showHelp()      print out help file              		*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/


void cmdln_parse ( int argc, char **argv, int *iret )
/************************************************************************
 * cmdln_parse                                                          *
 *                                                                      *
 * This function parses the command line.                               *
 *                                                                      *
 * void cmdln_parse ( argc, argv, iret )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc	int	number of parameters of command line            *
 *  **argv	char	parameter array of command line			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret	int     return code.                            	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/03                                           *
 ***********************************************************************/
{
    int        	ch;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( argc <= 1 ) {
	*iret = -1;
	return;
    }

    while ( ( ch = getopt ( argc, argv, "AaHhS:s:V:v:" )) != EOF ) {
   	switch ( ch ) {
	    case 'a':
	    case 'A':
		_cmdLineTbl.autoLoadData = True;
		break;

	    case 's':
	    case 'S': 
		if ( strlen (optarg) > (size_t)0 ) {
		    _cmdLineTbl.spFileName = (char *) malloc ( strlen (optarg) + 1 );
		    strcpy ( _cmdLineTbl.spFileName, optarg );
		    _cmdLineTbl.loadSPF = True;
		}
		break;

	    case 'v':
	    case 'V': 
		if ( strlen (optarg) > (size_t)0 ) {
		    _cmdLineTbl.vgFileName = (char *) malloc ( strlen (optarg) + 1 );
		    strcpy ( _cmdLineTbl.vgFileName, optarg );
		    _cmdLineTbl.loadVGF = True;
		}
		break;

	    case 'h':
	    case 'H':
		_cmdLineTbl.showHelp = True;
		cmdln_showHelp();
		break;
	}
    }

}

/*=====================================================================*/

Boolean cmdln_getLoadSPF ( void )
/************************************************************************
 * cmdln_getLoadSPF                                                     *
 *                                                                      *
 * This function returns a boolean value specifying whether the SPF 	*
 * file is to be loaded.						*
 *                                                                      *
 * Boolean cmdln_getLoadSPF ()                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *   cmdln_getLoadSPF	Boolean		True - load			*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/03                                           *
 ***********************************************************************/
{

    return _cmdLineTbl.loadSPF;

}

/*=====================================================================*/

Boolean cmdln_getLoadVGF ( void )
/************************************************************************
 * cmdln_getLoadVGF                                                     *
 *                                                                      *
 * This function returns a boolean value specifying whether the SPF 	*
 * file is to be loaded.						*
 *                                                                      *
 * Boolean cmdln_getLoadVGF ()                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *   cmdln_getLoadVGF	Boolean		True - load			*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 3/10	Created					*
 ***********************************************************************/
{

    return _cmdLineTbl.loadVGF;

}

/*=====================================================================*/


Boolean cmdln_getShowHelp ( void )
/************************************************************************
 * cmdln_getShowHelp                                                    *
 *                                                                      *
 * This function returns a boolean value specifying whether the help    *
 * file is shown.                                                	*
 *                                                                      *
 * Boolean cmdln_getShowHelp ()                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *   cmdln_getShowHelp  Boolean         True - show                     *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           01/04                                           *
 ***********************************************************************/
{

    return _cmdLineTbl.showHelp;

}

/*=====================================================================*/


void cmdln_getSPF ( char *filename )
/************************************************************************
 * cmdln_getSPF                                                     	*
 *                                                                      *
 * This function returns the SPF file name.				*
 *                                                                      *
 * Boolean cmdln_getSPF (filename )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*filename	char	SPF file name				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/03                                           *
 ***********************************************************************/
{

    strcpy ( filename, _cmdLineTbl.spFileName );

}

/*=====================================================================*/


void cmdln_getVGF ( char *filename )
/************************************************************************
 * cmdln_getVGF                                                     	*
 *                                                                      *
 * This function returns the VGF file name.				*
 *                                                                      *
 * Boolean cmdln_getVGF (filename )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*filename	char	VGF file name				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 3/10	Created					*
 ***********************************************************************/
{

    strcpy ( filename, _cmdLineTbl.vgFileName );

}

/*=====================================================================*/

Boolean cmdln_getAutoLoadData ( void )
/************************************************************************
 * cmdln_getAutoLoadData                                                *
 *                                                                      *
 * This function returns a boolean value specifying whether the data    *
 * is to be loaded automatically.                                       *
 *                                                                      *
 * Boolean cmdln_getAutoLoadData ()                                     *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * cmdln_getAutoLoadData	Boolean         True - load             *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/03                                           *
 ***********************************************************************/
{

    return _cmdLineTbl.autoLoadData;

}

/*=====================================================================*/

void cmdln_cleanUp ( void )                
/************************************************************************
 * cmdln_cleanUp                                                        *
 *                                                                      *
 * This function frees spFileName. 					*
 *                                                                      *
 * void cmdln_cleanUp ( )                                     		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/03                                           *
 * S. Jacobs/NCEP	 3/10	Added VGF clean up			*
 ***********************************************************************/
{

    if ( _cmdLineTbl.spFileName != NULL ) free ( _cmdLineTbl.spFileName );
    if ( _cmdLineTbl.vgFileName != NULL ) free ( _cmdLineTbl.vgFileName );

}

/*=====================================================================*/

static void cmdln_showHelp ( void )                
/************************************************************************
 * cmdln_showHelp                                                       *
 *                                                                      *
 * This function prints out the contents of nmap2.hlp file.             *
 *                                                                      *
 * void cmdln_showHelp ( )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/03                                           *
 * E.Safford/SAIC	02/04	nmap2.hlp moved				*
 ***********************************************************************/
{
    int		ier, ierr;
    char	filename[MXFLSZ], buff[256];
    FILE	*fptr;
/*---------------------------------------------------------------------*/

    ierr = 0;

    strcpy ( filename, "nmap2.hlp" );
    fptr = cfl_ropn ( filename, "$GEMHLP/hlp", &ier );

    if ( ier != 0 || fptr == (FILE *)NULL )  {
        printf("Error opening file %s\n", filename );
        return;
    }

    while ( ierr != 4 ) {
    	cfl_rdln ( fptr, sizeof(buff), buff, &ierr );
	if ( ierr == 0 ) printf ( "%s\n", buff );
    }

    cfl_clos ( fptr, &ier );

}

/*=====================================================================*/
