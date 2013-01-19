#include "dccmn.h"

void dc_shnd ( int sig )
/************************************************************************
 * dc_shnd 								*
 *									*
 * This routine will process the given signal.				*
 *									*
 * dc_shnd ( sig )							*
 *									*
 * Input parameters:							*
 *	sig		int		Signal to process		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAi		 5/95						*
 * S. Jacobs/NMC	 7/95	Update and clean up			*
 * S. Jacobs/NCEP	 6/96	Updated documentation			*
 * K. Tyle/GSC		 1/97	Changed numerr = 0 to 2 in dc_wclg calls*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 ***********************************************************************/
{

	int	ier;
	char	errstr[DCMXLN];

/*---------------------------------------------------------------------*/

	signal ( sig, SIG_IGN );

/*
**	Process the trapped signal.
*/
	switch ( sig ) {

	    case SIGINT :
/*
**			Interrupt
**			Send a message to the log files and exit.
*/
			strcpy ( errstr, "Interrupt Signal" );
			dc_wclg ( 0, "DC", 2, errstr, &ier );

			dc_exit ( &ier );

	    case SIGTERM :
/*
**			Terminate
**			Send a message to the log files and exit.
*/
			strcpy ( errstr, "Terminate Signal" );
			dc_wclg ( 0, "DC", 2, errstr, &ier );

			dc_exit ( &ier );

	    case SIGPIPE :
/*
**			Broken pipe
**			Send a message to the log files and exit.
*/
			strcpy ( errstr, "Broken Pipe Signal" );
			dc_wclg ( 0, "DC", 2, errstr, &ier );

			dc_exit ( &ier );

	}

	signal ( sig, dc_shnd );

}

/*=====================================================================*/

void dc_sgnl ( void )
/************************************************************************
 * dc_sgnl 								*
 *									*
 * This routine sets all valid signals.					*
 *									*
 * dc_sgnl ( )								*
 *									*
 * Input parameters:							*
 *	NONE								*
 *									*
 * Output parameters:							*
 *	NONE								*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAi		 5/95						*
 * S. Jacobs/NMC	 7/95	Update and clean up			*
 * S. Jacobs/NCEP	 6/96	Updated documentation			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	(void) signal ( SIGINT,  dc_shnd );

	(void) signal ( SIGTERM, dc_shnd );

	(void) signal ( SIGPIPE, dc_shnd );

}
