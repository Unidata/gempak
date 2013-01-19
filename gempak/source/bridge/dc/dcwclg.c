#include "dccmn.h"

void dc_wclg ( int loglev, char *errgrp, int numerr, char *errstr, int *iret )
/************************************************************************
 * dc_wclg								*
 *									*
 * This routine is an interface for writing log messages from a C	*
 * language routine.							*
 *									*
 * dc_wclg  ( loglev, errgrp, numerr, errstr, iret )			*
 *									*
 * Input parameters:							*
 *	loglev		int		Logging level			*
 *					   0 = normal log message	*
 *					  >0 = level of verbosity	*
 *	*errgrp		char		Error group			*
 *	numerr		int		Number of chars in the message	*
 *	*errstr		char		String to be embedded in an	*
 *					  error OR full message to be	*
 *					  logged			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  Same as for DC_WLOG		*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

/*
**	Call the routine to create and write the log message.
*/
	dc_wlog ( &loglev, errgrp, &numerr, errstr, iret,
		  strlen(errgrp), strlen(errstr) );


}
