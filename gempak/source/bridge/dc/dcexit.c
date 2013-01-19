#include "dccmn.h"

void dc_exit ( int *iret )
/************************************************************************
 * dc_exit								*
 *                                                                      *
 * This routine writes a message to the log files, closes them, and	*
 * exits the program.							*
 *                                                                      *
 * dc_exit ( iret )							*
 *                                                                      *
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NMC	 7/95						*
 * S. Jacobs/NCEP	 6/96	Added "Normal Termination" message	*
 * S. Jacobs/NCEP	 6/96	Updated documentation; Changed dc_wlog	*
 *				to dc_wclg				*
 * S. Jacobs/NCEP	 7/96	Removed log file close			*
 * K. Tyle/GSC		 1/97	Change numerr values in termination and	*
 *				shutdown messages			*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 ***********************************************************************/
{
	char	errstr[DCMXLN];

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Send a shut down message to the decoder log, then
**	close the file.
*/
	dc_wclg ( 0, "DC", 5, " ", iret );
	sprintf ( errstr, "Number of bulletins read and processed: %u",
		  nbull );
	dc_wclg ( 0, "DC", 2, errstr, iret );
	dc_wclg ( 0, "DC", 6, " ", iret );

/*
**	Exit the program.
*/
	exit ( *iret );

}
