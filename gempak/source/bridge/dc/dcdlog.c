#include "dccmn.h"

void dc_dlog ( char *messag, int *lenm, int *iret )
/************************************************************************
 * dc_dlog								*
 *									*
 * This routine opens the decoder log file, writes the given message	*
 * and closes the file.							*
 *									*
 * dc_dlog  ( messag, lenm, iret )					*
 *									*
 * Input parameters:							*
 *	*messag		char		Message to write to the log	*
 *	*lenm		int		Number of chars in the message	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -2 = error writing to log	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/95						*
 * S. Jacobs/NCEP	 6/96	Updated documentation; Removed check	*
 *				for log file not opened; Changed to use	*
 *				a FILE stream with CFL_WRIT		*
 * S. Jacobs/NCEP	 7/96	Added open and close of log file	*
 * K. Tyle/GSC		 1/97	Remove prog. name and dattim; remove	*
 *				loglev variable				*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 ***********************************************************************/
{

	char	mesg[DCMXLN+8], tstr[12];
	int	lens, ier;

	FILE	*fplog;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Start the output message with the process ID.
*/
	cst_inch ( ipid, tstr, &ier );
	strcpy ( mesg, "[" );
	strcat ( mesg, tstr );
	strcat ( mesg, "] " );

/*
**	Concatenate the input message to the output string.
*/
	strcat ( mesg, messag );
	cst_lstr ( mesg, &lens, &ier );

/*
**	Get the length of the message and add a Line Feed and
**	a NULL to the end.
*/
	if  ( lens > (DCMXLN+8) - 2 ) {
	    mesg [lens-1] = CHLF;
	    mesg [lens]   = CHNULL;
	}
	else {
	    mesg [lens]   = CHLF;
	    mesg [++lens] = CHNULL;
	}

/*
**	Open the decoder log file.
*/
	fplog = NULL;
	if  ( dcdlog[0] == '-' )
	{
/*
**	    If the file name is "-", open standard error for logging.
*/
	    fplog = stderr;
	}
	else
	{
/*
**	    Try to open the decoder log file.
*/
	    fplog = cfl_aopn ( dcdlog, &ier );
	    if  ( ier != 0 )
	    {
/*
**		If there is an error opening the file, open standard
**		error for real-time processing, otherwise write an
**		error message to the LDM log.
*/
		if  ( irltim )
		{
		    fplog = stderr;
		}
		else
		{
		    *iret = -3;
		    return;
		}
	    }
	}

/*
**	Write the message to the log file.
*/
	cfl_writ ( fplog, lens, (unsigned char *)mesg, &ier );
	if  ( ier != 0 ) {
	    *iret = -2;
	}

/*
**	Close the decoder log file.
*/
	if  ( fplog == stderr )
	{
/*
**	    If standard error is being used for logging, do nothing.
*/
	}
	else if ( fplog != NULL )
	{
/*
**	    Otherwise, close the log file.
*/
	    cfl_clos ( fplog, &ier );
	}

}
