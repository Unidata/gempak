#include "dccmn.h"

/*
** Define and initialize the finite-state machine for finding a
** bulletin in the buffer.
*/
static fstate_t finite_state = START;


void dc_gbul ( char *bulletin, int *lenb, int *ifdtyp, int *iret )
/************************************************************************
 * dc_gbul								*
 *									*
 * This routine will check the buffer for data then return a complete	*
 * bulletin to the decoder.						*
 *									*
 * dc_gbul ( bulletin, lenb, ifdtyp, iret )				*
 *									*
 * Output parameters:							*
 *	*bulletin	char		Bulletin			*
 *	*lenb		int		Length of bulletin		*
 *	*ifdtyp		int		Feed type of data		*
 *					  0 = WMO			*
 *					  1 = AFOS			*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAi		 6/95						*
 * S. Jacobs/NMC	 7/95	Clean up				*
 * S. Jacobs/NCEP	 4/96	Added AFOS data feed type		*
 * S. Jacobs/NCEP	 6/96	Updated documentation			*
 * E. Wehner/EAI	 7/96	Added more checking for state machine	*
 * J. Whistler/AWC	 8/96	Changed call to dcb_peekc		*
 * S. Chiswell/UPC	 1/99	Fixed CR_CR_ state when CR_ is found	*
 * S. Chiswell/UPC	 2/04	handle EBFULL to continue looking	*
 ***********************************************************************/
{

	static unsigned char	ch;
	int			ier;

/*---------------------------------------------------------------------*/
	*iret = 0;
	*lenb = 0;

/*
**	Loop forever. The loop will be broken if a complete
**	bulletin is found.
*/
	while ( 1 ) {

/*
**	    Check for an empty buffer.
*/
	    if  ( ( finite_state != START ) ||
		  ( dcb_isempt() == 1 ) ) {

/*
**		Read from standard input to the buffer.
*/
		if  ( ( ier = dcb_put ( STDIN_FILENO, itmout ) ) < 0 ) {

		    if  ( ier == EBFULL ) {
/*
**			If buffer overflowed, start over looking for 
**			bulletins that we can fit into buffer. 
*/
			dcb_sbtl ( 0 );
			ier = dcb_getb ( bulletin );
			/* reset start (Chiz) */
			finite_state = START;
			continue;
		    }

		    else {
/*
**		        Timeout or end of data file has occurred.
*/
			*iret = ier;
			return;
		    }
		}

	    }

/*
**	    Scan the input buffer.
*/
	    while ( ( dcb_peekc ( &ch ) ) != EOD ) {
		switch ( finite_state ) {

/*
**		    If at the start, check for a Control-A or a "Z".
*/
		    case START:
			if  ( ch == CHCTLA ) {
			    finite_state = SOH_;
			}
			else if  ( ch == 'Z' ) {
			    finite_state = Z_;
			}
			else {
			    finite_state = NOT_IN_BULLETIN;
			}
			break;

/*
**		    If not inside a bulletin, check for a Control-A
**		    or a "Z".
*/
		    case NOT_IN_BULLETIN:
			if  ( ch == CHCTLA ) {
			    finite_state = SOH_;
			}
			else if  ( ch == 'Z' ) {
			    finite_state = Z_;
			}
			else {
			    finite_state = NOT_IN_BULLETIN;
			}
			break;

/*
**		    If Control-A has been found, check for a
**		    Carriage Return.
*/
		    case SOH_:
			if  ( ch == CHCR ) {
			    finite_state = SOH_CR_;
			}
			else {
			    if (ch == 'Z')
			    {
				finite_state = Z_;
			    }
			    else
			    {
			        finite_state = NOT_IN_BULLETIN;
			    }
			}
			break;

/*
**		    If Control-A and Carriage Return have been found,
**		    check for another Carriage Return.
*/
		    case SOH_CR_:
			if  ( ch == CHCR ) {
			    finite_state = SOH_CR_CR_;
			}
			else {
			    if ( ch == 'Z' )
			    {
				finite_state = Z_;
			    }
			    else
			    {
			        finite_state = NOT_IN_BULLETIN;
			    }
			}
			break;

/*
**		    If Control-A and two Carriage Returns have been
**		    found, check for a Line Feed.
*/
		    case SOH_CR_CR_:
			if  ( ch == CHLF ) {
			    dcb_sbhd ( -3 );
			    finite_state = IN_BULLETIN;
			    *ifdtyp = 0;
			}
			else {
			    if (ch == 'Z')
			    {
 				finite_state = Z_;
   			    }
			    else
			    {
			        finite_state = NOT_IN_BULLETIN;
			    }
			}
			break;

/*
**		    If a "Z" has been found, check for a "C".
*/
		    case Z_:
			if  ( ch == 'C' ) {
			    finite_state = Z_C_;
			}
			else {
			    if  ( ch == CHCTLA ) 
                            {
			        finite_state = SOH_;
			    }
                            else
                            {
                                if (ch == 'Z' )
				{
				    finite_state = Z_;
				}
				else
                                {
			            finite_state = NOT_IN_BULLETIN;
  				}
                            }
			}
			break;

/*
**		    If a "Z" and a "C" have been found, check for
**		    another "Z".
*/
		    case Z_C_:
			if  ( ch == 'Z' ) {
			    finite_state = Z_C_Z_;
			}
			else
                        {
                            if (ch == CHCTLA )
			    {
			        finite_state = SOH_;
			    }
			    else
                            {
			        finite_state = NOT_IN_BULLETIN;
  			    }
			}
			break;

/*
**		    If a "Z", a "C" and a second "Z" have been found,
**		    check for another "C".
*/
		    case Z_C_Z_:
			if  ( ch == 'C' ) {
			    dcb_sbhd ( -3 );
			    finite_state = IN_BULLETIN;
			    *ifdtyp = 1;
			}
			else 
			{
  			    if ( ch == CHCTLA )
                   	    {
			        finite_state = SOH_;
 			    }
			    else
			    {
				if ( ch == 'Z' )
 				{
				    finite_state = Z_;
				}
				else
				{
			            finite_state = NOT_IN_BULLETIN;
				}
			    }
			}
			break;

/*
**		    If inside the bulletin, check for a Carriage Return.
*/
		    case IN_BULLETIN:
			if  ( *ifdtyp == 0 ) {
			    if  ( ch == CHCR ) {
				finite_state = CR_;
			    }
			    else {
				finite_state = IN_BULLETIN;
			    }
			}
			else {
			    if  ( ch == 'N' ) {
				finite_state = N_;
			    }
			    else {
				finite_state = IN_BULLETIN;
			    }
			}
			break;

/*
**		    If Carriage Return has been found, check for
**		    another Carriage Return.
*/
		    case CR_:
			if  ( ch == CHCR ) {
			    finite_state = CR_CR_;
			}
			else {
			    finite_state = IN_BULLETIN;
			}
			break;

/*
**		    If two Carriage Returns have been found, check for
**		    a Line Feed.
*/
		    case CR_CR_:
			if  ( ch == CHLF ) {
			    finite_state = CR_CR_NL_;
			}
			else 
			{
   			    if (  ch == CHCR )
			    {
				finite_state = CR_CR_;
  			    }
      			    else
			    {
			        finite_state = IN_BULLETIN;
			    }
			}
			break;

/*
**		    If two Carriage Returns and a Line Feed have been
**		    found, check for a Control-C.
*/
		    case CR_CR_NL_:
			if  ( ch == CHCTLC ) {
/*
**			    If it is a Control-C, reset the search, then
**			    retrieve and return the complete bulletin.
*/
			    finite_state = START;
			    dcb_sbtl ( 0 );
			    ier = dcb_getb ( bulletin );
			    if  ( ier < 0 )  {
				*iret = ier;
			    }
			    else {
				*lenb = ier;
				nbull++;
			    }
			    return;
			}
			else if ( ch == CHCR ) {
			    finite_state = CR_;
			}
			else {
			    finite_state = IN_BULLETIN;
			}
			break;

/*
**		    If there is one "N" search for three more.
*/
		    case N_:
			if  ( ch == 'N' ) {
			    finite_state = N_N_;
			}
			else {
			    finite_state = IN_BULLETIN;
			}
			break;
				
		    case N_N_:
			if  ( ch == 'N' ) {
			    finite_state = N_N_N_;
			}
			else {
			    finite_state = IN_BULLETIN;
			}
			break;
				
		    case N_N_N_:
			if  ( ch == 'N' ) {
/*
**			    If there are four "N"s in row, retrieve
**			    and return the complete bulletin.
*/
			    finite_state = START;
			    dcb_sbtl ( 0 );
			    ier = dcb_getb ( bulletin );
			    if  ( ier < 0 )  {
				*iret = ier;
			    }
			    else {
				*lenb = ier;
				nbull++;
			    }
			    return;
			}
			else {
			    finite_state = IN_BULLETIN;
			}
			break;
				
		}

	    }

	}

} 
