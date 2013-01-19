#include "geminc.h"
#include "gemprm.h"
#include "ercmn.h"


void er_wbuf ( int *iret )
/************************************************************************
 * er_wbuf								*
 *									*
 * This routine writes all of the error messages in the error message	*
 * buffer to the terminal.  The buffer is then re-initialized.		*
 *									*
 * er_wbuf ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 +1 = no msgs in buffer		*
 **									*
 * Log:									*
 * K. Tyle/GSC		12/96						*
 * K. Tyle/GSC		 1/97	Change nstr to nermsg			*
 * C. Lin/EAI		 1/97	Change msgstr to errmsg			*
 * S. Maxwell/GSC	 6/97	Documentation changes			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
	short int	i;
	int		ier;
/*---------------------------------------------------------------------*/
	*iret = 0;
/*
 *	Return error if there are no messages.
*/
	if ( nermsg == 0 ) {
	    *iret = 1;
	}
/*
 *	Write messages to the buffer.
 */
	else {
	    for ( i = 1; i <= nermsg; i++ ) {
		printf ("%s\n",errmsg[i-1]);
	    }
/*
 *	Re-initialize message buffer.
 */
	    er_init ( &ier );
	}
}
