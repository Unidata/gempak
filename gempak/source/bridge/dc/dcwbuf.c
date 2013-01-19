#include "dccmn.h"


void dc_wbuf ( int *iret )
/************************************************************************
 * dc_wbuf								*
 *									*
 * This routine will write all of the error messages stored in the 	*
 * error message buffer MSGSTR to a log file, via a call to DC_DLOG. 	*
 * The buffer is then flushed.						*
 *									*
 * dc_wbuf ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					   1 = empty buffer		*
 *									*
 **									*
 * Log:									*
 * K. Tyle/GSC		 1/97						*
 * K. Tyle/GSC		 1/97	Change nstr to nermsg			*
 * C. Lin/EAI		 1/97	Change msgstr to errmsg			*
 * J. Wu/SAIC		 1/04	call er_gnumerr & er_gerrmsg		*
 * J. Wu/SAIC		 2/04	modify call to er_gerrmsg		*
 * T. Piper/SAIC	10/06	Increased errmsg to 513			*
 ***********************************************************************/
{
	int 		ii, ier, lenm, nermsg;
	char 		errmsg[513];
/*---------------------------------------------------------------------*/

	*iret = 0;
	
	er_gnumerr ( &nermsg, &ier );
	
	if ( nermsg == 0 ) {
	    *iret = 1;
	}
	else {
	    for ( ii = 0; ii < nermsg; ii++ ) {
		er_gerrmsg ( &ii, errmsg, &ier );
		cst_lstr ( errmsg, &lenm, &ier ); 
		dc_dlog ( errmsg, &lenm, &ier );
	    }
	    
	    er_init ( &ier );
	}
}
