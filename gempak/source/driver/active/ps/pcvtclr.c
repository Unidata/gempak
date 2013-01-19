#include "pscmn.h"
#include "color.h" 

void pcvtclr ( void )
/************************************************************************
 * pcvtclr                                                            *
 *                                                                      *
 * This function reads table cvtclr.psc and converts the curren	graphic	*
 * colors to cvtclr.psc table colors when device is changed to		*
 * PS driver.								*
 *                                                                      *
 * pcvtclr ()                                                           *
 *                                                                      *
 * Input parameters: 							*
 * Output parameters:							*
 *									*
 *			None						*
 **									*
 * Log:									*
 * W. Li/EAI		01/98 						*
 * T. Piper/SAIC	07/03	Added cfl_clos()			*
 ***********************************************************************/
{
int	local_index, ired, igreen, iblue, iret;
char	buffer[256], color_name[40], abrev_name[40], x_name[40];
FILE	*fp;

/*---------------------------------------------------------------------*/
	    
	/*
	 * searching table cvtclr.psc.
	 */ 

	fp = cfl_tbop("cvtclr.psc", "colors", &iret );

	if ( fp == NULL || iret != 0 ) {
            return;
	}

       /*
        * read a record
        */	

	while (!feof(fp)) {

	    cfl_trln(fp, 256, buffer, &iret);
	    if ( iret == 0){ 
	        sscanf(buffer, "%d %s %s %d %d %d %s", &local_index, 
	               color_name, abrev_name, &ired, &igreen, &iblue,
		       x_name );

	       /*
	        * changing colors
	        */

	        cscrgb( &local_index, &ired, &igreen, &iblue, &iret );
	    }
	}                  
	cfl_clos(fp, &iret);
}
