#include "gbcmn.h"
#include "gb2def.h"

void gb2_ctim ( int time, char *param )
/************************************************************************
 * gb2_ctim								*
 *									*
 * This routine replaces the "--" characters in a GEMPAK parameter      *
 * string with the number specified in variable time.                   *
 *									*
 * gb2_ctim ( time, param )	                			*
 *									*
 * Input parameters:							*
 *	time  	        int             Integer time range              *
 *									*
 * Input/Output parameters:						*
 *      *param         char             GEMPAK parameter string         *
 **									*
 * Log:									*
 * S. Gilbert       12/04                                               *
 * T. Piper/SAIC	07/06	Replaced strncpy with cst_ncpy		*
 ***********************************************************************/
{
    int  ier, len;
    char ctim[12], dash[]="--", *cptr;

    /*
    **   Convert integer to character
    */
    if ( time != IMISSD ) 
       if ( time >= 0 && time < 10 )
          sprintf ( ctim, "%.2d", time);
       else
          cst_inch ( time, ctim, &ier);
    else 
	cst_ncpy(ctim, "XX", 2, &ier);

    /*
    **   replace "--" or append time character in parameter string.
    */
    cptr = strstr( param, dash);
    if ( cptr != 0 )
       cst_rpst( param, dash, ctim, param, &ier);
    else
       if ( time > 0 )  cst_ncat( param, ctim, &len, &ier);

}
