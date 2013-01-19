#include "geminc.h"
#include "gemprm.h"
#include "proto_ntrans.h"

void graf_init ( void )
/************************************************************************
 *                                                                      *
 * graf_init()                                                          *
 *                                                                      *
 * this function print to a postscript file                             *
 *                                                                      *
 * void graf_init()		                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC     1/97        				                *
 * I. Durham/GSC   5/98		Changed call for underscore		*
 * T. Piper/SAIC	07/03	Replaced xwcmn.h with gemprm.h		*
 ***********************************************************************/
{
int	  ixtype, iytype, iret;
float	  yx, xll, yll, xul, yul;

/*---------------------------------------------------------------------*/

	ixtype	= 1;
	iytype	= 1;
	yx	= 1.0F;
	xll	= 0.0F;
	yll	= 0.0F;
	xul	= 32767.0F;
	yul	= 32767.0F;

	gsgraf( &ixtype, &iytype, &yx, &xll, &yll, &xul, &yul, &iret);
}
