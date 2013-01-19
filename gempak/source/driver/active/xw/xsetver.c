#include "xwcmn.h"

void xsetver ( Boolean nmap2_value )
/************************************************************************
 * xsetver 								*
 *									*
 *  This routine sets the nmap2 (or version) flag in the gemwindow      *
 *  structures.  A value of TRUE signifies nmap2.  The xclear (gclear)  *
 *  function uses this flag to determine when to wipe all existing      *
 *  pixmaps.  FALSE (the default setting) means that calls to gclear    *
 *  will behave as nmap expects, while TRUE means gclear will behave as *
 *  nmap2 requires.							*
 *									*
 * xsetver   ( nmap2_value )						*
 *									*
 * Input parameters:							*
 *	nmap2_value	Boolean		new value for nmap2 flag  	*
 *									*
 * Output parameters:							*
 *	NONE								*
 **									*
 * Log:									*
 * E. Safford/GSC	02/99	initial coding                    	*
 * E. Safford/GSC	05/00	renamed from xsvflg               	*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii=0; ii<MAX_WINDOW; ii++) {
        gemwindow[ii].nmap2 = nmap2_value;
    }
}
