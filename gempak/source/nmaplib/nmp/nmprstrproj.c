#include "nmpcmn.h"
#include "Nxm.h"

void nmp_rstrproj ( int lp, int *iret )
/************************************************************************
 * nmp_rstrproj                                                         *
 *                                                                      *
 * This function sets the projection for the specified loop.         	*
 *                                                                      *
 * void nmp_rstrproj( lp, iret )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp                  int	loop index                      	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int	return code				*
 *				  -1 = invalid loop			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            03/01   Created                                 *
 ***********************************************************************/
{
int    	drpflg, which, ier;
/*---------------------------------------------------------------------*/

    if (lp < 0 || lp >= MAX_LOOP) {
	*iret = -1;
	return;
    }

    *iret = 0;
   
    if (strlen(maps[lp].garea[1]) > (size_t)0 ) {
        which = 1;
    }
    else {
	which = 0;
    }
    
    gg_maps( maps[lp].proj, maps[lp].garea[which], maps[lp].imgfile, 
    		&drpflg, &ier, 
		strlen(maps[lp].proj), strlen(maps[lp].garea[which]),
                strlen(maps[lp].imgfile) );

    if ( ier != G_NORMAL ) {
        NxmErr_update();
        *iret = ier;
    }

}

/*=====================================================================*/
