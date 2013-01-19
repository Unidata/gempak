#include "nmpcmn.h"

void nmp_mkscl ( int lp, int ovl, char *mscale, int *iret )
/************************************************************************
 * nmp_mkscl                                                      	*
 *                                                                      *
 * This function composes a distance scale legend string                *
 *                                                                      *
 * void nmp_mkscl( lp, ovl, mscale, iret )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp	 	int	loop index					*
 *  ovl		int	overlay index                           	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *mscale	char	MSCALE string                               	*
 *  *iret	int	return code					*
 *			= -4 - invalid itype				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	08/04	Created					*
 ***********************************************************************/
{
int         icolor, ifont, ianchor, itstyle, iunit, ipos, val_opt;
char	    *anchor[] = {"LL", "LC", "LR", "CL", "CC", "CR", "UL", "UC", "UR"};
char	    *unit[] = {"SM", "NM", "KM"};
char	    val_txt[64];
float       lat, tsize, x, y;
int         itype, lat_opt, lblfrq, ier;
nmpovlstr_t ovlattr;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nmp_govlattr(ovl, lp, &itype, ovlattr, &ier);
    if ( ier == 0 ) {
	if (itype == 5) {
	    sscanf ( ovlattr, "%d %d %d %f %d %s %d %d %f %d",
			&icolor, &iunit, &lat_opt, &lat, &val_opt, val_txt, &ipos, 
			&ifont, &tsize, &itstyle);
	    if ( lat_opt == 0 ) {  /*  Auto mode for latitude  */
	        lat = RMISSD;
	    }
	    if ( val_opt == 0 ) {  /*  Auto mode for values  */
	        val_txt[0] = '\0';
	    }
	    switch ( ipos ) {
		case 0:  /* Upper Left */
		    ianchor = 6;
		    x = 0.025;
		    y = 0.995;
		    lblfrq = -1;
		    break;
		case 1:  /* Upper Center */
		    ianchor = 7;
		    x = 0.5;
		    y = 0.995;
		    lblfrq = -1;
		    break;
		case 2:  /* Upper Right */
		    ianchor = 8;
		    x = 0.995;
		    y = 0.995;
                    lblfrq = -1;            
		    break;
		case 3:  /* Lower Left */
		    ianchor = 0;
		    x = 0.025;
		    y = 0.005;
                    lblfrq = 1;            
		    break;
		case 4:  /* Lower Center */
		    ianchor = 1;
		    x = 0.5;
		    y = 0.005;
                    lblfrq = 1;            
		    break;
		case 5:  /* Lower Right */
		    ianchor = 2;
		    x = 0.995;
		    y = 0.005;
                    lblfrq = 1;            
		    break;
	    }
	    ifont = itstyle * 10 + ifont + 1;
            sprintf(mscale, "%d/%s/%f/%s/%s/%f;%f//%d|%f/%d",
                	icolor, unit[iunit], lat, val_txt,
			anchor[ianchor], x, y, lblfrq, tsize, ifont);
       }
       else {
	   *iret = -4;
       }
   }
}

/*=====================================================================*/
