#include "nmpcmn.h"

void nmp_mkstn ( int lp, int ovl, char *stn_str, int *iret )
/************************************************************************
 * nmp_mkstn                                                      	*
 *                                                                      *
 * This function composes a station plot string                         *
 *                                                                      *
 * void nmp_mkstn( lp, ovl, stn_str, iret )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp	 	int	loop index					*
 *  ovl		int	overlay index                           	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *stn_str	char	STNPLT string                               	*
 *  *iret	int	return code					*
 *			= -4 - invalid itype				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		02/01	Created					*
 * J. Wu/SAIC		08/03	allow float number in marker attr	*
 ***********************************************************************/
{
int         mcolor, tcolor, width, type_id, txt_size_id;
float       size, tsize_f;
int         marker_state, itype, ier;
nmpovlstr_t ovlattr;
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    nmp_govlattr(ovl, lp, &itype, ovlattr, &ier);
    if ( ier == 0 ) {

       if (itype == 2) {
	   sscanf ( ovlattr, "%d %d %f %d %d %d",
			&mcolor, &type_id, &size, &marker_state,
			&txt_size_id, &width );
			
	   /*
            * get txt_size from table fontsz.tbl
            */
	   ctb_fszrd(&ier);
           ctb_fszval(txt_size_id, &tsize_f, &ier);

           /*
            * make text string color the same as marker color
            */
           tcolor = mcolor;

           if ( marker_state > 0 ) {

               /*
                * marker_state = 1 -> marker + text.
                * marker_state = 2 -> text only.
                */
                mcolor = (marker_state == 2 ) ? 0 : mcolor;
           }
           else {

               /*
                * marker_state = 0 -> marker only.
                */

                tcolor = 0;
           }

           sprintf(stn_str, "%d/%.3f|%d/%d/%2.1f/%d|%s", tcolor, tsize_f,
                mcolor, type_id, size, width, overlay[lp].mapovl[ovl].gname);

       }
       else {
	   *iret = -4;
       }
   }

}

/*=====================================================================*/
