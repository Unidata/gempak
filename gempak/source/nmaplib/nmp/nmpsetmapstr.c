#include "nmpcmn.h"

void nmp_setmapstr ( int lp, int *iret )
/************************************************************************
 * nmp_setmapstr                                                       	*
 *                                                                      *
 * This function composes and sets the map and map attribute string	*
 * in the Maps structure.						*
 *                                                                      *
 * void nmp_setmapstr ( lp, iret )                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop index				*
 * Output parameters:                                                   *
 *      *iret           int     Return code                     	*
 *			  	= -16 - mapfile string overrun		*
 *				= -17 - map string overrun		*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            02/01   Created                                 *
 ***********************************************************************/
{
char 	     map_str[MAX_STR], mapfil_str[MAX_STR], attrstr[MAX_STR];
char	     tmpstr[MAX_STR];
nmpovlstr_t  ovlattr;
int 	     jj, ier, intattr[3], ii, itype;
Boolean	     flag;
/*--------------------------------------------------------------------*/

	*iret = 0;

        map_str[0]    = '\0';
        mapfil_str[0] = '\0';

        for ( jj = overlay[lp].novl-1;  jj >= 0; jj-- ) {

            if ( overlay[lp].mapovl[jj].ityp == 1 ) {

                flag = overlay[lp].mapovl[jj].active;

                if (flag) {

                    /*
                     * compose the MAPFILE string
                     */
                    if ( mapfil_str[0] == '\0' ) {
                        strcpy( mapfil_str, overlay[lp].mapovl[jj].gname);
                    }
                    else {
                        strcpy( tmpstr, overlay[lp].mapovl[jj].gname);
                        if ( strlen(mapfil_str) +
                                strlen(tmpstr) + 1 < (size_t)MAX_STR ) {
                            strcat( mapfil_str, "+");
                            strcat( mapfil_str, tmpstr);
                        }
                        else {
			    *iret = -16;
                            break;
			}
                    }

                    /*
                     * compose the map attribute string
                     */
                    nmp_govlattr(jj, lp, &itype, ovlattr, &ier);
                    cst_ilst(ovlattr, ' ', 1, 3, intattr, &ii, &ier);
                    sprintf(attrstr, "%d/%d/%d",
                                intattr[0], intattr[1], intattr[2]);

                    if ( map_str[0] == '\0' ) {
                        strcpy( map_str, attrstr);
                    }
                    else {
                        if ( strlen(map_str) +
                                strlen(attrstr) + 1 < (size_t)MAX_STR ) {
                            strcat( map_str, "+");
                            strcat( map_str, attrstr);
                        }
			else {
			    *iret = -17;
			}
                    }
                }
            }
        }


        strcpy( maps[lp].mapfile, mapfil_str);
        strcpy( maps[lp].mapattr, map_str);

}

/*=====================================================================*/
