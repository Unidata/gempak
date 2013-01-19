#include "nmpcmn.h"
#include "Nxm.h"
#include "proto_xw.h"

#define DEF_MAPFILE     "nmap"         /* default map file alias */

void nmp_plot ( int lp, Pixmap pixmap, char panel[], int *iret )
/************************************************************************
 * nmp_plot                                                             *
 *                                                                      *
 * This function draw a map by calling the nmp_dspl routine		*
 *                                                                      *
 * void nmp_plot ( lp, pixmap, panel, iret )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop index 				*
 *  pixmap		Pixmap	pixmap for the plot area		*
 *  panel[]		char	panel location for the data		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int 	return code				*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		02/01	Created					*
 * E. Safford/GSC	05/01	bug in map draw (allow no base map)	*
 * T. Piper/SAIC	10/04	Added scale legend support (itype = 5)	*
 ***********************************************************************/
{
int    	ii, itype, ier;
char   	ltln_str[MAX_STR], stn_str[MAX_STR], parm[20];
Boolean flag;

/*---------------------------------------------------------------------*/

    *iret = 0;
/*
 *  Set the requested pixmap.
 */
    xscpxm (pixmap, &ier);
/*
 *  Set the GEMPAK "$MAPFIL" string.
 */
    strcpy(parm, "$MAPFIL");
    if( strlen(maps[lp].mapfile ) < (size_t)1) nmp_setmapstr(lp, &ier);

    ip_putv(parm, maps[lp].mapfile, &ier, strlen(parm), 
    					strlen(maps[lp].mapfile) );
/*
     * set projection
     */
    nmp_sproj(lp, &ier);


    /*
     * draw map, if needed
 */
    itype = 1;  

    if ( strlen(maps[lp].mapfile) > (size_t)0 ) {
        nmp_dspl( panel, &itype, maps[lp].mapfile, maps[lp].mapattr, &ier, 
		strlen(panel), strlen(maps[lp].mapfile), 
		strlen(maps[lp].mapattr) );
    }

    if ( ier != G_NORMAL ) NxmErr_update();
/*
 *  Draw LAT/LON and station plot when necessary and centroid.
 */
    for ( ii = 0; ii < overlay[lp].novl; ii++ ) {

        flag = overlay[lp].mapovl[ii].active;
        itype = overlay[lp].mapovl[ii].ityp;

        if (flag) {
           switch (itype) {
                case 0:         /* LAT/LON */
                    nmp_gltln(lp, ltln_str, &ier);
                    nmp_dspl( panel, &itype, maps[lp].mapfile, ltln_str, &ier, 
			strlen(panel), strlen(maps[lp].mapfile), strlen(ltln_str) );
                    break;

                case 2:         /* station plot */
                    nmp_mkstn (lp, ii, stn_str, &ier);
		    nmp_dspl( panel, &itype, "", stn_str, &ier, 
			strlen(panel), 0, strlen(stn_str) );
		    break;

		case 5:		/* distance scale legend */
		    nmp_mkscl (lp, ii, stn_str, &ier); 
		    nmp_dspl( panel, &itype, "", stn_str, &ier,
			strlen(panel), 0, strlen(stn_str) );
		    break;
           }
           if ( ier != G_NORMAL )  NxmErr_update();
	} /* end of if (flag)  */
    }
    if ( ier != 0 )  *iret = ier;
}
