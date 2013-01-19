#include "nmpcmn.h"

void nmp_restore ( int *iret )
/************************************************************************
 * nmp_restore                                                        	*
 *                                                                      *
 * This function copies all the saved map array values into		*
 * the maps array, copies all the saved_overlay array values into	*
 * the overlay array.							*
 *                                                                      *
 * void nmp_restore ( iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            05/01   combine nmp_restormap & nmp_restorovl	*
 ***********************************************************************/
{
int    lp, ii; 

/*---------------------------------------------------------------------*/
    *iret = 0;

    for (lp=0; lp<MAX_LOOP; lp++) {

	/*
	 * Restore the saved map array values
	 */ 
        strcpy(maps[lp].imgfile, saved_map[lp].imgfile);
        maps[lp].imgtyp = saved_map[lp].imgtyp;
        strcpy(maps[lp].mapfile, saved_map[lp].mapfile);	
        strcpy(maps[lp].mapattr, saved_map[lp].mapattr);
	strcpy(maps[lp].map, saved_map[lp].map);
	strcpy(maps[lp].proj, saved_map[lp].proj);
	strcpy(maps[lp].garea[0], saved_map[lp].garea[0]);
	strcpy(maps[lp].garea[1], saved_map[lp].garea[1]);
	maps[lp].mode = saved_map[lp].mode;

	/*
	 * Restore the saved_overlay array values
	 */
	overlay[lp].novl = saved_overlay[lp].novl;

        for (ii =0; ii < overlay[lp].novl; ii++ ) {
            overlay[lp].mapovl[ii].ityp = saved_overlay[lp].mapovl[ii].ityp;
            strcpy (overlay[lp].mapovl[ii].attr,
                        saved_overlay[lp].mapovl[ii].attr);
            strcpy (overlay[lp].mapovl[ii].gname,
                        saved_overlay[lp].mapovl[ii].gname);
            strcpy (overlay[lp].mapovl[ii].title,
                        saved_overlay[lp].mapovl[ii].title);
            overlay[lp].mapovl[ii].active = saved_overlay[lp].mapovl[ii].active;
        }

    }

}

/*=====================================================================*/
