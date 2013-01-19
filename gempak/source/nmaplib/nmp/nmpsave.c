#include "nmpcmn.h"

void nmp_save ( int *iret )
/************************************************************************
 * nmp_save                                                         	*
 *                                                                      *
 * This function copies all the stored map array values into		*
 * the saved_map array, copies all the stored overlay array values into	*
 * the saved_overlay array.						*
 *                                                                      *
 * void nmp_save ( iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            05/01   Combine nmp_svmap & nmp_savovl		*
 * J. Wu/SAIC           08/01   save off default_overlay[]		*
 * J. Wu/SAIC           08/01   move default_overlay saving to nmp_init	*
 ***********************************************************************/
{
int    lp, ii; 

/*---------------------------------------------------------------------*/
    *iret = 0;

    for (lp=0; lp<MAX_LOOP; lp++) {

	/*
	 * Save map array values
	 */
        strcpy(saved_map[lp].imgfile, maps[lp].imgfile);
        saved_map[lp].imgtyp = maps[lp].imgtyp;
        strcpy(saved_map[lp].mapfile, maps[lp].mapfile);
        strcpy(saved_map[lp].mapattr, maps[lp].mapattr);
	strcpy(saved_map[lp].map, maps[lp].map);
	strcpy(saved_map[lp].proj, maps[lp].proj);
	strcpy(saved_map[lp].garea[0], maps[lp].garea[0]);
	strcpy(saved_map[lp].garea[1], maps[lp].garea[1]);
	saved_map[lp].mode = maps[lp].mode;

	/*
 	 * Save overlay array values
	 */
	saved_overlay[lp].novl = overlay[lp].novl;

        for (ii =0; ii < saved_overlay[lp].novl; ii++ ) {
            saved_overlay[lp].mapovl[ii].ityp = overlay[lp].mapovl[ii].ityp;
            strcpy (saved_overlay[lp].mapovl[ii].attr,
                        overlay[lp].mapovl[ii].attr);
            strcpy (saved_overlay[lp].mapovl[ii].gname,
                        overlay[lp].mapovl[ii].gname);
            strcpy (saved_overlay[lp].mapovl[ii].title,
                        overlay[lp].mapovl[ii].title);
            saved_overlay[lp].mapovl[ii].active = overlay[lp].mapovl[ii].active;
        }

    }

}

/*=====================================================================*/
