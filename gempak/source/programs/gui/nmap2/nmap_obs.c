#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"
#include "nmap_data.h"  
#include "nmap_dttm.h"  


dttms_t _mosCycle;


/************************************************************************
 * nmap_obs.c                                                           *
 *                                                                      *
 * This module contains functions related to displaying observational   *
 * (surface and sounding) data    					*
 * for nmap.   								*
 *                                                                      *
 * CONTENTS:                                                            *
 *	obs_setAttr()		set the data driver attr		*
 ***********************************************************************/

/*=====================================================================*/

void obs_setAttr ( dsrc_t *dsrc )
/************************************************************************
 * obs_setAttr                                                          *
 *                                                                      *
 * This function sets the data driver attributes for the obs source     *
 *                                                                      *
 * void obs_setAttr ( dsrc )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  *dsrc       dsrc_t   data source data structure                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       09/99   initial coding                          *
 * E. Safford/GSC	01/00	delete print statements			*
 * S. Jacobs/NCEP	 5/00	Get the sub category from the table	*
 * S. Jacobs/NCEP	 9/02	Use the sub cat num to check cycle time	*
 * S. Jacobs/NCEP	12/02	Added check for type SCAT_SNF		*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
int     isbcat, idx, ier, d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct,
	dionoff, dtmch;
char	d1[256], d2[256];
char	cycle[256], parms[256], color[256], filter[256], txtatt[256];
char	level[256], vcord[256];
char	source[256], *alias, *subcat, sub_cat[256], *pp;
/*---------------------------------------------------------------------*/

    strcpy (source, dsrc->path);
    alias = strtok(source, "/");
    alias = strtok(NULL, "/");
    subcat = strtok(NULL, "/");

    if (strcmp (subcat, "standard") == 0) {
	strcpy (sub_cat, alias);
    }
    else {
	strcpy (sub_cat, subcat);
    }

    /*
     *  Get subcat code from datatype.tbl.
     */
    ctb_dtget ( alias, d1, d2, &d3, &isbcat, &d4, &d5, &d6, 
		&dionoff, &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &ier );

    if  ( isbcat == SCAT_SFF || isbcat == SCAT_SNF ) {
	strcpy (cycle, subcat);
	pp  = strchr (cycle, '_');
	*pp = '/';

	pp = strrchr(dsrc->path, '/');
	pp++;

	if (strcmp (pp, "standard") == 0) {
	    strcpy (sub_cat, alias);
	}
	else {
	    strcpy (sub_cat, pp);
	}
    }
    else {
        strcpy (cycle, "NONE");
    }


    ctb_plget(alias, sub_cat, "COLORS", color,  &ier);
    ctb_plget(alias, sub_cat, "FILTER", filter, &ier);
    ctb_plget(alias, sub_cat, "TEXT",   txtatt, &ier);
    
    idx = dsrc->attridx;

    if  ( isbcat == SCAT_SND || isbcat == SCAT_SNF ) {
	ctb_plget (alias, sub_cat, "LEVEL", level, &ier);
	ctb_plget (alias, sub_cat, "VCOORD", vcord, &ier);
        ctb_plget (alias, sub_cat, "SNPARM", parms,  &ier);
	nsn_satt ( idx, alias, isbcat, cycle, parms, color,
	       	level, vcord, filter, txtatt, &(dsrc->attridx), &ier);
    }
    else {
        ctb_plget(alias, sub_cat, "SFPARM", parms,  &ier);
        nsf_satt ( idx, alias, isbcat, cycle, parms,	
   	       	color, filter, txtatt, &(dsrc->attridx), &ier);
    }

}

/*=====================================================================*/
