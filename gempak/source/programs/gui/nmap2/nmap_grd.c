#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"  
#include "nmap_data.h"  
#include "nmap_dttm.h"  


#define TIME_STR_LEN (MAX_PIXMAP*128)


/************************************************************************
 * nmap_grd.c                                                           *
 *                                                                      *
 * This module contains functions related to displaying grid data       *
 * for nmap.   								*
 *                                                                      *
 * CONTENTS:                                                            *
 *	grd_setAttr()							*
 *	grd_setEnsAttr()						*
 ***********************************************************************/

/*=====================================================================*/

void grd_setAttr ( dsrc_t *dsrc )
/************************************************************************
 * grd_setAttr                                                          *
 *                                                                      *
 * This function sets the data driver attributes for the grid source.   *
 *                                                                      *
 * void grd_setAttr ( dsrc )                                            *
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
 * S. Law/GSC		11/99	changed mdl -> grd			*
 * S. Jacobs/NCEP	 7/00	Removed special checks for model types	*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	Add bin mins and mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		isbcat, idx, ier, d3, d4, d5, d6, d7, d7m, d8, d8m,
	        mstrct, dionoff, dtmch;
    char	d1[256], d2[256];
    char	*cycle, rstfil[256], *pp;
    char	source[256], *alias;
/*---------------------------------------------------------------------*/

    strcpy (source, dsrc->path);
    alias  = strtok(source, "/");
    alias  = strtok(NULL, "/");
    cycle  = strtok(NULL, "/");

    /* 
     * convert cycle time to a GEMPAK date/time string
     */
    pp = strchr (cycle, '_');
    *pp = '/';


    dslw_getGrdResfile(dsrc->path, rstfil);

    idx    = dsrc->attridx;

    ctb_dtget ( alias, d1, d2, &d3, &isbcat, &d4, &d5, &d6,
		&dionoff, &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &ier );

    ngd_satt ( idx, alias, isbcat, cycle, rstfil, &(dsrc->attridx), &ier);
}

/*=====================================================================*/

void grd_setEnsAttr ( dsrc_t *dsrc )
/************************************************************************
 * grd_setEnsAttr                                                       *
 *                                                                      *
 * This function sets the data driver attributes for ensemble source.   *
 *                                                                      *
 * void grd_setEnsAttr ( dsrc )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *  *dsrc       dsrc_t   data source data structure                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/08 	initial coding                          *
 * M. Li/SAIC		05/08 	Add cycidx to dslw_getModList		*	
 ***********************************************************************/
{
    int		isbcat, idx, ier;
    char	mlist[LLMXLN], rstfil[256], cycidx[40];
/*---------------------------------------------------------------------*/

    dslw_getEnsResfile(dsrc->path, rstfil);
    dslw_getModList(dsrc->path, mlist, cycidx);


    idx    = dsrc->attridx;
    isbcat = SCAT_FCT;

    ngd_satt ( idx, mlist, isbcat, "", rstfil, &(dsrc->attridx), &ier);
}

/*=====================================================================*/
