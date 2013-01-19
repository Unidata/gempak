#include "geminc.h"
#include "gemprm.h"
#include "nmap_stnm.h"

void stnm_copy ( stnm_t *s1, stnm_t *s2 );

/* NOTE:  This function is NOT used.
 *	Can we delete it?
 */
/************************************************************************
 * nmap_stnm.c                                                          *
 *                                                                      *
 * This module contains the station model object for nmap.          	*
 *                                                                      *
 * CONTENTS:                                                            *
 *      stnm_copy()   copy one station model structure to another.    	*
 ***********************************************************************/

/*=====================================================================*/

void stnm_copy ( stnm_t *s1, stnm_t *s2 ) 
/************************************************************************
 * stnm_copy                                              		*
 *                                                                      *
 * This function copies station model S2 to S1.   			*
 *                                                                      *
 * void stnm_copy ( s1, s2 )                   				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *s1       stnm_t  destination station model structure               *
 *  *s2       stnm_t  original station model structure                  *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * C. Lin/EAI      04/97	take out marker field  			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	s1->skpmis = s2->skpmis;
	s1->filter = s2->filter;

	strcpy(s1->parm,   s2->parm);
	strcpy(s1->colors, s2->colors);

	s1->ifont  = s2->ifont;
	s1->ihwsw  = s2->ihwsw;
	s1->itxtw  = s2->itxtw;
	s1->txtsiz = s2->txtsiz;

}

/*=====================================================================*/
