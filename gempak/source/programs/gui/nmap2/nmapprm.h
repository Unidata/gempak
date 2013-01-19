
/************************************************************************
 * nmapprm.h  								*
 * This header file contains the nmap parm definitions.			*
 *                                                                      *
 **                                                                     *
 * C.Lin/EAI            4/98						*
 * S. Jacobs/NCEP	11/98	Added DSP_MOS and SEL_MOS		*
 * S. Jacobs/NCEP	 5/99	Added DSP_WWN and SEL_WWN		*
 * S. Law/GSC		09/99	Added cursor parameters			*
 * S. Law/GSC		10/99	_WWN -> _MSC				*
 * E. Safford/GSC	12/99	move cursor definitions to gemprm.h	*
 * J. Wu/GSC		04/01	change logo_getNames			*
 * M. Li/SAIC		01/02	added MAPSET_APPLY, MAPSET_GET		*
 * H. Zeng/SAIC		09/04	added DEFAULT_NMAP2_ICON_NAME		* 
 ***********************************************************************/
#ifndef NMAPPRM_H
#define NMAPPRM_H

/*
 * data display mode
 */
#define DSP_SFC		1	/* surface */
#define DSP_IMG		2	/* image */
#define DSP_SNF		3	/* sounding data */
#define DSP_MDL		4	/* model */
#define DSP_VGF		5	/* vgf file */
#define DSP_MOS		6	/* MOS */
#define DSP_MSC		7	/* Watches and Warnings */

/*
 * data selection mode
 */
#define SEL_STN		1	/* station model based */
#define SEL_DIR		2	/* directory driven */
#define SEL_MDL		3	/* model table driven */
#define SEL_VGF		4	/* vgf */
#define SEL_MOS		5	/* mos, need to get times from data file */
#define SEL_MSC		6	/* Watches and Warnings */

#define TIMESTR_SIZE	12	/* GEMPAK time string size */

/*
 * logo definitions 
 */
#define LOGONUM		10
#define LOGOLEN		15
void 	logo_getNames ( int	*num,
			char	*logolist[LOGOLEN] );

/*
 * map setting selection
 */
#define	MAPSET_APPLY	0	/* Apply map settings 	*/
#define	MAPSET_GET	1	/* get map setting	*/


/*
 * default icon name
 */
#define DEFAULT_NMAP2_ICON_NAME "nmap2"


#endif
