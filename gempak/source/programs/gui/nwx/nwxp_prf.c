#include "nwx_cmn.h"


/************************************************************************
 * nwx_prf.c                                                            *
 *                                                                      *
 * This module for decoding 72 PRF PRG data.                            *
 *                                                                      *
 * CONTENTS:                                                            *
 *      prf_decode()   decode 72 PRF PRG data into plotting structure.  *
 ***********************************************************************/

/*=====================================================================*/

void prf_decode ( char *text, stnlist_t *stnlist, plotdata_t *plotdata )
/************************************************************************
 * prf_decode								*
 *									*
 * This routines decodes the 72 PRF PRG report text and sets the 	*
 * marker attriubtes. The text will be altered after this process.	*
 *									*
 * prf_decode(text, stnlist, plotdata )					*
 *									*
 * Input parameters:							*
 *	*text		char		Report text			*
 *	*stnlist	stnlist_t	Station list to be checked	*
 *									*
 * Output parameters:							*
 *      *plotdata       plotdata_t      Data to be plotted              *
 *									*
 * Return paramters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	01/95						*
 * C. Lin/EAI	        09/95						*
 * D. Kidwel/NCEP        5/99	Replaced MAX_PLOT_STN with LLSTFL	*
 ***********************************************************************/
{
struct {
	char	station[4];
	int	temp;
} prf_info[LLSTFL];
	
char   *ptr, *str, *start;
int    i, j, x, k;
int    found_flg;
size_t    len;

/*---------------------------------------------------------------------*/
/*
 * Set marker attributes
 */
        plotdata->plt_mark.ncolor   = 14;
        plotdata->plt_mark.marktype = 5;
        plotdata->plt_mark.marksize = 1.0F;
        plotdata->plt_mark.markwdth = 2;
        plotdata->plt_mark.pltflag  = G_TRUE;
        plotdata->plt_mark.iposn    = 0;

        plotdata->plt_mark.icolrs[0] = 30;
        plotdata->plt_mark.icolrs[1] = 29;
        plotdata->plt_mark.icolrs[2] = 28;
        plotdata->plt_mark.icolrs[3] = 27;
        plotdata->plt_mark.icolrs[4] = 26;
        plotdata->plt_mark.icolrs[5] = 25;
        plotdata->plt_mark.icolrs[6] = 4;
        plotdata->plt_mark.icolrs[7] = 22;
        plotdata->plt_mark.icolrs[8] = 3;
        plotdata->plt_mark.icolrs[9] = 21;
        plotdata->plt_mark.icolrs[10] = 20;
        plotdata->plt_mark.icolrs[11] = 19;
        plotdata->plt_mark.icolrs[12] = 17;
        plotdata->plt_mark.icolrs[13] = 2;

        plotdata->plt_mark.breaks[0] = -1;
        plotdata->plt_mark.breaks[1] = 9;
        plotdata->plt_mark.breaks[2] = 19;
	plotdata->plt_mark.breaks[3] = 29;
	plotdata->plt_mark.breaks[4] = 39;
	plotdata->plt_mark.breaks[5] = 49;
	plotdata->plt_mark.breaks[6] = 59;
	plotdata->plt_mark.breaks[7] = 69;
	plotdata->plt_mark.breaks[8] = 79;
	plotdata->plt_mark.breaks[9] = 89;
	plotdata->plt_mark.breaks[10] = 99;
	plotdata->plt_mark.breaks[11] = 109;
	plotdata->plt_mark.breaks[12] = 119;

/*
 * Locate information and store into buffer
 */
	start = strstr(text, "ABQ");
	if (!start) return; 

/*
 * substitue newline(\n) with white space
 */
	ptr = start;
	while (*ptr) {
		if ( *ptr == '\n' ) *ptr = ' ';
		ptr++;
	}

/*
 * Store information into struct
 */
	k = 0;
	str = strtok(start, " ");
	found_flg = 0;
	while ( str ) { 

/*
 * check the station name
 */
	       if ( (dchk_alpha(str) == 0) && ( !found_flg ) ) {
	          strcpy(prf_info[k].station, str);
		  found_flg = 1;
	       }
	       else {

/*
 * check the temperature data
 */
		  if ( (dchk_digit(str) == 0) && ( found_flg ) ) {
	             prf_info[k].temp = atoi(str);		
		     found_flg = 0;
		     ++k;
		  }
	       }
	
/*
 * get next token
 */
	       str = strtok(NULL, " ");

	}

/*
 * Determine data values to plot
 */
	x=0;
	for ( i = 0; i < k; i++ ) {
		len = strlen(prf_info[i].station);
		for ( j = 0; j < stnlist->nstn; j++) {
		    if (strncmp(stnlist->srchstr[j], prf_info[i].station,
			 len ) == 0){
			plotdata->plt_mark.lat[x] = stnlist->lat[j];
			plotdata->plt_mark.lon[x] = stnlist->lon[j];
			plotdata->plt_mark.dvalues[x] = prf_info[i].temp;
			x++;
			break;
		    }
		}
	}

	plotdata->plt_mark.nstn = x;
}
