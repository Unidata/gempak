#include "nwx_cmn.h"


/************************************************************************
 * uvi.c                                                                *
 *                                                                      *
 * This module for decoding UVI data.         				*
 *                                                                      *
 * CONTENTS:                                                            *
 *      uvi_decode()   decode uvi data into plotting structure.         *
 ***********************************************************************/

/*=====================================================================*/

void uvi_decode ( char *text, stnlist_t *stnlist, plotdata_t *plotdata )
/************************************************************************
 * uvi_decode								*
 *									*
 * This routine decodes the UVI report text and sets the marker		*
 * attributes.								*
 * 									*
 *									*
 * uvi_decode(text, stnlist, plotdata)					*
 *									*
 * Input parameters:							*
 *	*text		char		Report text 			*
 *	*stnlist	stnlist_t	station list to be checked 	*
 *									*
 * Output parameters:							*
 *	*plotdata	plotdata_t	data info for plotting		*
 *									*
 * Return paramters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	12/94						*
 * C. Lin/EAI	         9/95						*
 * S. Jacobs/NCEP	 3/99	Removed & from string var in sscanf	*
 * D. Kidwell/NCEP	 5/99	Replaced MAX_PLOT_STN with LLSTFL       *
 * T. Piper/SAIC	03/04	Adjusted breaks based upon SCN 04-08	*
 ***********************************************************************/
{
struct uvi_info {
	char	city[30];
        char	state[3];
	char	uvi[5];
} table[LLSTFL];

char	*ptr, *ptr2, *line;
char    tmp[80];
int	i, j, x, count;
int     len;
size_t	city_len, state_len;

/*---------------------------------------------------------------------*/
/*
 * Set marker attributes
 */
	plotdata->plt_mark.ncolor    = 5;
	plotdata->plt_mark.marktype  = 5;
	plotdata->plt_mark.marksize  = 1.0F;
	plotdata->plt_mark.markwdth  = 2;
	plotdata->plt_mark.iposn     = 0;
	plotdata->plt_mark.pltflag   = G_TRUE;

	plotdata->plt_mark.icolrs[0] = 25;
	plotdata->plt_mark.icolrs[1] = 3;
	plotdata->plt_mark.icolrs[2] = 5;
	plotdata->plt_mark.icolrs[3] = 17;
	plotdata->plt_mark.icolrs[4] = 2;

	plotdata->plt_mark.breaks[0] = 2;
	plotdata->plt_mark.breaks[1] = 5;
	plotdata->plt_mark.breaks[2] = 7;
	plotdata->plt_mark.breaks[3] = 10;	

/*
 * Locate UVI information
 */
	ptr = strstr(text, "CITY");
	if ( ptr == NULL ) {
		plotdata->plt_mark.nstn = 0;
		return;
	}

/*
 * find the begining of the data
 */
	ptr2 = strstr(ptr, "\n");
	ptr  = ptr2 + 1;

/*
 * make sure the last char is a '\n'
 */
        len = (int)strlen(ptr);
        if ( ptr[len-1] != '\n' ) {
                ptr[len] = '\n';
                ptr[len +1] = '\0';
        }

/* 
 * process data line by line
 */
	count = 0;
	while (*ptr) {

/*
 * get a line
 */ 
	    line = &tmp[0];
	    while ( *ptr != '\n' ) {
		*line = *ptr;
		ptr++;
		line++;
	    }
	    *line = '\0';
	    ptr++; /* points to next line */
	
/*
 * process the line
 */
	    if (tmp[0] != ' ') {
		
/*
 * if city name consists of separate strings,
 * use '_' to put them together
 */
		for ( j=2; j < (int)strlen(tmp); j++ ) {
		    if ( (tmp[j-1] != ' ') && (tmp[j] == ' ') 
		 	      && (tmp[j+1] != ' ') ) 
		    tmp[j] = '_';
		}

	        sscanf( tmp, "%s %s %s %s %s %s", table[count].city,
		              table[count].state, table[count].uvi,
			      table[count+1].city, table[count+1].state,
		   	      table[count+1].uvi ); 

		count += 2;
	    }
	}

/*
 * Determine data values to plot. 
 */
	x=0;
	for ( i = 0; i < count; i++) {

		city_len  = strlen( table[i].city );
		state_len = strlen( table[i].state );

		for ( j = 0; j < stnlist->nstn; j++) {

			if ( strncmp(stnlist->stnName[j], 
				table[i].city, city_len) == 0 &&
			     strncmp(stnlist->stateId[j],  
				table[i].state, state_len) == 0 ) {

	                	plotdata->plt_mark.lat[x] = stnlist->lat[j];
	            	        plotdata->plt_mark.lon[x] = stnlist->lon[j];
		    		if ( dchk_digit(table[i].uvi) == 0 ) 
		       		    plotdata->plt_mark.dvalues[x] = 
						atoi(table[i].uvi);
		    		else
	               		    plotdata->plt_mark.dvalues[x] = IMISSD; 
	            		x++;
		    		break;
			}
			
		}
	}
	plotdata->plt_mark.nstn = x;
}
