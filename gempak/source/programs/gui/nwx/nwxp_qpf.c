#include "nwx_cmn.h"

#define WHITE " \t"
 

/************************************************************************
 * qpf.c                                                                *
 *                                                                      *
 * This module for decoding QPF DISC data.                              *
 *                                                                      *
 * CONTENTS:                                                            *
 *      qpf_decode()   decode QPF DISC data into plotting structure.    *
 ***********************************************************************/

/*=====================================================================*/

void qpf_decode ( char *text, plotdata_t *plotdata )
/************************************************************************
 * qpf_decode								*
 *									*
 * This routine decodes the QPF DISC report text and sets the contours	*
 * plotting data.							*
 *									*
 * qpf_decode( text, plotdata )						*
 *									*
 * Input parameters:							*
 *	*text		char		Text report 			*
 *									*
 * Output parameters:                                                   *
 *	*plotdata	plotdata_t	Struct of plotting data		*
 *									*
 * Return paramters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	01/95						*
 * C. Lin/EAI		 9/95					        *
 * D. Plummer/NCEP	12/95	Added logic to check for max number	*
 *				of points				*
 * M. Li/SAIC		10/02	Added a new color for qpf		*
 ***********************************************************************/
{
char		*ptr, *ptr2, *str;
char		lat[4], lon[4];
char		*line, tmp[80];
int		npts, i, jj, nn;
int		nlines;
int             colrs[3], chgclr, ncolr[4];

int             len, nl; 
struct contour  *contours;

/*---------------------------------------------------------------------*/

	contours = &(plotdata->plt_cnt);
        
	chgclr   = -1;
	colrs[0] = 2;
	colrs[1] = 3;
	colrs[2] = 5;
	for ( jj = 0; jj < 4; jj++ ) {
	    ncolr[jj] = 0;
	}

	nlines = 0;

	
/*
 * make sure the last char is a '\n'
 */
	len = (int)strlen(text);
	if ( text[len-1] != '\n' ) {
		text[len] = '\n';
		text[len +1] = '\0';
	}

/*
 * Locate data and store into buffer
 */
	ptr = strstr(text, "24HR QP");
	if ( !ptr ) {
		contours->nc = 0;
		return;
	}

/*
 * find the beginning of data
 */
	ptr2 = strstr(ptr, "\n");
	ptr = ptr2 +1; 

/*
 *  Process data line by line
 */
	npts = 0;
	nn = 0;
	while ( *ptr ) {

	    if ( nlines == MAX_CONTOURS ) {
		printf("Number of contour lines exceeds maximum (%d)\n", MAX_CONTOURS );
		break;
	    }
/*
 * get a line
 */
	    line = &tmp[0];
	    while ( *ptr != '\n') {
		*line = *ptr;
	    	ptr++;
		line ++;
	    }
	    *line = '\0'; /* end of line */ 
	    ptr++; /* move to next line */

/*
 * process the line
 */
	    str = strtok(tmp, WHITE);
	    if ( !str ) continue;

/*
 *  Check for the number of data sets
 */
	    if ( strcmp (str, "24HR") == 0 ) {
   		chgclr = nlines - 1;
		nn++;
		ncolr[nn] = nlines;
	    }
	    else {
/*
 *  Check for invalid data
 */
		    if ( dchk_digit(str) == 0 ) {
/* 
 * process each data string 
 */
		        while ( str ) {
/*
 *  Distinguish between contour and latlon
 */
		            if ( atol(str) <= 10 ) {
				if ( nlines < MAX_CONTOURS ) {
			        	nlines++;
					nl = nlines - 1;
		                	strcpy(contours->line[nl].label,str);
			        	npts=0;
				}
				else {
					break;
				}
		    	    }
		            else {
				   if ( npts < MAX_PTS ) {
			                   strncpy(lat, str, 3);
					   lat[3] = '\0';
			                   strncpy(lon, &(str[3]), 3);
					   lon[3] = '\0';
				           contours->line[nl].lat[npts] = (float)atof(lat);
					   contours->line[nl].lon[npts] = (float)atof(lon);
       	                                   contours->line[nl].lat[npts]/= 10.0F;
       	                                   contours->line[nl].lon[npts]/= 10.0F;
       	                                   if ( contours->line[nl].lon[npts] < 50.0F )
       	                                      contours->line[nl].lon[npts]+= 100.0F;
       	                                   contours->line[nl].lon[npts] *= -1.0F;
			           	   npts++;
				           contours->line[nl].npt = npts; 
				   }
				   else {
					   if ( npts == MAX_PTS )
					   	printf("Number of points for contour line %s exceeds maximum (%d)\n", 
							contours->line[nl].label, MAX_PTS );
					   npts++;
				   }
			    }
		            str = strtok (NULL, WHITE);

		        } /* end of while(str) */

		    } /* end of dchk_alpha() */

	    }/* end of else */

	} /* end of while(*ptr) */

/*
 * assign color for each contour line
 */
	if ( nlines > 0 ) {
            ncolr[nn+1] = nlines;
        }

	if ( chgclr != -1 ) {
	    contours->line[0].color = colrs[0];
	    for ( i = 1; i < nlines; i++ ) {
		for ( jj = 0; jj <= nn; jj++ ) {
		    if ( i > ncolr[jj] && i <= ncolr[jj+1] ) {
		        contours->line[i].color = colrs[jj];
		    }
		}
	    }
	}
	else
	    for ( i = 0; i < nlines; i++)
		contours->line[i].color = colrs[0];

	contours->nc = nlines;
} 
