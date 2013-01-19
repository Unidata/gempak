#include "nwx_cmn.h"


/************************************************************************
 * nwxp_wbox.c								*
 *									*
 * This module decodes the watch box report text.			*
 *									*
 * CONTENTS:								*
 *	wbox_decode()	decode the watch box report			*
 ***********************************************************************/


void wbox_decode ( char *text, int nwatch, plotdata_t *plotdata )
/************************************************************************
 * wbox_decode								*
 *									*
 * This routine decodes the WATCH BOX report text and sets the watch  	*
 * box plotting data.							*
 *									*
 * wbox_decode( text, nwatch, plotdata )				*
 *									*
 * Input parameters:							*
 *	*text		char		Text report 			*
 *	nwatch		int		Number of watches		*
 *									*
 * Output parameters:                                                   *
 *	*plotdata	plotdata_t	Struct of plotting data		*
 *									*
 * Return parameters:							*
 *									*
 **									*
 * Log:									*
 * D. Kidwell/NCEP	 8/98	Adapted from qpf_decode			*
 * S. Jacobs/NCEP	 4/99	Check for missing itype after decode	*
 * D. Kidwell/NCEP	 4/99	Changed processing of watch cancel;     *
 *                              changed call for underscore             *
 * D. Kidwell/NCEP	 6/99	Changed color for thunderstorm watchbox *
 * D. Kidwell/NCEP	 7/99	Treat watch replacements as cancel      *
 * T. Piper/SAIC	12/01	freed rwnum				* 
 * T. Piper/SAIC	09/07	Corrected call to ww_dcod		*
 ***********************************************************************/
{
float		rlat[MAX_WPTS], rlon[MAX_WPTS];
char		wnum[5], strtim[20], stptim[20], tissue[20];
char		rnums[40];
char		**rwnum;
int		itype, icorr, icancl, npt, irepl, iret;
int		i, ii, j, npts;
int             colrs[2];

int             len; 
struct watchbox  *wtchbox;

/*---------------------------------------------------------------------*/

	if ( (nwatch+1) >= MAX_WATCHES ) {
	    printf("Number of watch boxes exceeds maximum (%d)\n",
		    MAX_WATCHES );
	    return;
	}

	wtchbox = &(plotdata->plt_wbox);

	colrs[0] = 2;
	colrs[1] = 6;

/*
 * make sure the last char is a '\n'
 */
	len = (int)strlen(text)+1;
	if ( text[len-1] != '\n' ) {
		text[len] = '\n';
		text[len +1] = '\0';
		len = len +1;
	}
	strcpy ( wnum, "0000" );
	wnum[4] = '\0';
	strcpy ( strtim, "yymmdd/hhmm" );
	strtim[11] = '\0';
	strcpy ( stptim, "yymmdd/hhmm" );
	stptim[11] = '\0';
	strcpy ( tissue, "yymmdd/hhmm" );
	tissue[11] = '\0';
	strcpy ( rnums, "                                       " );
	rnums[39] = '\0';

/*
 * Decode the watch box data
 */
	ww_dcod  ( text, &len, &itype, wnum, strtim, stptim, tissue,
                   &icorr, &icancl, rlat, rlon, &npt, &irepl, 
		   rnums, &iret, len, strlen(wnum), strlen(strtim),
                   strlen(stptim), strlen(tissue), strlen(rnums) );
 
	if ( iret != 0 || itype == IMISSD ) {
	    wtchbox->nb = nwatch;
	    return;
	}

/*
 * Check cancel flag
 */
	if  ( icancl == 1 )  {
	    for ( i = 0; i < nwatch; i++ )  {
		if  ( strcmp ( wtchbox->winfo[i].wtchnum, wnum ) == 0 ) {
	            strcpy(wtchbox->winfo[i].expire,tissue);
		}
	    }
	}
	else {

/*
 * assign watch number
 */
	    strcpy(wtchbox->winfo[nwatch].wtchnum,wnum);

/*
 * assign watch valid GEMPAK dattim
 */
	    strcpy(wtchbox->winfo[nwatch].valid,strtim);

/*
 * assign watch expires GEMPAK dattim
 */
	    strcpy(wtchbox->winfo[nwatch].expire,stptim);

/*
 * assign number of watch area vertices
 */
	    wtchbox->winfo[nwatch].npt = npt; 

/*
 * assign color for watch box
 */
	    wtchbox->winfo[nwatch].color = colrs[itype];

	    npts = 0;
	    while ( npts < npt ) {

/*
 * store lat and long
 */
		if ( npts < MAX_WPTS ) {
		    wtchbox->winfo[nwatch].lat[npts] = rlat[npts];
		    wtchbox->winfo[nwatch].lon[npts] = rlon[npts];
		    npts++;
		}
		else {
		    if ( npts == MAX_WPTS )
			printf("Number of points for watch box %s exceeds maximum (%d)\n", 
			    wtchbox->winfo[nwatch].wtchnum, MAX_WPTS );
		     npts++;
		}

	    } /* end of while(npts<npt) */

/*
 * check for watches being replaced, and use new start time
 * as expiration time
 */
	    if ( irepl > 0 ) {
	        rwnum = (char **) malloc(sizeof(char *) * 10); 
 	        for ( i = 0; i < 10; i++ ) {
        	    rwnum[i] = (char *) malloc(5);
   	        }

	        cst_clst ( rnums, ',', " ", 10, 5, rwnum, &j, 
                           &iret );
	        for ( j = 0; j < irepl; j++ )  {
	            rwnum[j][3] = ' ';
		    rwnum[j][4] = '\0';
	      	    for ( i = 0; i < nwatch ; i ++ )  {
	                if ( strcmp ( wtchbox->winfo[i].wtchnum,
	      	                      rwnum[j] ) == 0 ) {
	      	        strcpy(wtchbox->winfo[i].expire,strtim);
	                }
	      	    }
	        }
		for ( ii = 0; ii < 10; ii++ ) {
                    free(rwnum[ii]);
                }
		free(rwnum);
	    }
	       
	    wtchbox->nb = nwatch + 1;
	}
} 
