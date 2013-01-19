#include "nwx_cmn.h"


#define DELMTR     "\n"
#define CACHE_POS  8

int repnum;		/* number of reports */

typedef struct {
        char            stnid[25];
        int             position;
        char            fname[133];
        int             stnindex;
} rptinfo_t;
rptinfo_t reportInfo[MAX_REPORTS]; 

char   _cache[31];

void _getrpt ( srchinfo_t *srchinfo, char *report, int *iret );
int srchw_binSrch ( char data[][SRCHSTR_LEN], int dlen, char *key );
void srchw_fosdMarkrep ( srchinfo_t *srch_info, char *text_rep,
			 char *srch_str, int called_from, int *iret );

/************************************************************************
 * srchw.c                                                              *
 *                                                                      *
 * This module processes FOS (Family Of Services) data for type W.      *
 *                                                                      *
 * CONTENTS:                                                            *
 *	srchw_fosdScan()	scan all reports 			*
 *				for the requested location for WATCHWARN*
 *				the report.     			*
 *	srchw_fosdMarkrep()	internal function used in srchw_fosdScan*
 *				to find	the search string and the 	*
 *				beginning position of each report.	*
 *      srchw_fosdGetrep()      finds the report for the requested	*
 *                              for the requested location		*
 *	srchw_binSrch()   	internal function used to for binary    *
 *				searching of the station table.   	*
 *      _getrpt()               retrieves the report.			*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void srchw_fosdScan ( srchinfo_t *srchinfo, int called_from, int *iret )
/************************************************************************
 * srchw_fosdScan							*
 *									*
 * This routine will scan a file given a search structure and return    *
 *    the appropriate report.                                           *
 *                                                                      *
 * srchw_fosdScan ( srchinfo, called_from, iret )  	               	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *srchinfo       srchinfo_t      Search info structure           *
 *      called_from     int     	Indicate from where it is called*
 *                                	  1 - from user click           *
 *                                	  2 - from auto-update          *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-8 -- scandir failure		*
 *					-9 -- open file error		*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI       10/95						*
 * D.Plummer/NCEP        12/95		Reorganize for fosd type W      *
 * G. Krueger/EAI	  3/96		CFL_SOPN -> CFL_ROPN		*
 * L. Williams/EAI	  6/96		Check for duplicate reports	*
 * S. Jacobs/NCEP	 12/98		Changed fclose to cfl_clos	*
 * S. Danz/AWC		 12/00		Updated call to dttm_cpy	*
 * S. Danz/AWC		 12/00		Remove NULL chars from report	*
 * R. Tian/SAIC         04/02           Query file size before open it  *
 * T. Piper/SAIC	05/02	Close last file opened before returning	*
 * R. Tian/SAIC		11/03		Added called_from arg		*
 * T. Piper/SAIC	01/04	Added check for -8 from dir_getflist	*
 * T. Piper/SAIC	01/04	Added check on cfl_ropn			*
 * E. Safford/SAIC	12/07	rm direct access to textW widget	*
 ***********************************************************************/
{
char	*text, last_flg;
char	search_str[2];

static struct date_time_info _startd_save;
static struct date_time_info _endd_save;

int		i, j, duplicate, ier;
char            nofile_msg[80];
long            flen;
char            dumyname[133];

/* ------------------------------------------------------------------- */

	repnum=0;

/*
 * Scan the data directory for a list of valid files.
 */
        if ( (srchinfo->idtyp != _idtyp_save) ||
                (dttm_cmp(srchinfo->startd, _startd_save) != 0) ||
                (dttm_cmp(srchinfo->endd, _endd_save) != 0) )
        {
                _idtyp_save = srchinfo->idtyp;
                dttm_cpy(&_startd_save, srchinfo->startd);
                dttm_cpy(&_endd_save,   srchinfo->endd);

                dir_getflist( nwxTable->dtyp_info, srchinfo->idtyp, 
			srchinfo->startd, srchinfo->endd,
                        &srchinfo->dir_info, iret );
		if ( *iret == -8 ) {
		    return;
		}
		else if  ( *iret != 0 ) {
/*
 * no matching data file
 */
                    *iret = -1;
		    sprintf( nofile_msg, "No reports.\n\n" );
		    if ( called_from == 1 ) {
			pdata_setReportText( nofile_msg, &ier );
			txtw_dsplyReport( &ier );
		    }
		    stnList.nrptstn = 0;
                    return;
                }
	}

	last_flg = 0;
	search_str[0] = CHCTLA;
	search_str[1] = '\0';

	while ( !last_flg ) {

/*
 * Get the next data file.
 */
            dir_getnextf( &(srchinfo->dir_info), srchinfo->sflag,
                        &(srchinfo->file_info), iret);

            if (srchinfo->sflag == 0)
                srchinfo->sflag = -1;

            if ( *iret == 3 )
                last_flg = 1;

            if ( (*iret == 0) || (*iret == 3) ) { /* new file */

/*
 * Close the open file.
 */
                if  ( srchinfo->file_info.fptr != NULL ) {
                    cfl_clos ( srchinfo->file_info.fptr, iret );
		    srchinfo->file_info.fptr = NULL;
		}

/*
 * Get the size of the new file. If the size is 0, skip it.
 */
                cfl_inqr(srchinfo->file_info.filnam, NULL, &flen, dumyname,
                         iret);
                if  ( *iret != 0 || flen == 0) {
                    if ( last_flg ) {
                        break;
                    } else {
                        continue;
                    }
                }
                srchinfo->file_info.file_len = (int)flen;

/*
 * Open the new file.
 */
                srchinfo->file_info.fptr = cfl_ropn(
                                srchinfo->file_info.filnam,
                                NULL, iret );
                if  ( *iret != 0 ) {
		    *iret = -9;
		    return;
		}

/*
 * Allocate enough space to hold the entire contents of the file.
 */
                text = (char *) malloc((size_t)(srchinfo->file_info.file_len + 1) *
                                        sizeof(char) );

/*
 * Read the contents of the file into the TEXT string.
 */
                if ( fread( text, (size_t)srchinfo->file_info.file_len, 1,
                                srchinfo->file_info.fptr) != (size_t)1 ){
/*
 * reading error
 */
                    free ( text );
                    if ( last_flg ) {
                        break;
                    } else {
                        continue;
                    }
                }
                text[srchinfo->file_info.file_len] = '\0';

                for (i=0; i<srchinfo->file_info.file_len; i++) {
                    if (!text[i]) text[i] = ' ';
                }

/*
 * Search the TEXT for the search string and report's position.
 */

                srchw_fosdMarkrep( srchinfo, text, search_str, called_from, iret);

/*
 * Free the allocated space
 */
                free ( text );

            }
            else
                break;

	}   /*  end while  */

/*
 * Close the last open file.
 */
	if  ( srchinfo->file_info.fptr != NULL ) {
	    cfl_clos ( srchinfo->file_info.fptr, iret );
	    srchinfo->file_info.fptr = NULL;
	}

	stnList.nrptstn = 0;
	for( i = 0 ; i < stnList.nreports; i++ )  {
	    duplicate = G_FALSE;
	    for( j=0; j < i; j++ ) {
	        if( reportInfo[i].stnindex == reportInfo[j].stnindex ) {
		   duplicate = G_TRUE;
		   break;
	        }
	    }
	    if( ! duplicate ) {
	       stnList.rptstn[stnList.nrptstn] = reportInfo[i].stnindex;
	       stnList.rptstnlat[stnList.nrptstn] = stnList.lat[reportInfo[i].stnindex];
	       stnList.rptstnlon[stnList.nrptstn] = stnList.lon[reportInfo[i].stnindex];
	       stnList.nrptstn++;
	    }
	}
}

/*=====================================================================*/

void srchw_fosdMarkrep ( srchinfo_t *srch_info, char *text_rep, 
			char *srch_str, int called_from, int *iret )
/************************************************************************
 * srchw_fosdMarkrep							*
 *									*
 * This routine will search for the search string of each report and	*
 * the beginning position and save that position in a structure.        *
 *									*
 * srchw_fosdMarkrep (srch_info, text_rep, srch_str, called_from, iret) *
 *									*
 * Input parameters:                                                    *
 *      *srch_info	srchinfo_t	Search structure 		*
 *      *text_rep	char		file contents			*
 *      *srch_str	char		search string 			*
 *      called_from     int     	Indicate from where it is called*
 *                                	  1 - from user click           *
 *                                	  2 - from auto-update          *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **									*
 * Log:									*
 * D.Plummer/NCEP	12/95		Reorganize for fosd type W      *
 * L.Williams/EAI	6/96		Check for duplicate reports	*
 * R.Tian/SAIC  	1/02		Avoid possible overflow buf	*
 * R. Tian/SAIC		11/03		Added called_from arg		*
 * E. Safford/SAIC	12/07	rm direct access to textW widget	*
 ***********************************************************************/
{

char	*str, t_str[15];
char	buf[REPMAX];
int	ibeg, ipos;
int	flag;
int	replen;
int	index, ier;
char	notfound_msg[80];
char	exceeds_msg[80];

struct data_file_info *file_info;
struct directory_info *dir_info;

/* ------------------------------------------------------------------- */

	ipos=0;
	flag=0;
	file_info = &(srch_info->file_info);
	dir_info  = &(srch_info->dir_info);

	ibeg = file_info->file_len - 1;

	_cache[0] = '\0';
        while ( ( ipos >= 0 ) && ( *iret == 0 ) ) {

/*
 *  Locate the starting position of each report
 */

		cst_srch( ibeg, 0, srch_str, text_rep,
			 &ipos, iret );	 
/*
 *  Copy report into a temporary buffer
 */

		replen = G_MIN((ibeg - ipos), (REPMAX - 1));
		strncpy(buf, &text_rep[ipos], (size_t)replen);
		buf[replen] = '\0';

/*
 *  Advance to the correct line
 */
		str = strtok(buf, DELMTR);
		if ( str != NULL ) {
			str = strtok(NULL, DELMTR);
			str = strtok(NULL, DELMTR);
		}

		if ( str ) {

		   str = strtok(str, " ");

/*
 *  Get the station and the WMO id 
 */
		   while ( str ) {
			if ( isalpha(str[0]) ) {

			    if ( flag == 0 ) {
				strcpy(t_str, str);
				flag = 1;
			    }
			    else {
				if ( repnum < MAX_REPORTS ) {

			           sprintf(reportInfo[repnum].stnid, " %s ", str);
				   index = srchw_binSrch( stnList.srchstr, stnList.nstn, 
					reportInfo[repnum].stnid );

				   if( index >= 0 ) {
                                       if( strncmp( _cache, &(text_rep[ipos+CACHE_POS]), 30 ) != 0 ) {

						reportInfo[repnum].stnindex = index;
				           	reportInfo[repnum].position = ipos;
				           	sprintf(reportInfo[repnum].fname, "%s/%s",
						    dir_info->dirpath, 
						    dir_info->filnam[srch_info->dir_info.cfilnum] );
	        		   		repnum++;
						cst_ncpy( _cache, &(text_rep[ipos+CACHE_POS]), 30, iret );
				       }
				   }
				   else {
                    			sprintf( notfound_msg, 
						"PRODUCT STATION \"%s\" not found in table.\n", 
						reportInfo[repnum].stnid );
					if ( called_from == 1 ) {
					    pdata_setReportText( notfound_msg, &ier );
					    txtw_dsplyReport( &ier );
					}
					reportInfo[repnum].stnid[0] = '\0';
				   }

				}	
				else {
				    sprintf( exceeds_msg, 
					     "Report limit reached (%d) -- "
					     "Reduce Time Covered.\n",
					     MAX_REPORTS );
				    if ( called_from == 1 ) {
					pdata_setReportText( exceeds_msg, &ier );
					txtw_dsplyReport( &ier );
				    }
				}
			    }
			}
			else {
			   flag = 0;
			   break;
			}
			str = strtok(NULL, " ");
		   }
		}
		ibeg = --ipos;
        }
	stnList.nreports = repnum;
}

/*=====================================================================*/

int srchw_binSrch ( char data[][SRCHSTR_LEN], int dlen, char *key )
/************************************************************************
 * srchw_binSrch   							*
 *									*
 * This routine will search for the search string of each report and	*
 * the beginning position and save that position in a structure.        *
 *									*
 * int srchw_binSrch ( data, dlen, key )                    		*
 *									*
 * Input parameters:                                                    *
 *      data[][SRCHSTR_LEN]	char	search array    		*
 *      dlen			int	length of array 		*
 *      *key			char	search string			*
 *                                                                      *
 * Output parameters:                                                   *
 *      srchw_binSrch		int	Return code			*
 **									*
 * Log:									*
 * D.Plummer/NCEP        12/95		New				*
 ***********************************************************************/
{
int	ibeg, iend, iptr, test, one = 1;

/* ------------------------------------------------------------------- */

	ibeg = 0;
	iend = dlen;
	iptr = (iend - ibeg) / 2;
	while ( one ) {
		test = strcmp( key, data[iptr]);
		if ( test == 0)  
			return ( iptr );
		if ( iptr == ibeg  ||  iptr == iend )  
			return ( -1 );
		if ( test < 0 ) {
			iend = iptr;
		}
		else if ( test > 0 ) {
			ibeg = iptr;
		}
		iptr = ibeg + (iend - ibeg) / 2;
	}
	return(-2);
}

/*=====================================================================*/

void srchw_fosdGetrep ( srchinfo_t *srchinfo, char *report, int *iret )
/************************************************************************
 * srchw_fosdGetrep                                                     *
 *                                                                      *
 * This routine will find the report for the requested location.        *
 *                                                                      *
 * srchw_fosdGetrep( srchinfo, report, iret )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *srchinfo       srchinfo_t      Search info structure           *
 *                                                                      *
 * Output parameters:                                                   *
 *      *report         char            Report                          *
 *      *iret           int             Return code                     *
 *                               0 -- successful                        *
 *                              -1 -- no data                           *
 *                              -6 -- last file in fore/backward        *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI       6/96           modified from srchb_fosdFndrepWW*
 * T. Piper/SAIC	 5/02	fixed date/time display			*
 ***********************************************************************/
{
int		ier, found, last_flg, tmp_ptr;
char		dirnam[LLPATH], filnam[MXFLSZ];
/*---------------------------------------------------------------------*/
	found = G_FALSE;
	last_flg = G_FALSE;

        tmp_ptr = srchinfo->current;

        switch( srchinfo->sflag ) {

              case 0:                   /* START OVER  */

                   srchinfo->current = 0;

                   while( srchinfo->current < stnList.nreports ) {

                      if( strcmp( reportInfo[srchinfo->current].stnid,
                                        srchinfo->srchstr ) == 0 ) {
                         strcpy( srchinfo->file_info.filnam,
                                        reportInfo[srchinfo->current].fname );
                         _getrpt( srchinfo, report, iret );
                         if( *iret != 0 ) {
                            report[0]='\0';
                            return;
                         }
                         found = G_TRUE;
                      }
                      if( found ) {
                         break;
                      }
                      (srchinfo->current)++;
                   }
                   if( srchinfo->current >= stnList.nreports - 1 ) {
                      last_flg = G_TRUE;
                   }
                   break;

              case 1:                   /* NEXT   */

                   (srchinfo->current)--;
                   while( srchinfo->current >= 0 ) {

                      if( strcmp( reportInfo[srchinfo->current].stnid,
                                        srchinfo->srchstr ) == 0 ) {
                         strcpy( srchinfo->file_info.filnam,
                                        reportInfo[srchinfo->current].fname );
                         _getrpt( srchinfo, report, iret );
                         if( *iret != 0 ) {
                            report[0]='\0';
                            return;
                         }
                            found = G_TRUE;
                      }
                      if( found ) {
                         break;
                      }
                      (srchinfo->current)--;
                   }
                   if( srchinfo->current <= 0 ) {
                      last_flg = G_TRUE;
                   }
                   break;

              case -1:                  /* PREVIOUS   */

                   (srchinfo->current)++;
                   while( srchinfo->current < stnList.nreports ) {

                      if( strcmp( reportInfo[srchinfo->current].stnid,
                                        srchinfo->srchstr ) == 0 ) {
                         strcpy( srchinfo->file_info.filnam,
                                        reportInfo[srchinfo->current].fname );
                         _getrpt( srchinfo, report, iret );
                         if( *iret != 0 ) {
                            report[0] = '\0';
                            return;
                         }
                            found = G_TRUE;
                      }
                      if( found ) {
                         break;
                      }
                      (srchinfo->current)++;
                   }
                   if( srchinfo->current >= stnList.nreports - 1 ) {
                      last_flg = G_TRUE;
                   }
                   break;

        }

        if( ! found ) {
           srchinfo->current = tmp_ptr;
           *iret = -6;
           return;
        }

/*
 * Remove the unprintable characters.
 */
        cst_unpr( report, report, iret );

/*
 * Display report date in the text window.
 */
	cfl_path(srchinfo->file_info.filnam, dirnam, filnam, &ier);
	txtw_dttmSet(filnam); 
	txtw_dsplyDateTime( &ier );

/*
x* set the return code if it is the last file in forward/backward
 */
        if ( last_flg )
                *iret = -6;
}

/*=====================================================================*/

void _getrpt ( srchinfo_t *srchinfo, char *report, int *iret )
/************************************************************************
 * _getrpt                                                            	*
 *                                                                      *
 * This routine will retrieve the report for the requested location.	*
 *									*
 * _getrpt ( srchinfo, report, iret )                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *srchinfo       srchinfo_t      Search info structure           *
 *                                                                      *
 * Output parameters:                                                   *
 *      *report         char            Report                          *
 *      *iret           int             Return code                     *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Williams/EAI       6/96	modified from srchb_fosdFndrepWW	*
 * S. Jacobs/NCEP	12/98	Changed fclose to cfl_clos		*
 * S. Chiswell/UCAR	04/01	Changed strchr to memchr		*
 * R.Tian/SAIC  	1/02	Avoid possible overflow report		*
 * R. Tian/SAIC         04/02   Query file size before open it 		*
 ***********************************************************************/
{
int     rep_len;
long    flen;
char    dumyname[133];
char	*iepos, *ispos, *text;
/*---------------------------------------------------------------------*/
/*
 * Get the size of the new file. If the size is 0, skip it.
 */
	cfl_inqr(srchinfo->file_info.filnam, NULL, &flen, dumyname, iret);
        if  ( *iret != 0 || flen == 0 ) {
               report[0] = '\0';
               *iret = -1;
               return; /* error in query file */
        }
        srchinfo->file_info.file_len = (int)flen;

/*
 * Open the new file.
 */
        srchinfo->file_info.fptr = cfl_ropn(
                srchinfo->file_info.filnam,
                NULL, iret );
        if  ( *iret != 0 ) {
               report[0] = '\0';
               *iret = -1;
               return; /* error in opening file */
        }

/*
 * Allocate enough space to hold the entire contents of the file.
 */
        text = (char *) malloc((size_t)(srchinfo->file_info.file_len + 1) *
                sizeof(char) );

/*
 * Read the contents of the file into the TEXT string.
 */
        if ( fread( text, (size_t)srchinfo->file_info.file_len, 1,
                srchinfo->file_info.fptr) != (size_t)1 ){
/*
 * reading error
 */
                report[0] = '\0';
                *iret = -1;
        	free( text );
                return;
        }

/*
 * Copy the report from string text to string report
 */

        ispos = &text[reportInfo[srchinfo->current].position];
        iepos = memchr( &text[reportInfo[srchinfo->current].position],
				CHCTLC, (size_t)(srchinfo->file_info.file_len - 
				reportInfo[srchinfo->current].position));
	if ( iepos != NULL )
	    rep_len = G_MIN((iepos - ispos), (REPMAX - 1));
	else
	    rep_len = 0;

        strncpy( report, &text[reportInfo[srchinfo->current].position], (size_t)rep_len
);
        report[rep_len] = '\0';

        free( text );

/*
 * Close the data file.
 */
        if  ( srchinfo->file_info.fptr != NULL )  {
		cfl_clos ( srchinfo->file_info.fptr, iret );
		srchinfo->file_info.fptr = NULL;
	}
}
