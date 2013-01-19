#include "nwx_cmn.h"
/*#include "gui.h" */

struct reportSTD	*_rstd;
struct report_infoSTD {
    char                *text;  /* holds contents of file */
    struct reportSTD    repstd[MAX_REPORTS];
    int                 index;          /* number of reports */
    int                 lastfl;         /* last file indicator */
    int                 lastrpt;        /* last report indicator */
    int                 next;           /* pointer to next STD report */
    char                cache[36];
} _rep;

void _get_prvnxtrep ( int nextfl, char *report, int *iret );
void _scan_file ( srchinfo_t *srchinfo, char *report, int *iret );
void srchb_fosdMarkrep ( char *text, srchinfo_t *srchinfo, char *report,
			 int *iret );

/************************************************************************
 * srchb.c                                                              *
 *                                                                      *
 * This module processes FOS (Family Of Services) data for type B, S,	*
 * R, M, F, and Z.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      srchb_fosdGetrep()    finds the report 				*
 *				for the requested location.   		*
 *      srchb_fosdMarkrep()    finds the beginning and ending		*
 *				position of the report.     		*
 *	srchb_fosdGetnxt()    finds the next report			*
 *				for the requested location.		*
 *	srchb_repInit()       initialize the report_infoSTD structure	*
 *	_get_prvnxtrep()      retrieves the previous/next report.	*
 * 	_scan_file()	      scans the current file for the next	*
 *				report					*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void srchb_fosdGetrep ( struct datatype_list *dtinfo, 
			srchinfo_t *srchinfo, char *report, int *iret )
/************************************************************************
 * srchb_fosdGetrep							*
 *									*
 * This routine will find the first report for the requested location.	*
 *									*
 * srchb_fosdGetrep ( dtinfo, srchinfo, report, iret )  		*
 *									*
 * Input parameters:							*
 *	*dtinfo	        struct datatype_list	Data type structure	*
 *	*srchinfo	srchinfo_t		Search info structure	*
 *									*
 * Output parameters:							*
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *				 0 -- successful			*
 *				-1 -- no data				*
 *				-2 -- MAX_REPORTS reached		*
 *				-6 -- last file in fore/backward	*
 *				-8 -- scandir failure                   *
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * L. Williams/EAI       7/95		clean up			*
 * L. Williams/EAI	 8/95		Add station ID to text message, *
 *					call get_date and disable	*
 *					prev/next buttons when no data  *
 *					is available.			*
 * C. Lin/EAI		 9/95           rewrite based on nwx2.1         *
 * D.Plummer/NCEP	12/95		reorganize for fosd type W	*
 * G. Krueger/EAI	 3/96		CFL_SOPN -> CFL_ROPN		*
 * L. Williams/EAI	 5/96		modify for new search method	*
 * L. Williams/EAI	 7/96		indicate last flag for "S" data *
 *					type				*
 * S. Jacobs/NCEP	12/98		Changed fclose to cfl_clos	*
 * S. Danz/AWC		12/00		Updated call to dttm_cpy	*
 * S. Danz/AWC		12/00		Remove NULL chars from report	*
 * R. Tian/SAIC		04/02		Query file size before open it	*
 * S. Jacobs/NCEP	 5/02		Added check for report found	*
 * T. Piper/SAIC	 5/02	Fixed date/time display			*
 * T. Piper/SAIC	01/04	Added check for -8 from dir_getflist	*
 ***********************************************************************/
{
int		found;
int		ii, ier;
char		last_flg;
long		flen;	
char		dumyname[133];

static struct date_time_info _startd_save;
static struct date_time_info _endd_save;


/*---------------------------------------------------------------------*/
	last_flg = 0;

/*
 * If the search is starting over, initializing 
 * directory contents and search string.
 */
	if  ( srchinfo->sflag == 0 ) {
/*
 * Set the search text markers.
 */
 	    sstruct_stxtmk( srchinfo->idtyp, dtinfo, iret );

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

	        dir_getflist( dtinfo, srchinfo->idtyp, srchinfo->startd,
			srchinfo->endd, &srchinfo->dir_info, iret );
	        if  ( *iret == -8 ) {
		    return;
		}
		else if ( *iret != 0 ) {
/*
 * no matching data file
 */
		    report[0] = '\0';
		    *iret = -1;
		    return;
	        }
	    }
	}
/*
 * Loop until the report is found.
 */
	found = G_FALSE;
	while ( ! found ) { 

/*
 * Get the last data file.
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
            	if  ( srchinfo->file_info.fptr != NULL )  {
            	    cfl_clos ( srchinfo->file_info.fptr, &ier );
		    srchinfo->file_info.fptr = NULL;
		}

/*
 * Get the size of the new file. If the size is 0, skip it.
 */
		cfl_inqr(srchinfo->file_info.filnam, NULL, &flen, dumyname, 
			 iret);
            	if  ( *iret != 0 || flen == 0 ) {
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
		   report[0] = '\0';
		   *iret = -9;
		   return; /* error in opening file */
		}

/*
 * Allocate enough space to hold the entire contents of the file.
 */
                if (_rep.text) {
	            _rep.text = (char *)realloc(_rep.text, ((size_t)srchinfo->file_info.file_len + 1)
				*sizeof(char) );
                } else {
	            _rep.text = (char *)malloc((size_t)(srchinfo->file_info.file_len + 1)
				*sizeof(char) );
                }

/*
 * Read the contents of the file into the TEXT string.
 */
	        if ( fread( _rep.text, (size_t)srchinfo->file_info.file_len,
				1, srchinfo->file_info.fptr) != (size_t)1 ){
/*
 * reading error
 */
		    report[0] = '\0';
		    *iret = -1;
		    return;
	        }
                _rep.text[srchinfo->file_info.file_len] = '\0';

                for (ii=0; ii<srchinfo->file_info.file_len; ii++) {
                    if (!_rep.text[ii]) _rep.text[ii] = ' ';
                }

/*
 * Search the TEXT for the correct report.
 */
		srchb_fosdMarkrep( _rep.text, srchinfo, report, iret);
	        if  ( *iret == 0  ||  *iret == 1 || *iret == 2 ) {
 		    found = G_TRUE;
		    if ( *iret == 2 )
		       *iret = -6; /* maximum limit reached */
		}
/*		if ( last_flg )
			break; */
	    }
	    else
		break;


	}

/*
 * If the data has still not been found, return with an error.
 */
	if  ( ! found )  {
	    report[0] = '\0';
	    *iret = -1;
	    return;
	}

	if  ( report[0] ) {

/*
 * Remove the unprintable characters.
 */
	    cst_unpr( report, report, iret );

/*
 * Display report date in the text window.
 */
 	    if ( strncmp(dtinfo[srchinfo->idtyp].datatyp,"WATCH_BOX", 9) != 0 ) {
		txtw_dttmSet(srchinfo->dir_info.filnam[srchinfo->dir_info.cfilnum]); 
		txtw_dsplyDateTime( &ier );
	    }
	}
 
/*
 * Close the data file.
 */
	if  ( srchinfo->file_info.fptr != NULL )  {
		cfl_clos ( srchinfo->file_info.fptr, &ier );
		srchinfo->file_info.fptr = NULL;
	}

/*
 * Check for last file in forward/backward
 */
	if ( last_flg ) {
	   _rep.lastfl = 1;
/*
 * if "S" data type is indicated set return code
 */
	   if( nwxTable->dtyp_info [srchinfo->idtyp].bsflag[0] == 'S' ) {
		*iret = -6;
	   }
	}
}

/*=====================================================================*/

void srchb_fosdMarkrep ( char *text, srchinfo_t *srchinfo, 
						char *report, int *iret )
/************************************************************************
 * srchb_fosdMarkrep							*
 *									*
 * This routine will find the specified report in the given text. It	*
 * will search for the string, then find the beginning and ending	*
 * positions for the report and save the report information in the	*
 * structure.								*
 *									*
 * srchb_fosdMarkrep ( text, srchinfo, report, iret )			*
 *									*
 * Input parameters:                                                    *
 *	*text		char		bulk reports			*
 *	*srchinfo	srchinfo_t	Search info criteria		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *					    = 0  NORMAL			*
 *					    = 1  found, but buffer full	*
 *					    = other, report not found	*
 *					    = 2  found, but maximum 	*
 *					      reports reached, report   *
 *					      not found			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI	         9/95	modified from nwx2.1			*
 * D.Plummer/NCEP	12/95	reorganize for fosd type W		*
 * D.W.Plummer           2/96   Add "M"-type data type for MOS  	*
 * L. Williams		 5/96	save report information			*
 * D. Keiser/GSC	10/96	Added changes from S. Chiswell		*
 * K. Tyle/GSC		 3/97	Add "F"-type data type for FFG		*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * D.W.Plummer/NCEP	 3/99	Force type "B" to search for ^A		*
 ***********************************************************************/
{
int	ibeg, iend, ipos;
int     ispos, iepos;
int     ispos1, iepos1, ispos2, iepos2;
int     iprev;
int     rep_len, rep_pos;
int	process;

struct data_file_info *file_info;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	process = G_TRUE;
	rep_pos = 0;

/*
 * Initialize the report text.
 */
	report[0] = CHNULL;

	file_info = &(srchinfo->file_info);

/*
 * Find the search string. Search backwards through the data.
 */
	ipos = file_info->file_len - 1;

	while ( process == G_TRUE ) {

		ibeg = ipos;
		if ( srchinfo->srchstr[0] != (char)NULL ) {
		    cst_srch( ibeg, 0, srchinfo->srchstr, text,
				&ipos, iret );
		}
		else
		    ipos = 0;

		switch( nwxTable->dtyp_info[srchinfo->idtyp].bsflag[0] ){

			case 'S' :
				if ( *iret != 0 && rep_pos == 0 )  
					return;
				else if ( *iret != 0 && rep_pos != 0 ) {
					*iret = 0;
					return;
				}
				break;

			default :
				if  ( *iret != 0 )  return;
				break;
		}

/*
 * Find the start of text marker(s).  (Backward search)
 */

		switch( nwxTable->dtyp_info[srchinfo->idtyp].bsflag[0] ){
			case 'B' :
			case 'R' :
			case 'F' :
				iprev = 0;
				break;
			default :
				iprev = ipos - 30;
				break;
		}

		if  ( srchinfo->start_of_text[0][0] != CHNULL ) {
		    cst_srch( ipos, iprev, srchinfo->start_of_text[0],
			       text, &ispos1, iret );
		    if  ( srchinfo->start_of_text[1][0] != CHNULL ) {
			cst_srch(ipos, iprev, srchinfo->start_of_text[1],
				   text, &ispos2, iret );
		    }
		    else {
			ispos2 = ispos1;
		    }
		    if  ( *iret != 0 )  return;

/*
 * Set the start of text position to be the maximum of ispos1 and ispos2.
 */
		    ispos = G_MAX( ispos1, ispos2 );
		    if (ispos < 0) ispos = ipos; 
		}
		else {
/*
 * Set the start of text position to be the string position.
 */
		    	ispos = ipos;
		}

/*
 * Find the end of text marker(s).  (Foreward search)
 */
		switch( nwxTable->dtyp_info[srchinfo->idtyp].bsflag[0] ){
			case 'M' :
				ipos++;
				break;
			default :
				break;
		}
		ibeg = ipos;
		iend = file_info->file_len;
		if  ( srchinfo->end_of_text[0][0] != CHNULL ) {
			cst_srch( ibeg, iend, srchinfo->end_of_text[0],
			       text, &iepos1, iret );
		    	if  ( srchinfo->end_of_text[1][0] != CHNULL ) {
			   cst_srch(ibeg, iend, srchinfo->end_of_text[1],
					   text, &iepos2, iret );
		    	}
			else {
				iepos2 = iepos1;
		    	}
		    	if  ( *iret != 0 )  return;

/*
 * Set the end of text position to be the minimum of iepos1 and iepos2.
 */
       		     	if  ( iepos1 < 0 ) {
				iepos = iepos2;
       		     	}
       		     	else if  ( iepos2 < 0 ) {
       		        	iepos = iepos1;
       		     	}
		     	else {
		    		iepos = G_MIN( iepos1, iepos2 );
			}
			 if ( iepos <= ispos )
			iepos = ipos + (int)strlen( srchinfo->srchstr ) + 1;
		}
		else {
/*
 * Set the end of text position to be the end of the string.
 */
	    		iepos = ipos + (int)strlen( srchinfo->srchstr ) + 1;
		}

/*
 * Copy the report from the entire text.
 * If the report is too big, only display part of the report.
 */

		switch( nwxTable->dtyp_info[srchinfo->idtyp].bsflag[0] ){

			case 'S' :
				rep_len = G_MIN( iepos-ispos,REPMAX-1-rep_pos );
				if ( rep_len + rep_pos == REPMAX - 1 )
					*iret = 1;
				memmove( &report[rep_pos], &text[ispos], (size_t)rep_len );
				rep_pos = rep_pos + rep_len;
				rep_len = rep_pos;
				break;

			default :
				rep_len = G_MIN(iepos-ispos, REPMAX-1);
				if  ( rep_len == REPMAX - 1 ) 
					*iret = 1;
				memmove( report, &text[ispos], (size_t)rep_len );
				process = G_FALSE;
				break;
		}

		if ( report[rep_len-1] != '\n' ) {
			report[rep_len]   = '\n';
			report[rep_len+1] = CHNULL;
		}
		else
			report[rep_len] = CHNULL;

	}

	(_rep.index)++;

/*
 * check if report limit has been reached
 */
	if( _rep.index == MAX_REPORTS ) {
	   (_rep.index)--;
	   *iret = 2;
	   report[0] = '\0';
           printf( "Report limit reached (%d).\n", MAX_REPORTS );
	   return;
	}

/*
 * Save file information
 */
	_rep.next = _rep.index;
	_rstd[_rep.index].position = ispos;
	strcpy( _rstd[_rep.index].fname,  srchinfo->file_info.filnam );
	_rstd[_rep.index].length = rep_len;

}

/*=====================================================================*/

void srchb_fosdGetnxt ( srchinfo_t *srchinfo, char *report, int *iret )
/************************************************************************
 * srchb_fosdGetnxt							*
 *									*
 * This routine will search the current file for the next report.  If	*
 * the file has been searched, it will return the report based on the	*
 * saved information in the structure.					*
 *									*
 * srchb_fosdGetnxt ( srchinfo, report, iret )  			*
 *									*
 * Input parameters:							*
 *	*srchinfo	srchinfo_t	Report info 			*
 *									*
 * Output parameters:							*
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *				 	 0  = report found		*
 *			        	-1  = report not found		*
 *					-2  = last file scanned		*
 *					-9  = file open/read error	*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI       5/96						*
 * T. Piper/SAIC	 5/02	Fixed date/time display			*
 * T. Piper/SAIC	01/04	Changed error return code from -3 to -9	*
 ***********************************************************************/
{
int	ier;
char	dirnam[LLPATH], filnam[MXFLSZ];
/*---------------------------------------------------------------------*/

	ier = 0;
	*iret = 0;
	report[0] = '\0';


	switch( srchinfo->sflag ) {

	   case 1:	/* next */

/*
 * set pointer to the next report
 */
		_rep.next--;
		if( _rep.next < 0 ) {
		   _rep.next = 0;
		   *iret = -2;
		   return;
		}

/*
 * last report in forward search
 */
		if( _rep.next == 0 )
		   *iret = -2;

/*
 * get next report
 */
		_get_prvnxtrep( _rep.next, report, &ier );
		if( ier != 0 ) {
		   *iret = -9;
		   return;
		}
		break;

	   case -1:	/* previous */

/*
 * check for previous selection
 */
		if( ( _rep.next == _rep.index ) && ( _rep.lastrpt == 0 ) ) {
/*
 * no previous selection - scan current file skipping duplicates.
 */ 
		   report[0] = '\0';
		   _scan_file( srchinfo, report, iret );
		   if( *iret != 0 )
		      return;
		}
		else {
/*
 * previous selection indicated
 */
		   _rep.next++; /* set pointer to the previous report */
		   if( _rep.next > _rep.index ) {
		      _rep.next = _rep.index;
		      *iret = -2;
		      return;
		   }

/*
 * last report in backward search
 */
		   if( _rep.next == _rep.index ) /* last report */
			*iret = -2;

/*
 * get previous report
 */
		   _get_prvnxtrep( _rep.next, report, &ier );
		   if( ier != 0 ) {
		      *iret = -9;
		      return;
		   }
	        }
		break;

	}

	if( report[0] ) {

/*
 * Remove the unprintable characters.
 */
	   cst_unpr( report, report, &ier );

/*
 * Display report date in the text window.
 */
	    if ( strncmp(nwxTable->dtyp_info[srchInfo.idtyp].datatyp, "WATCH_BOX",9) != 0 ) {
	       cfl_path(_rstd[_rep.next].fname, dirnam, filnam, &ier);
	       txtw_dttmSet(filnam); 
	       txtw_dsplyDateTime( &ier );
	    }
	}
}

/*=====================================================================*/

void srchb_repInit ( void )
/************************************************************************
 * srchb_repInit							*
 *									*
 * This routine will initialize the structure report_infoSTD		*
 *									*
 * srchb_repInit()							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	 6/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

/*
 * Initialize structure
 */
	_rstd = &(_rep.repstd[0]);
	_rep.index    = -1;
	_rep.next     = 0;
	_rep.lastfl   = 0;
	_rep.lastrpt  = 0;
	_rep.cache[0] = '\0';

}

/*=====================================================================*/

void _scan_file ( srchinfo_t *srchinfo, char *report, int *iret )
/************************************************************************
 * _scan_file								*
 *									*
 * This routine will scan the current file for the previous report.	*
 *									*
 * void _scan_file( srchinfo, report, iret )				*
 *									*
 * Input parameters:							*
 *	*srchinfo	srchinfo_t	Info found in file		*
 *									*
 * Output parameters:							*
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *				 	  0 = report found 		*
 *			        	 -1 = report not found 		*
 *					 -2 = last file 		*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI       5/96						*
 * S. Jacobs/NCEP	12/98	Removed setting string=NULL after free	*
 * G. Krueger/EAI	 3/99	Check duplicates after bulletin ID num.	*
 ***********************************************************************/
{
char	*text;
int	rpos, ier;
int	dupfnd, new_pos;

/*---------------------------------------------------------------------*/

	ier=0;
	*iret = 0;
	dupfnd = 1;
	rpos = _rstd[_rep.index].position;

	while( dupfnd ) {
/*
 * check for invalid position
 */
	   if( rpos >= 0 ) {

	      strncpy( _rep.cache, &(_rep.text[rpos])+8, 35 );
	      _rep.cache[35] = '\0';

/*
 * allocate enough space for the report
 */
	       text = (char *)malloc((size_t)(rpos+1) * sizeof(char));
	       memcpy( text, _rep.text, (size_t)rpos );
	       text[rpos] = '\0';

/*
 * search for the next report in the current file
 */
              srchb_fosdMarkrep( text, srchinfo, report, &ier);
              free( text );

/*
 * report found
 */
              if( ier == 0  ||  ier == 1 ) {
	         new_pos = _rstd[_rep.index].position;

/*
 * check for duplicate reports
 */
	         if( strncmp( _rep.cache, &(_rep.text[new_pos])+8, 35) == 0 ) {
		    rpos = new_pos;
	            _rep.index--;
	         }
	         else {
		    dupfnd = 0;
	         }
   	      }
	      else {
		 dupfnd = 0;
/*
 * report not found
 */
		 _rep.cache[0] = '\0';
	         if( ( _rep.lastfl == 1 ) || ( ier == 2 ) ) {
		     *iret = -2; /* last file indicated or max limit reached */
		     _rep.lastrpt = 1;
	         }
	         else {
        	    *iret = -1;
	         }
                 free( _rep.text );
                 _rep.text = 0;
              }
	   }
	   else
	      *iret = -1;

	} /*while */
}

/*=====================================================================*/

void _get_prvnxtrep ( int nextfl, char *report, int *iret )
/************************************************************************
 * _get_prvnxtrep							*
 *									*
 * This routine will retrieve the previous/next report.			*
 *									*
 * void _get_prvnxtrep ( nextfl, report, iret ) 			*
 *									*
 * Input parameters:							*
 *	nextfl		int		Pointer to the next file	*
 *									*
 * Output parameters:							*
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *				 	  0 = successful 		*
 *					 -2 = reading error		*
 *			        	 -9 = open file error		*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI       5/96						*
 * S. Jacobs/NCEP	12/98	Changed fclose to cfl_clos		*
 * S. Jacobs/NCEP	12/98	Removed setting string=NULL after free	*
 * R. Tian/SAIC		04/02	Query file size before open it		*
 * T. Piper/SAIC	01/04	Changed error return from -1 to -9	*
 ***********************************************************************/
{
int			ier, pos;
char			*text;
long			flen;	
char			dumyname[133];
FILE			*fp;

/*---------------------------------------------------------------------*/
/*
 * Get the size of the file. If the size is 0, skip it.
 */
	cfl_inqr( _rstd[nextfl].fname, NULL, &flen, dumyname, iret );
        if  ( *iret != 0 || flen == 0 ) {
	    *iret = -9;      
	    return; 
	}

/*
 * open new file.
 */
	fp = cfl_ropn( _rstd[nextfl].fname, NULL, iret );
	if( *iret != 0 ) {
	   *iret = -9; 	/* error in opening file */
	   return;
	}

/*
 * allocate enough space to hold the entire contens
 * of the file.
 */
	text = (char *)malloc((size_t)(flen+1) * sizeof(char));

/*
 * read the contents of the file into the TEXT string
 */
	if( fread( text, (size_t)flen, 1, fp ) != (size_t)1 ) {
	   free( text );
	   *iret = -2; 	/* reading error */
	   return;
	}

/*
 * Close the file.
 */
	if  ( fp != NULL )  {
	    cfl_clos ( fp, &ier );
	    fp = NULL;
	}

/*
 * retrieve report
 */
	pos = _rstd[nextfl].position;
	memcpy( report, &(text[pos]), (size_t)_rstd[nextfl].length );
	report[_rstd[nextfl].length] = '\0';

/*
 * free text
 */
	free( text );

}
