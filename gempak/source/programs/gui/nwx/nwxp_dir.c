#include "nwx_cmn.h"

/*
 * Macro to set the date/time based on the input and
 *	current date/time.
 */
#define SET_DTTM(dttm, curdttm) {\
	if  ( dttm.year  == 0 ) \
		dttm.year  = curdttm.year; \
	if  ( dttm.month == 0 ) \
		dttm.month = curdttm.month; \
	if  ( dttm.day   == 0 ) \
		dttm.day   = curdttm.day; \
	if  ( dttm.hour  > 24 ) \
		dttm.hour  = curdttm.hour; \
}

#ifdef IRIX
int _select_dir ( struct dirent *check );
#else
int _select_dir ( const struct dirent *check );
#endif


char    _sdttm[12], _edttm[12]; /* date/time constraints */
char	_exten[10];	        /* file extension */
char	_dtype[13];		/* data type */
int     _extlen;
size_t	_dttmlen;

/************************************************************************
 * nwxp_dir.c                                                           *
 *                                                                      *
 * This module scans the directory for desired data files.              *
 *                                                                      *
 * CONTENTS:                                                            *
 *      dir_getflist()  get the desired file lists in the directory.    *
 *      dir_getnextf()  get the latest file in the file list.    	*
 *      _select_dir()   internal function used in dir_getfiles to       *
 *			 select files with matched extention and time   *
 *			 constraints.    				*
 ***********************************************************************/

/*=====================================================================*/

void dir_getflist ( struct datatype_list *dtyp_info, int idtyp, 
			struct date_time_info startdttm, 
			struct date_time_info enddttm, 
			struct directory_info *dir_info, int *iret )
/************************************************************************
 * dir_getflist								*
 *									*
 * This routine will scan the data directory for the existing file	*
 * names. Only the files matching the requested file extension and      *
 * the time constraits will be returned. 				*
 * The file names will be sorted in alphabetical order, which based     *
 * on our yymmdd.ext data file naming convention, the latest appear     *
 * as the last file in the file list.					*
 *									*
 * dir_getflist ( dtyp_info, idtyp, startdttm, enddttm, dir_info, iret )*
 *									*
 * Input parameters:							*
 * *dtyp_info	struct datatype_list	Data type struct array		*
 * idtyp	int			Index into struct array		*
 * startdttm	struct date_time_info	Start date/time information	*
 * enddttm      struct date_time_info	End  date/time information	*
 *									*
 * Output parameters:							*
 * *dir_info	struct directory_info	Directory struct array		*
 * *iret		int		Return code			*
 *					-8 -- scandir failure		*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI	 	 8/95	rewrite for new nwx 			*
 * D.W.Plummer/NCEP	 9/96	added "O" data type processing		*
 * D.W.Plummer/NCEP	12/96	added check for NFILES limit		*
 * I. Durham/GSC	 5/98   changed call for underscore		*
 * S. Jacobs/NCEP	 4/99	Changed all years to 4 digits		*
 * A. Hardy/NCEP	 6/03   added tmzn to CSS_DATE			*
 * T. Piper/SAIC	01/04	changed iret to -8 for scandir failure	*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * T. Piper/SAIC	03/04	fixed logic on scandir status		*
 * M. Li/SAIC		01/08	_selectdir -> _select_dir		*
 * T. Piper/SAIC	02/08	Initialize namelist to NULL		*
 * S. Jacobs/NCEP	 4/13	Added checks for TAFS_DEC 		*
 * M. James/Unidata	10/09	changed _exten for "_" file exts	*	
 ***********************************************************************/
{
int		i, ier, isec, jday;
char		direxp[133], tmzn[4];

struct dirent	**namelist=NULL;

int                     itype;
struct date_time_info	curtim;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 * Get the current time.
 */
	itype = 1;
	css_date ( &itype, &curtim.year, &curtim.month, &curtim.day,
		   &curtim.hour, &curtim.minute, &isec, &jday, tmzn, &ier );

/*
 * set the starting time 
 */
	if ( startdttm.year == -1 )
		_sdttm[0] = '\0';
	else {
	    SET_DTTM(startdttm, curtim);
	    if ( dtyp_info[idtyp].bsflag[0] == 'O' ) {
		if ( strcmp ( dtyp_info[idtyp].datatyp, "TAFS_DEC" ) == 0 ) {
		  sprintf( _sdttm, "%04d%02d%02d%02d",
                      startdttm.year, startdttm.month,
                      startdttm.day,  startdttm.hour );
              }
              else {
                   sprintf( _sdttm, "%04d%02d%02d",
	      		startdttm.year, startdttm.month,
	      		startdttm.day );
	        }
	    }
	    else {
		sprintf( _sdttm, "%04d%02d%02d%02d",
	      		startdttm.year, startdttm.month,
	      		startdttm.day,  startdttm.hour );
	     }
	}

/*
 * set the ending time 
 */
	SET_DTTM(enddttm, curtim);
	if ( strcmp ( dtyp_info[idtyp].datatyp, "TAFS_DEC" ) == 0 ) {
	    sprintf( _edttm, "%04d%02d%02d%02d",
                  enddttm.year, enddttm.month,
                  enddttm.day,  enddttm.hour );
        }
	if ( dtyp_info[idtyp].bsflag[0] == 'O' ) {
		sprintf( _edttm, "%04d%02d%02d",
		      enddttm.year, enddttm.month,
		      enddttm.day );
	}
	else
		sprintf( _edttm, "%04d%02d%02d%02d",
		      enddttm.year, enddttm.month,
		      enddttm.day,  enddttm.hour );

	_dttmlen = strlen(_sdttm);

/*
 * Set EXTEN for use in the SELECT_DIR function.
 */
	 if  ( ( srchInfo.smethod == OBS ) &&
            ( ( strcmp ( dtyp_info->datatyp,"SFC_HRLY" ) == 0 ) ||
              ( strcmp ( dtyp_info->datatyp, "SND_DATA" ) == 0 ) ||
              ( strcmp ( dtyp_info->datatyp, "SYN_DATA" ) == 0 ) ) ) {
		sprintf( _exten, "%s", dtyp_info[idtyp].filext );
	}
	else {
		sprintf( _exten, ".%s", dtyp_info[idtyp].filext );
	}
	_extlen = (int)strlen(_exten);
/*
 * Set DTYPE for use in the SELECT_DIR function.
 */
	strcpy( _dtype, dtyp_info[idtyp].datatyp );

/*
 * Expand the environmental variable in the directory path.
 */
	css_envr( dtyp_info[idtyp].datadir, direxp, &ier );
	if  ( ier == 0 ) {
	    strcpy( dir_info->dirpath, direxp );
	}
	else {
	    strcpy( dir_info->dirpath, dtyp_info[idtyp].datadir );
	}

/*
 * Scan the directory, selecting only the files with the
 * correct file extension and time constraints.
 */
	if  ( (dir_info->nent = scandir( dir_info->dirpath,
		&namelist, _select_dir,
#ifdef HPUX
	(int(*)(const struct dirent **, const struct dirent **))
#endif
	    alphasort) ) <= 0 ) {
	    if ( dir_info->nent == 0 ) {
		*iret = -1;
	    }
	    else {
		*iret = -8;
	    }
	    return;
	}
	dir_info->cfilnum = dir_info->nent - 1;

/*
 * Set the structure array of file names.
 */
	for ( i = 0; i < dir_info->nent && i < NFILES; i++ ) {
	    strcpy( dir_info->filnam[i], namelist[i]->d_name );
	    free( namelist[i] );
	}

/*
 * Free the allocated space.
 */
	free(namelist);
}

/*=====================================================================*/

#ifdef IRIX
int _select_dir ( struct dirent *check )
#else
int _select_dir ( const struct dirent *check )
#endif
/************************************************************************
 * _select_dir								*
 *									*
 * This function is used with the SCANDIR function to limit the search	*
 * to the files matching the file extension for the data type and       *
 * within the time constraints.						*
 *									*
 * int _select_dir ( check )						*
 *									*
 * Input parameters:							*
 *	*check		struct dirent  Structure used by SCANDIR	*
 *									*
 * Output parameters:							*
 *	_select_dir	int		Function value			*
 *					  0 = Not a valid file		*
 *					  1 = Valid file		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI	 	 8/95   use strcmp instead of strstr		*
 * C. Lin/EAI	 	10/95   add time constraints checking 		*
 * D. Kidwell/NCEP	 4/99   change iyr < 20 to iyr <= 20            *
 * D. Kidwell/NCEP	 9/02   add check for SFC_HRLY data type        *
 * M. Mainelli/TPC	11/03   add check for SYN_DATA data type	*
 * T. Piper/SAIC	04/05	add check for SND_DATA data type	*
 ***********************************************************************/
{
	int	len, lendat, idate, iyr, imn, idy, ihr, ier;
	char	tmpstr[11], tmpnam[11];

/*---------------------------------------------------------------------*/
/*
 *  Exclude all the files starting with '.'.
 */
	if (check->d_name[0] == '.')
        return(0);
/*
 * Check for the file extension, date/time constraints in the file.
 */
	len = (int)strlen(check->d_name);
	lendat = len - _extlen;

	cst_ncpy ( tmpstr, check->d_name, lendat, &ier );

	cst_numb ( tmpstr, &idate, &ier );
	
	if  ( ( srchInfo.smethod == OBS ) &&
	    ( ( strcmp ( _dtype, "SFC_HRLY" ) == 0 ) ||
	      ( strcmp ( _dtype, "SND_DATA" ) == 0 ) ||
	      (	strcmp ( _dtype, "SYN_DATA" ) == 0 ) ) ) {

	    idy = idate % 100;
	    imn = ( idate / 100 ) % 100;
	    iyr = idate / 10000;
	    if  ( iyr <= 20 )  iyr += 2000;
	    if  ( iyr < 100 )  iyr += 1900;
	    sprintf ( tmpnam, "%04d%02d%02d", iyr, imn, idy );

	}
	else {

	    ihr = idate % 100;
	    idy = ( idate / 100 ) % 100;
	    imn = ( idate / 10000 ) % 100;
	    iyr = idate / 1000000;
	    if  ( iyr <= 20 )  iyr += 2000;
	    if  ( iyr < 100 )  iyr += 1900;
	    sprintf ( tmpnam, "%04d%02d%02d%02d", iyr, imn, idy, ihr );
	}
	
	if ( _sdttm != NULL ) {
	    if ( (strncmp(tmpnam, _sdttm, _dttmlen ) < 0) ||
		(strncmp(tmpnam, _edttm, _dttmlen) > 0) || 
	        (strcmp(&(check->d_name[len - _extlen]), _exten) != 0) )
	    	return ( 0 );
	    else
		return ( 1 );
	}
	else {
	    if ( (strncmp(tmpnam, _edttm, _dttmlen) > 0) || 
	        (strcmp(&(check->d_name[len - _extlen]), _exten) != 0) )
	    	return ( 0 );
	    else
	    	return ( 1 );
	}
}

/*=====================================================================*/

void dir_getnextf ( struct directory_info *dir_info, int strtflg, 
			struct data_file_info *file_info, int *iret ) 
/************************************************************************
 * dir_getnextf								*
 *									*
 * This routine will open the last file in the data directory. The      *
 * assumption is that dir_info only contains desired files which meets  *
 * file extension and date/time limits.					*
 *									*
 * dir_getnextf ( dir_info, strtflg, file_info, iret )         		*
 *									*
 * Input parameters:							*
 * *dir_info	struct directory_info	Directory information		*
 * strtflg	int			Start/resume flag		*
 *					  -1 = Resume backward		*
 *					   0 = Start over		*
 *					   1 = Resume foreward		*
 *									*
 * Output parameters:							*
 * *file_info	struct data_file_info	Data file information		*
 * *iret	int			Return code			*
 *				0 -- new file				*
 *				2 -- no new file			*
 *				3 -- new file and also last file in     *
 *					forward/backward		*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI		 8/95 rewrite for new nwx from nwx2.1		*
 * G. Krueger/EAI	 3/96 cleaned-out cfl_sopn declaration		*
 * L. Williams/EAI	 5/96 check for daily reports			*
 * L. Williams/EAI	 6/96 check for "S" data type			*
 ***********************************************************************/
{
char            newfile, lastf;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	newfile = 1;
	lastf   = 0;

	switch ( strtflg ) {

	  case 0: 	/* start over, new product */

		dir_info->cfilnum = dir_info->nent - 1;
		if( dir_info->nent == 1 ) {	/* daily report */
			lastf = 1;
		}
		break;

	  case 1:	/* foreward */
		
/*
 * check upper limit
 */
		if ( dir_info->cfilnum < dir_info->nent - 1)
			dir_info->cfilnum ++;
		else
			newfile = 0;

		if ( dir_info->cfilnum == dir_info->nent-1 ) {
		   if ( nwxTable->dtyp_info[srchInfo.idtyp].bsflag[0] == 'S' ) 
			dir_info->cfilnum = dir_info->nent - 2 ;
		   lastf = 1;
	 	}
			
		break;
		
	  case -1:	/* backward */
		
/*
 * check lower limit
 */
		if ( dir_info->cfilnum > 0 ) { 
			dir_info->cfilnum --;
		}
		else
			newfile = 0;

		if ( dir_info->cfilnum == 0 ) {
		   if ( nwxTable->dtyp_info[srchInfo.idtyp].bsflag[0] == 'S' ) 
			dir_info->cfilnum += 2;
		   lastf = 1;
		}
			
		break;
	}

/*
 * If new data file is found.
 */
	if ( newfile ) {
		sprintf( file_info->filnam, "%s/%s", dir_info->dirpath,
				dir_info->filnam[dir_info->cfilnum]);
		*iret = 0;
		if ( lastf ) *iret = 3;
	}
	else {
		*iret = 2;
	}
}
