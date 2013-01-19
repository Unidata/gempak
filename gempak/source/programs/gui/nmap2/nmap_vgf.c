#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"


#define VGF_FILE_EXT 	".vgf"


/************************************************************************
 * nmap_vgf.c                                                           *
 *                                                                      *
 * This module contains functions related to displaying VGF file        *
 * for nmap.         							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      vgf_setDomtTime()     set dominant time info for VGF data	*
 *                                                                      *
 *      vgf_getLatestFile()   get latest VGF file in specified directory*
 *      vgf_getFname()        get VGF file name by usr_title/fname	*
 *                                                                      *
 *      vgf_validateDsname()  validate the VGF dsname string		*
 ***********************************************************************/

/*=====================================================================*/

int vgf_setDomtTime ( dsrc_t *data, frame_t frames[] )
/************************************************************************
 * vgf_setDomtTime                                                      *
 *                                                                      *
 * This function fills out VGF data information as the dominant data  	*
 * for each frame.                                                      *
 *                                                                      *
 * int vgf_setDomtTime(data, frames)	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  *data      dsrc_t      pointer to a MODEL data source               *
 *                                                                      *
 * Input/Output parameters:                                             *
 *  frames[]   frame_t     array of frames                              *
 *                                                                      *
 * Return parameters:                                                   *
 *	vgf_setDomtTime		int		0  - successful         *
 *						-1 - error occurred     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      5/97                                                 *
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 ***********************************************************************/
{
int     ret;
/*char 	fname[256];
long	flen; */

/*---------------------------------------------------------------------*/

/*	vgf_getFname(data->dir_path, fname);

	if ( fname[0] == '\0' )
	    return(-1);
*/
	/*
	 * check VGF file 
	 */
/*	cfl_inqr(fname, NULL, &flen, fname, &ret);

*/
	/*
	 * fill out frame info structure
	 */
	ret = 0;
	if ( ret == 0 ) {

/*          frames[0].nlayer = 1;
            frames[0].ipxm = 0; 
            frames[0].overlay[0].dsrc = data;
            strcpy(frames[0].overlay[0].fname, fname);
*/
	    return(0);
        }

	return(-1);
}

/*=====================================================================*/

void vgf_getLatestFile ( char *dir, char *vgfname ) 
/************************************************************************
 * vgf_getLatestFile                                                    *
 *                                                                      *
 * This function scans specified directory to find latest "*.vgf" file. *
 *                                                                      *
 * void vgf_getLatestFile(dir, vgfname)                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dir        char     directory where VGF file resides       	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *vgfname      char     latest VGF filename       			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      5/97  						*
 * C. Lin/EAI     12/97	    add free dnamelist  			*
 * T. Piper/SAIC	10/07	Added if check before free dnamelist	*
 ***********************************************************************/
{
int    ii, nfiles, ignore;
char   fname[256], file[256];

time_t lastt;
struct dirent **dnamelist=NULL;
struct stat   buf;
/*---------------------------------------------------------------------*/

	vgfname[0] = '\0';

        nfiles = cfl_rdir(0, dir, VGF_FILE_EXT, &dnamelist, &ignore);

	if ( nfiles <= 0 )
		return;

	lastt = -1;
	file[0]  = '\0';
	for ( ii = 0; ii < nfiles; ii++ ) {
	    sprintf(fname, "%s/%s", dir, dnamelist[ii]->d_name);
	    if ( stat(fname, &buf) == 0 ) {
		if ( buf.st_ctime > lastt ) {
		    lastt = buf.st_ctime;
		    strcpy(file, dnamelist[ii]->d_name);
		}
	    }
	    free( dnamelist[ii]);
	}
        if ( dnamelist != NULL ) free (dnamelist);

	if (file[0] != '\0' ) 
		strcpy(vgfname, file);
}

/*=====================================================================*/

void vgf_getFname ( char *instr, char *vgfname ) 
/************************************************************************
 * vgf_getFname                                                  	*
 *                                                                      *
 * This function first parses the input string to two pieces of         *
 * information: usrer title name and file name. It then searches the    *
 * VGF table to find the VGF directory path, therefore compose the      *
 * full path name of the specified VGF file.				*
 *                                                                      *
 * void vgf_getFname(instr, vgfname)                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *instr        char     input string of format: usr_title/fname      *
 *                                                                      *
 * Output parameters:                                                   *
 *  *vgfname      char     VGF full path file name       		*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      5/97  						*
 ***********************************************************************/
{
char   temp[256], fname[256], usrpath[256], *ptr;

/*---------------------------------------------------------------------*/

	vgfname[0] = '\0';

	strcpy(temp, instr);

	ptr = strchr(temp, '/');
	strcpy(fname, ptr+1);
	*ptr = '\0';
	
	dslw_getVGFpathByTitle(temp, usrpath);

	/*
	 * compose VGF file name
	 */
	if ( usrpath[0] != '\0' )
		sprintf(vgfname, "%s/%s", usrpath, fname);
}

/*=====================================================================*/

int vgf_validateDsname ( char *instr ) 
/************************************************************************
 * vgf_validateDsname                                                  	*
 *                                                                      *
 * This function first checks whether the input string is a valid       *
 * DSNAME for VGF file. Format: VGF/usrer_title/file name. 		*
 *                                                                      *
 * int vgf_validateDsname(instr)                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *  *instr        char     input string of format: usr_title/fname      *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *	vgf_validateDsname	int 	1 = valid, 0 = not valid	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      6/97  						*
 ***********************************************************************/
{
char   tmpstr[256];

/*---------------------------------------------------------------------*/

	strcpy(tmpstr, instr);
        if ( strtok(tmpstr, "/") == NULL )
                return(0);
        else if ( strtok(NULL, "/") == NULL )
                return(0);
        else if ( strtok(NULL, "/") == NULL )
                return(0);

	return(1);
}
