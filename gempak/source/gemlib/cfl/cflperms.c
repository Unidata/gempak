#include "geminc.h"
#include "gemprm.h"


void cfl_perms ( char *file, Boolean *can_read, Boolean *can_write, int *iret )
/************************************************************************
 * cfl_perms                                                    	*
 *                                                                      *
 * This function checks the permissions on the passed in file to see if *
 * the user has read and write permission.  If the file is not found,   *
 * can_read is FALSE, and the write permission of the directory is      *
 * checked.  If the directory is not found, can_write is FALSE.  Due to *
 * policy, the user is only allowed to write in a directory owned by    *
 * user.                                                                *
 *                                                                      *
 * void cfl_perms ( file, can_read, can_write, iret )                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *file          char    filename to be checked                   *
 *                                                                      *
 * Output parameters:                                                   *
 *      *can_read       Boolean read  permission result                 *
 *      *can_write      Boolean write permission result                 *
 *	*iret		int	Status = -1 if file cannot be verified	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	 5/02	Created from another function		*
 ***********************************************************************/
{
    uid_t        uid;
    gid_t        gid;
    struct stat  fstat;
    char         cpath[LLPATH], basnam[MXFLSZ], newfile[FILE_FULLSZ];
    int          istat, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;
/*
 * Expand any environment variables in the file name.
 */
    css_envr ( file, newfile, &ier );

    uid = getuid();
    gid = getgid();

    *can_read = *can_write = FALSE;

/*
 * If the file can be found, check the read and write permissions.
 */
    istat = stat( newfile, &fstat );
    if ( istat == 0 ) {
        /*
         * Check read permissions (other, then group, then user).
         */
        if (fstat.st_mode & S_IROTH ||
            (gid == fstat.st_gid && (fstat.st_mode & S_IRGRP)) ||
            (uid == fstat.st_uid && (fstat.st_mode & S_IRUSR)))
            *can_read = TRUE;

        /*
         * Check write permission (other, then group, then user).
         */
        if (fstat.st_mode & S_IWOTH ||
            (gid == fstat.st_gid && (fstat.st_mode & S_IWGRP)) ||
            (uid == fstat.st_uid && (fstat.st_mode & S_IWUSR)))
            *can_write = TRUE;
    }
    else {
	*iret = -1;
        /*
         *  If the newfile was determined to contain a subdirectory then
         *  pull file name off and test for write permission on the target
         *  directory.
         */
	cfl_path ( file, cpath, basnam, &ier );
        if ( cpath == NULL ) strcpy(cpath,"./"); 

	/*
         * If the directory can be found, check the write permissions.
         */
        if (stat (cpath, &fstat) == 0) {
            /*
             * Check write permission (other, then group, then user).
             */
            if (fstat.st_mode & S_IWOTH ||
                    (gid == fstat.st_gid && (fstat.st_mode & S_IWGRP)) ||
                    (uid == fstat.st_uid && (fstat.st_mode & S_IWUSR))) {
                *can_write = TRUE;
            }
        }
    }

}
