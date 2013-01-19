#include "geminc.h"
#include "gemprm.h"
#include "defs.h"
#include "prog.h"


int numOfProcess = 0;
pid_t processPID[MAX_PROC];

/************************************************************************
 * prog.c                                                               *
 *                                                                      *
 * This module deals with invoking the application application programs.*
 *                                                                      *
 * CONTENTS:                                                            *
 *      progGetName()   get application program name 			*
 *      progInvoke()    invoke an application program 			*
 *      progWait()      wait for a child process to exit 		*
 ***********************************************************************/

/*=====================================================================*/

void progGetName ( char prog_name[], const char *envname, 
						const char *apname )
/************************************************************************
 * progGetName()                                                        *
 *                                                                      *
 * This function gets the application program name.                     *
 *                                                                      *
 * void progGetName(prog_name, envname, apname)				*
 *	*envname const char 	env var name				*
 *	*apname	 const char     application prog name			*
 *                                                                      *
 * Output parameters:                                                   *
 *	prog_name[]     char	program executable file name		*
 *                                                                      *
 * Return parameters:                                                   *
 *			NULL						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi       08/94             				*
 * C. Lin              6/95    reorganize the program and comments      *
 * C. Lin/EAI         12/95    clean up the header                      *
 ***********************************************************************/
{
char            *def_dir = NULL;
struct stat     statbuf;
int             fflag = 0;

/*---------------------------------------------------------------------*/

        if (strcmp (envname, "EXIT") != 0) {

           if (stat(apname, &statbuf) == 0) {

/*
 * File is in current directory.
 * We have to make sure it's executable.
 */
                if ( S_ISREG(statbuf.st_mode) &&
                   (statbuf.st_mode & S_IXUSR) ) {
                   fflag = 1;
                }
           }
           if (fflag == 0) {

/*
 * Can't find appl prog name in current directory.
 * Try to locate it with the env var prefix.
 */
                def_dir = getenv(envname);
                strcpy(prog_name, def_dir);
                strcat(prog_name, "/");
                strcat(prog_name, apname);

           } 
           else {

/*
 * Program is in current directory.
 */
                strcpy(prog_name, apname);
	   }

        } /* end of strcmp(envname... */
}

/*=====================================================================*/

pid_t progInvoke ( Widget parent, char *progname, char *apname )
/************************************************************************
 * progInvoke()                                                         *
 *                                                                      *
 * This function invokes the specified program.                     	*
 *									*
 * pid_t progInvoke(parent, progname, apname)				*
 *	parent 		Widget 	parent widget ID			*
 *	*progname 	char    application executable full path	*
 *	*apname		char    application name from button		*
 *                                                                      *
 * Output parameters:                                                   *
 *			NULL						*
 *                                                                      *
 * Return parameters:                                                   *
 *	progInvoke	pid_t	return the process ID of application  	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi       08/94             				*
 * C. Lin              6/95    reorganize the program and comments      *
 * C. Lin/EAI         12/95    clean up the header                      *
 * G. Krueger/EAI      9/97    Changed NxmWarning -> NxmWarn_show	*
 ***********************************************************************/
{ 
pid_t   pid;
char command[MAX_LBUF], message[MAX_MESSAGE];

/*---------------------------------------------------------------------*/

        if ( ( pid = fork() ) == 0 ) {

            strcpy( command, progname );

            if (execl( command, progname,  NULL ) < 0) {
                if (execvp(apname, NULL) < 0) {
                    sprintf(message, "Can't execute %s.\n", command);
                    NxmWarn_show( parent, message);
                }
	    }

            exit(errno);
	}
        return( pid );
}

/*=====================================================================*/
/* ARGSUSED */
void progWait ( int dummy )
/************************************************************************
 * progWait(dummy)                                                      *
 *                                                                      *
 * This function waits for the child process to exit.                   *
 *									*
 * void progWait (dummy)						*
 *                                                                      *
 * Input parameters:                                                    *
 * dummy	int	Not used (eliminates compile warning)		*
 *									*
 * Output parameters:                                                   *
 *			NULL						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin		 8/95                          			*
 * C. Lin/EAI		12/95   clean up the header                     *
 * T. Piper/GSC	 	10/99	Added 'dummy' to eliminate compile warn	*
 ***********************************************************************/
{
int     i;
pid_t   pid;

/*---------------------------------------------------------------------*/

	pid = wait(NULL);

        for ( i = 0; i < numOfProcess; i++ ) {
            if ( processPID[i] == pid ) 
		processPID[i] = -1;
        }

        if (signal(SIGCHLD, progWait) == SIG_ERR){
                printf("signal(SIGCHLD) error\n");
                exit(0);
        }
}

/*=====================================================================*/
