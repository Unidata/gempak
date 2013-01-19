#include "geminc.h"
#include "Nxm.h"

void NxmRes_check ( Display *dpy, char *resfil, char *newfil )
/************************************************************************
 * NxmRes_check								*
 *									*
 * This function checks whether a resource file exists based on the     *
 * defined path in environmental variable $XUSERFILESEARCHPATH. If      *
 * this environmental variable is not defined, check the current        *
 * directory, then $HOME directory. If none is found, exit the          *
 * application. If 'newfil' is not NULL, the resource file used will    *
 * be copied into this string.						*
 *									*
 * void NxmRes_check( dpy, resfil, newfil )				*
 *									*
 * Input parameters:							*
 *	*dpy		Display	pointer to the display.			*
 *	*resfil		char	resource file name.			*
 *									*
 * Output parameters:							*
 *	*newfil		char    the resource file name that is used.	*
 *									*
 * Return parameters:							*
 *	  		NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    10/97						*
 * T. Piper/SAIC	02/04	Renamed file from NxmRes.c		*
 ***********************************************************************/
{
char	    *path, *ftmp, filename[256];
int         resflg;
struct stat sbuf;
/*---------------------------------------------------------------------*/

	resflg = 0;

/*
 * check path XUSERFILESEARCHPATH
 */
	path = getenv("XUSERFILESEARCHPATH");
        if ( path ) {

            ftmp = XtResolvePathname(dpy, NULL,
                 		resfil, NULL, path, NULL, 0, NULL);
	    if (ftmp) {
		resflg = 1;
		strcpy(filename, ftmp);
                XtFree(ftmp);
	    } 

	}

/*
 * check home directory
 */
        if (!resflg) {
	    path = getenv("HOME");
	    if ( path ) {
		sprintf(filename, "%s/%s", path, resfil);
		if ( stat( filename, &sbuf ) == 0 ) {
		    resflg = 1;
		}
	    }
	}

/*
 * print out result
 */
        if (!resflg) {
            printf("Fatal Error: cannot find resource file (%s).\n",
                        resfil);
	    printf(" 		check $XUSERFILESEARCHPATH or under $HOME\n");
	    if (newfil)
	        newfil[0] = '\0';

            exit(1);
        }
        else {
            printf("Resource File:  %s\n", filename);
	    if ( newfil )
		strcpy( newfil, filename);
        }
}
