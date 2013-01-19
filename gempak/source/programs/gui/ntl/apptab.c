#include "geminc.h"
#include "gemprm.h"
#include "app.h"

int apptabFindIcon ( char *result, const char *icon_file );  

apptab *apps_bp;

/************************************************************************
 * apptab.c                                                             *
 *                                                                      *
 * This module deals with the application table. 			*
 *                                                                      *
 * CONTENTS:                                                            *
 *	apptabRead()	 read the application table    			*
 *	apptabFindIcon() find icon file    				*
 ***********************************************************************/

/*=====================================================================*/

unsigned apptabRead ( Widget parent, apptab **bp )
/************************************************************************
 * apptabRead()                                                         *
 *                                                                      *
 * This function reads in the application table file.    		*
 *                                                                      *
 * unsigned apptabRead(parent, bp)                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent 	  Widget	parent widget ID			*
 *      *bp       apptab	pointer to application structure	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *       unsigned  return the effective line count in the table         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi       08/94    						*
 * C. Lin              6/95    reorganize the program and comments      *
 * C. Lin/EAI         12/95    clean up the header                      *
 * G. Krueger/EAI      9/97    Changed NxmWarning -> NxmWarn_show	*
 * T. Piper/SAIC	01/04	replaced fopen with cfl_tbop		*
 * T. Piper/SAIC	01/04	removed filename parameter  		*
 * T. Piper/SAIC	01/04	replaced fgets with cfl_trln		*
 ***********************************************************************/
{
int	ier;
unsigned lc;                    /* table file line counter */
unsigned fc;                    /* table file real line cntr */
unsigned tokc;                  /* token counter in one line */
char    *tokptr;                /* pointer to line_buf token */
FILE    *tfp;                   /* table file contains progs info */
apptab  *ap;                    /* current application pointer */
apptab *last;                   /* pointer to the last application */
char    line_buf[MAX_LBUF];     /* input line buffer */
char    tmp_buf[MAX_LBUF];      /* temp line buffer which is */
                                        /* used to prnt out err if an */
                                        /* invalid line has been found*/
char    tmp_icon[MAX_ICON_NAME];/* temp icon name */

/*---------------------------------------------------------------------*/

    last = *bp;

    /*
     * read table file and create application buttons
     */
    tfp = cfl_tbop(APP_TBL, APP_DIR, &ier);
    if ( tfp == NULL  ||  ier != 0 ) {
	printf("ERROR: Can't find application table file %s.\n", APP_TBL);
	exit(1);
    }
	
    lc = 0;
    tokc = 0;
    fc = 0;
    while ( !feof(tfp) ) {
	cfl_trln(tfp, sizeof(line_buf), line_buf, &ier);
	if ( ier == 0 ) {
            ++fc;

           /*
            * Save the original input line before parsing.
            */
            strcpy (tmp_buf, line_buf);

           /*
            * Not reach end of file yet, process the current input line buffer.
            */
            if ((tokptr = strtok(line_buf, WHITE)) == NULL)
                continue;         /* end of line or end of file */

            if ( *tokptr == ';' || *tokptr == '#' || *tokptr == '!' )
                continue;         /* comment line */

            if ((ap = (apptab *) malloc(sizeof(apptab))) == NULL ) {
                NxmWarn_show( parent, "malloc error [read_table]");
                exit(1);
            }

            ++tokc;
            strcpy(ap->name, tokptr);

            while ( (tokptr = strtok(NULL, WHITE)) != NULL) {

                ++tokc;

                switch (tokc) {
                    case 2: /* icon file */
                        strcpy(tmp_icon, tokptr); break;
                    case 3: /* foreground */
                        strcpy(ap->fg, tokptr); break;
                    case 4: /* background */
                        strcpy(ap->bg, tokptr); tokc = 0; break;
                    default: break;
                } /* end of switch */

            } /* end of while ( (tokptr = strtok(NULL, WHITE)) */

            if ( (apptabFindIcon(ap->icon, tmp_icon) != 0 ) || (tokc != 0) ) {
                printf("Error in table file, line %d : %s\n", fc, tmp_buf);
                exit(1);
            }
            else {
                ap->copies = 0;
                ap->pNext = NULL;
            }


            if ( appbtnCreate(parent, ap ) > 0 ) {
               if ( *bp == NULL ) { /* first node */
                   *bp = ap;
                   last = *bp;
               }
               else {
                   last->pNext = ap;
                   last = ap;
                   last->pNext = NULL;
               }

               ++lc;
            }
	}

    } /* end of while ( !feof(tfp) ) */

    fclose(tfp);

    return lc;
}

/*=====================================================================*/

int apptabFindIcon ( char *result, const char *icon_file )
/************************************************************************
 * apptabFindIcon()                                                     *
 *                                                                      *
 * This function finds the application icon file.    			*
 *                                                                      *
 * int apptabFindIcon(result, icon_file)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      *icon_file  const char  icon table file				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *result  char  full path name of the icon table file		*
 *                                                                      *
 * Return parameters:                                                   *
 *       int           0 = success, -1 = error         			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi       08/94    Patched to use external table            *
 *                               for application programs               *
 * C. Lin              6/95    reorganize the program and comments      *
 * C. Lin/EAI         12/95    clean up the header                      *
 ***********************************************************************/
{
char request[MAX_ICON_NAME]; /* temp buffer for icon file request */
char *ptr1;                  /* always points to head of token */
char *ptr2;                  /* is used to find end of token */
                                /* or the env var */

/*---------------------------------------------------------------------*/

	strcpy(request, icon_file);
   	ptr1 = request;

   	if (request[0] == '$') { /* look for env var */
      	    ptr1 = strtok(request, "/");
      	    ++ptr1; /* by pass the '$' symbol */

      	    if (*ptr1 == '(') {
        	++ptr1;

        	/*
         	 * find the parenthesis pair
         	 */
        	ptr2 = ptr1;
        	while ( (*ptr2 != '\0') && (*ptr2 != ')') ) ++ptr2;
        	if ( *ptr2 == ')' ) *ptr2 = '\0';
        	else return 1; 

      	    } /* end of if (*ptr1 == '(') */

      	    /*
       	     * looking for env var
       	     */
      	    if ((ptr2 = getenv(ptr1)) == NULL) {
        	printf("Environmental variable %s doesn't exist.\n", ptr1);
        	return 1;
            }
      	    else {
        	strcpy(result, ptr2);
            }

            /*
             * find the rest of the path
             */
            while ( (ptr1 = strtok(NULL, "/")) != NULL) {
        	strcat(result, "/");
        	strcat(result, ptr1);
            } /* end of while */

   	} /* end of if (*request == '$') */
   	else {
      		/*
       		 * the path does not contain env var.
       		 * just copy the whole path to the result.
       		 */
      		strcpy(result, request);

   	} /* end of else */

   	if ( access(result, F_OK) != 0 ) {

     		/*
      		 * Can't locate icon file
      		 */
     		printf("Can't find icon file %s\n", result);
     		return 1;
   	}

   	return 0;

}

/*=====================================================================*/
