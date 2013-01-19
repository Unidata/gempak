#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"

#define LINE_BUF	256

typedef struct _rsrc_t{
        char           name[20];  /* resource name */
        char           parm[128]; /* resource value */
        struct _rsrc_t *next;     /* pointer to the next resource in 
					the link list */
}rsrc_t;

rsrc_t *_rsrcTbl = NULL;	/* head pointer to the resource list */


/************************************************************************
 * nmap_rsrc.c                                                          *
 *                                                                      *
 * This module contains functions related to dealing with the resource  *
 * file for nmap.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      rsrc_readFile()    open and read the resource file.         	*
 *      rsrc_getParm()     get parameter from the resource file.        *
 ***********************************************************************/

/*=====================================================================*/

int rsrc_readFile ( char *fname ) 
/************************************************************************
 * rsrc_readFile                                                        *
 *                                                                      *
 * This function opens an NMAP resource file and read it into memory.   *
 *                                                                      *
 * int rsrc_readFile(fname)              				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname      char     resource filename       			*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *        int      0 = successful      					*
 *                -1 = file can not be opened				*
 *                -2 = syntax error					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96  						*
 * C. Lin/EAI      1/97	   add check after cfl_trln			*
 ***********************************************************************/
{
FILE   *fp;
char   buffer[LINE_BUF], *ptr, *ptr1;
int    len, iret;
rsrc_t *current, *next;

/*---------------------------------------------------------------------*/

	/*
	 * free the previous resource when necessary 
	 */
	if ( _rsrcTbl ) {

		current = _rsrcTbl;

		while(current) {
			next = current->next;
			free((rsrc_t *)current);
			current = next;
		}

		_rsrcTbl = NULL;

	}

	/*
	 * open resource file
	 */
        fp = cfl_tbop(fname, "nmap", &iret);
        if (iret != 0) {
	    return(-1);
        }
        else { /* parse each line */
		

            while ( ! feof(fp) ) {

                cfl_trln(fp, LINE_BUF, buffer, &iret);

		if ( iret == 0 ) {
		    if ( strstr(buffer, "=") == NULL )
			continue;

                    if ( _rsrcTbl == NULL ) { /* head */
                        _rsrcTbl = current =
                            (rsrc_t *)malloc(sizeof(rsrc_t));
                    }
                    else {
                        current->next = (rsrc_t *)malloc(sizeof(rsrc_t));
                        current = current->next;
                    }

		    /*
		     * get the name
		     */
		    ptr = &buffer[0];
		    while ( *ptr == ' ' || *ptr == '\t' ) 
			ptr++;	/* remove leading space */ 
		    ptr1 = current->name;
		    while ( *ptr && *ptr != '=' ) 
			*ptr1++ = *ptr++;
		    *ptr1 = '\0';
		    cst_rmbl(current->name, current->name, &len, &iret);
	
		    /*
		     * get the parm
		     */
		    ptr++;
		    while ( *ptr == ' ' || *ptr == '\t' ) 
			ptr++;	/* remove leading space */ 
		    strcpy(current->parm, ptr);
		    cst_rmbl(current->parm, current->parm, &len, &iret);

                    current->next = NULL;
		}

            }

            fclose(fp);

        }

	return(0);

}

/*=====================================================================*/

int rsrc_getParm ( char *pname, char *parm )
/************************************************************************
 * rsrc_getParm                                                         *
 *                                                                      *
 * This function gets a parameter value in resource file.   		*
 *                                                                      *
 * int rsrc_getParm(pname, parm)              				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *pname      char     parameter name       				*
 *  *parm       char     parameter value string       			*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *        int      0 = successful      					*
 *                -1 = parameter cannot be found			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96  						*
 ***********************************************************************/
{
int    found;
rsrc_t *ptr;

/*---------------------------------------------------------------------*/

	ptr = _rsrcTbl;

	found = 0;
	while ( ptr ) {
		if ( strcmp(ptr->name, pname) == 0) {
			found = 1;
			strcpy(parm, ptr->parm);
			break;
		}
		ptr = ptr->next;
	}

	if (found) 
		return(0);
	else {
		parm[0] = '\0';
		return(-1);
	}

}

/*=====================================================================*/
