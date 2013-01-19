#include "geminc.h"
#include "gemprm.h"
#include "nmap_stbl.h"  

/*
 *  private callback functions
 */
void stbl_readSfcTbl ( char *tblname, sfcstbl_t *stnmtbl, int *iret );
void stbl_readSnmTbl ( char *tblname, snmstbl_t *stnmtbl, int *iret );

/************************************************************************
 * nmap_stbl.c                                                        	*
 *                                                                      *
 * This module reads surface or sounding station model tables. It 	*
 * stores the information from table to corresponding data structure.	*
 *                                                                      *
 * CONTENTS:                                                            *
 *   stbl_readSfcTbl()	process surface table file into data structure	*
 *   stbl_readSnmTbl()	process sounding table file into data structure	*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void stbl_readSfcTbl ( char *tblname, sfcstbl_t *stnmtbl, int *iret ) 
/************************************************************************
 * stbl_readSfcTbl                                                     	*
 *                                                                      *
 * This routine will read surface station model table and create the    *
 * data structure to store the information.                             *
 *                                                                      *
 * void stbl_readSfcTbl(tblname, stnmtbl, iret)                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *                                                                      *
 * Output parameters:                                                   *
 *      *stnmtbl        sfcstbl_t   surface station model table struct  *
 *      *iret		int             0 - success                     *
 *					-1 - error                      *
 * Return code:                                                         *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            3/97                                           *
 * C. Lin/EAI            2/98	add filter                              *
 ***********************************************************************/
{
int   i, n, ier;
char  buffer[256], title[40], alias[40];
float filter;
FILE  *fp;

/*---------------------------------------------------------------------*/

	*iret = 0; 

        fp = cfl_tbop( tblname, "nmap", &ier );
        if ( fp == NULL || ier != 0 ) {
		*iret = -1;
                return;
        }

        n = 0;
        while ( !feof(fp) ) {

            /*
             * read a record
             */
            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 )  n++;

        }

        if ( n != 0 ) {
            stnmtbl->nitems = n;
            stnmtbl->items  = (sfc_ent_t *)malloc(n*sizeof(sfc_ent_t));
        }
        else {
            fclose(fp);
	    *iret = -1;
            return;
        }

        rewind(fp);

        i = 0;
        while ( i < n ) {

            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 ) {
                sscanf(buffer, "%s %s %f", title, alias, &filter);

                stnmtbl->items[i].title =
                        (char *)malloc(strlen(title) + 1);
                strcpy(stnmtbl->items[i].title, title);

                stnmtbl->items[i].alias =
                        (char *)malloc(strlen(alias) + 1);
                strcpy(stnmtbl->items[i].alias, alias);

		stnmtbl->items[i].filter = filter;

                i++;
            }
        }

        fclose(fp);

}

/*=====================================================================*/

void stbl_readSnmTbl ( char *tblname, snmstbl_t *stnmtbl, int *iret )
/************************************************************************
 * stbl_readSnmTbl                                                     	*
 *                                                                      *
 * This routine will read sounding station model table and create the   *
 * data structure to store the information.                             *
 *                                                                      *
 * void stbl_readSnmTbl(tblname, stnmtbl, iret)                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *                                                                      *
 * Output parameters:                                                   *
 *      *stnmtbl        snmstbl_t  sounding station model table struct  *
 *      *iret		int             0 - success                     *
 *					-1 - error                      *
 * Return code:                                                         *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            3/97                                           *
 ***********************************************************************/
{
int  i, n, level, ier;
char buffer[256], title[40], alias[40], vcoord[40];
FILE *fp;

/*---------------------------------------------------------------------*/

	*iret = 0; 

        fp = cfl_tbop( tblname, "nmap", &ier );
        if ( fp == NULL || ier != 0 ) {
		*iret = -1;
                return;
        }

        n = 0;
        while ( !feof(fp) ) {

            /*
             * read a record
             */
            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 )  n++;

        }

        if ( n != 0 ) {
            stnmtbl->nitems = n;
            stnmtbl->items  = (snm_ent_t *)malloc(n*sizeof(snm_ent_t));
        }
        else {
            fclose(fp);
	    *iret = -1;
            return;
        }

        rewind(fp);

        i = 0;
        while ( i < n ) {

            cfl_trln(fp, 256, buffer, &ier);

            if ( ier == 0 ) {
                sscanf(buffer, "%s %s %s %d", title, alias, 
						vcoord, &level);

                stnmtbl->items[i].title =
                        (char *)malloc(strlen(title) + 1);
                strcpy(stnmtbl->items[i].title, title);

                stnmtbl->items[i].alias =
                        (char *)malloc(strlen(alias) + 1);
                strcpy(stnmtbl->items[i].alias, alias);

		cst_lcuc(vcoord, vcoord, iret);
                stnmtbl->items[i].vcoord =
                        (char *)malloc(strlen(vcoord) + 1);
                strcpy(stnmtbl->items[i].vcoord, vcoord);

		stnmtbl->items[i].level = level;

                i++;
            }
        }

        fclose(fp);

}

/*=====================================================================*/
