#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

Prmlst_t	PlTable;
int		PlReadin=G_FALSE;

#define	MAXPRM	10
#define	PRMLEN	16

void ctb_plrd ( int *iret )
/************************************************************************
 * ctb_plrd								*
 *									*
 * This function loads the prmlst.tbl table into its structure.		*
 *									*
 * ctb_plrd ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *iret		int	Return code				*
 *				  -1 - Unable to open prmlst table	*
 *				  -2 - No valid records in prmlst table	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * T. Piper/SAIC	12/01	Freed carr				*
 ***********************************************************************/
{
int	i, ier;
char 	buff[120];
FILE	*fp;
char	**carr, *parmnam, *parmval;
char	defstr[2], *phat, *ptr, *ptrlp, *ptrrp;
int	nstr, ieq, ilen, numalias, ld, npe;
fpos_t	fpos;

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( PlReadin == G_TRUE )  return;

    /*
     *  Open the prmlst table. If not found return an error.
     */

    fp = cfl_tbop( PRMLST_TBL, "config", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    /* 
     *  Count the number of aliases.
     */

    numalias = 0;
    while ( !feof(fp) ) {
        cfl_trln(fp, sizeof(buff), buff, &ier);
        if ( ier == 0 && strchr(buff, '^') != NULL )  (numalias)++;
    }

    if ( numalias != 0 )  {

        /* 
         *  Allocate the prmlst strucure elements.
         */
        PlTable.nalias   = numalias; 
        PlTable.info     = (PLinfo *) malloc( numalias * sizeof(PLinfo) );
    }
    else  {
	cfl_clos(fp, &ier);
        *iret = -2;
        return;
    }

    rewind(fp);

    /* 
     *  Scan for first line starting with a '^'.
     *  Read in that record and parse to get alias name and datatype.
     *  Scan ahead to determine number of parm names/values strings and
     *  allocate that number of ParmEle structures.  Re-read each string
     *  and place in a ParmEle structure.
     */

    phat = NULL;
    while ( !feof(fp) && phat == NULL ) {
        cfl_trln(fp, sizeof(buff), buff, &ier);
        phat = strchr(buff, '^');
    }
    cst_rmbl( buff, buff, &ilen, &ier );
    cst_lcuc( buff, buff, &ier );

    strcpy( defstr, " " );
    carr = (char **)malloc(sizeof(char*)*MAXPRM);
    for ( i = 0; i < MAXPRM; i++ )  {
	carr[i] = (char *)malloc(sizeof(char)*PRMLEN);
    }

    i = -1;
    while ( i < numalias-1 && !feof(fp) ) {
    
	/*
	 *  New alias; process
	 */
	i++;

	cst_clst( phat+1, '|', defstr, MAXPRM, PRMLEN, carr, &nstr, &ier );

	PlTable.info[i].alias = (char *)malloc(sizeof(char)*strlen(carr[0])+1);
	PlTable.info[i].dtype = (char *)malloc(sizeof(char)*strlen(carr[1])+1);

	strcpy( PlTable.info[i].alias, carr[0] );
	strcpy( PlTable.info[i].dtype, carr[1] );
	

	cst_lstr( PlTable.info[i].dtype, &ld, &ier );
	PlTable.info[i].dtype[ld] = '\0';

	ier = fgetpos( fp, &fpos );

        cfl_trln(fp, sizeof(buff), buff, &ier);
        phat = strchr(buff, '^');

	npe = 0;
	while ( !feof(fp) && phat == NULL ) {
	    npe++;
            cfl_trln(fp, sizeof(buff), buff, &ier);
            phat = strchr(buff, '^');
	}

	PlTable.info[i].npe = npe;
	PlTable.info[i].colcod = 0;

	if ( npe != 0 )  {

	    PlTable.info[i].parmele = (ParmEle *)malloc(sizeof(ParmEle)*npe);
	    
	    ier = fsetpos( fp, &fpos );

            cfl_trln(fp, sizeof(buff), buff, &ier);
            phat = strchr(buff, '^');

	    npe = -1;
	    while ( !feof(fp) && phat == NULL ) {

	        npe++;
    	        cst_rmbl( buff, buff, &ilen, &ier );
	        cst_lcuc( buff, buff, &ier );
	        cst_lstr( buff, &ilen, &ier );

	        ptr = strchr( buff, '=' );
	        buff[ilen] = '\0';

	        parmval = ptr + 1;
	        strcpy( PlTable.info[i].parmele[npe].value, parmval );

	        ieq = (int) ( ptr - buff );
	        buff[ieq] = '\0';

	        parmnam = buff;
	        strcpy( PlTable.info[i].parmele[npe].name, parmnam );

		if ( strcmp( parmnam, "COLORS" ) == 0 )  {
		    ptrlp = strchr(parmval, '(');
                    ptrrp = strchr(parmval, ')');
                    if ( ptrlp != NULL && ptrrp != NULL )
                        PlTable.info[i].colcod = 1;
                    else
                        PlTable.info[i].colcod = 0;
		}

                cfl_trln(fp, sizeof(buff), buff, &ier);
                phat = strchr(buff, '^');

	    }

	}

    }

    for ( i = 0; i < MAXPRM; i++ )  {
	free(carr[i]);
    }
    free(carr);
    cfl_clos(fp, &ier);

    PlReadin = G_TRUE;

}
