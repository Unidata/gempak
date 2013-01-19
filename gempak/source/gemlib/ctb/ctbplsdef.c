#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Prmlst_t	PlTable;
extern	int		PlReadin;

#define	MAXPRM	10
#define	PRMLEN	20

void ctb_plsdef ( char *dtype_i, char *alias_i, char *parm_i, int *iret )
/************************************************************************
 * ctb_plsdef								*
 *									*
 * This function loads information from the prmlst.tbl table into the 	*
 * PlTable structure.	Only information matching alias_i and dtype_i	*
 * and parm_i will be loaded, unless the string parm_i is "ALL", 	*
 * then all parms are loaded.						*
 *									*
 * ctb_plsdef ( dtype_i, alias_i, parm_i, iret )			*
 *									*
 * Input parameters:							*
 *      *dtype_i        char    Data type                               *
 *      *alias_i        char    Alias name                              *
 *      *parm_i         char    Parameter name				*
 *									*
 * Output parameters:							*
 * 	*iret		int	Return code				*
 *				  -1 - Unable to open prmlst table	*
 *				       or table not read in		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * D.W.Plummer/NCEP	 3/98	Modification for matching on parm	*
 * S. Jacobs/NCEP	 8/00	Added initialization of defstr		*
 * S. Jacobs/NCEP	 8/00	Increased PRMLEN from 16 to 20		*
 ***********************************************************************/
{
int	i, ier;
char 	buff[120];
FILE	*fp;
char	**carr, *parmnam, *parmval;
char	defstr[2], *phat, *ptr, *ptrlp, *ptrrp;
int	nstr, ieq, ilen, npe;
char    alias[32], dtype[16], parm[20];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    if ( PlReadin == G_FALSE )  {
        *iret = -1;
        return;
    }

    /*
     *  Open the prmlst table. If not found return an error.
     */

    fp = (FILE *)cfl_tbop( PRMLST_TBL, "config", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    cst_lcuc( dtype_i, dtype, &ier );
    cst_lcuc( alias_i, alias, &ier );
    cst_lcuc(  parm_i,  parm, &ier );

    carr = (char **)malloc(sizeof(char*)*MAXPRM);
    for ( i = 0; i < MAXPRM; i++ )  {
        carr[i] = (char *)malloc(sizeof(char)*PRMLEN);
    }

    defstr[0] = '\0';

    /* 
     *  Find the entry matching alias and dtype.
     */

    while ( !feof(fp) ) {

        cfl_trln(fp, sizeof(buff), buff, &ier);
        phat = strchr(buff, '^');

        if ( ier == 0 && phat != NULL )  {

    	    cst_rmbl( buff, buff, &ilen, &ier );
    	    cst_lcuc( buff, buff, &ier );

	    cst_clst( phat+1, '|', defstr, MAXPRM, PRMLEN, carr, &nstr, &ier );

	    if ( strcmp( alias, carr[0] ) == 0  &&
		 strcmp( dtype, carr[1] ) == 0 )  {

		i = 0;
		while ( strcmp(PlTable.info[i].alias,alias) != 0  ||
			strcmp(PlTable.info[i].dtype,dtype) != 0 )  {
		    i++;
		}

		cfl_trln(fp, sizeof(buff), buff, &ier);
		phat = strchr(buff, '^');

		npe = 0;
		while ( !feof(fp) && phat == NULL &&
			npe < PlTable.info[i].npe )  {

    	            cst_rmbl( buff, buff, &ilen, &ier );
	            cst_lcuc( buff, buff, &ier );
		    
	            cst_lstr( buff, &ilen, &ier );

	            ptr = strchr( buff, '=' );
	            buff[ilen] = '\0';

	            parmval = ptr + 1;

	            ieq = (int) ( ptr - buff );
	            buff[ieq] = '\0';

	            parmnam = buff;

		    if ( strcmp( parm, "ALL" ) == 0 )  {

	                strcpy( PlTable.info[i].parmele[npe].value, parmval );
	                strcpy( PlTable.info[i].parmele[npe].name, parmnam );

		    }
		    else  {

			if ( strcmp( parmnam, parm ) == 0  &&  
			     strcmp( parmnam, 
				PlTable.info[i].parmele[npe].name ) == 0 )  

	                strcpy( PlTable.info[i].parmele[npe].value, parmval );

		    }

		    if ( strcmp( parmnam, "COLORS" ) == 0 )  {
		        ptrlp = strchr(parmval, '(');
                        ptrrp = strchr(parmval, ')');
                        if ( ptrlp != NULL && ptrrp != NULL )
                            PlTable.info[i].colcod = 1;
                        else
                            PlTable.info[i].colcod = 0;
		    }

		    npe++;

		    cfl_trln(fp, sizeof(buff), buff, &ier);
		    phat = strchr(buff, '^');

		}
		
    		cfl_clos(fp, &ier);

		return;

	    }
	    
	}

    }

    cfl_clos(fp, &ier);

}
