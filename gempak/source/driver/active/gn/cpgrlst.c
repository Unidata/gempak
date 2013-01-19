#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"

#define MAX_STRING_LEN 100
#define DELIM ":"


void pg_rlst ( char *wheel, char *subset, PrdRec *prec, int *iret )
/************************************************************************
 * pg_rlst								*
 *									*
 * This function reads the product table list (product.tbl) to determine*
 * the settings for the product requested in the wheel variable.	*
 *									*
 * pg_rlst ( wheel, subset, prec, iret )				*
 *									*
 * Input parameters:							*
 *	*wheel		char	Name of the product requested		*
 *	*subset		char	Subset of the product requested		*
 *									*
 * Output parameters:							*
 *	*prec		PrdRec	Record of product parameters		*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * E. Wehner/EAi	 5/96	Created					*
 * G. Krueger/EAI	 8/96	Changed CFL_TOPN->TBOP			*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * E. Wehner/Eai	12/96	Remove string literals			*
 * S. Jacobs/NCEP	 7/97	Changed default dir from parms to pgen	*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    FILE * fp;
    char prod_str[MAX_STRING_LEN];
    char table_nm[20];
    char table_sub[10];


    *iret = G_NORMAL;

    strcpy(table_nm, "product.tbl\0");
    strcpy(table_sub, "pgen\0");

    fp = (FILE *)cfl_tbop(table_nm, table_sub, iret);
    if (fp == NULL)
    {
         *iret = G_NOTBL;   /* report no product table found */
         return;
    }


    pg_mtch(fp, wheel, subset, prod_str);

    if (strlen(prod_str) > (size_t)5)
    {
	/* parse the record out into the product record */
        pg_prse(prod_str, prec, iret);
    }
    else
        *iret = G_NOPROD;

    cfl_clos ( fp, iret );


    return;
}


