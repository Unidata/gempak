#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

Fontsz_t	FszTbl;
char 		FszReadin = 0;

void ctb_fszrd ( int *iret )
/************************************************************************
 * ctb_fszrd								*
 *									*
 * This function loads the fontsz.tbl table into its structure.		*
 *									*
 * ctb_fszrd ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *iret		int	Return code				*
 *				  -1 - Unable to open fontsz table	*
 *				  -2 - No valid records in fontsz table	*
 **									*
 * Log:									*
 * C. Lin/EAI	 8/98							*
 ***********************************************************************/
{
FILE    *fp;
int     n, nr, xfont, ier;
float   value;
char    buffer[256], name[40];

/*---------------------------------------------------------------------*/
    	*iret = G_NORMAL;

    	if ( FszReadin == 1 )  return;

    	/*
     	 *  Open the fontsz table. If not found return an error.
     	 */

    	fp = (FILE *)cfl_tbop( FONTSZ_TBL, "config", &ier);
    	if ( fp == NULL  ||  ier != 0 )  {
        	*iret = -1;
        	return;
    	}

        cfl_tbnr(fp, &nr, &ier);
        if ( ier != 0 || nr == 0 ) {
            *iret = -2;
            return;
        }

        FszTbl.info = (FSZinfo *)malloc(nr*sizeof(FSZinfo));

        n  = 0;
        while ( n < nr ) {

            cfl_trln( fp, 256, buffer, &ier );
            if ( ier != 0 )
                    break;

            sscanf( buffer, "%s %f %d", name, &value, &xfont );

            cst_uclc(name, name, &ier);
            name[0] = toupper(name[0]);

            if ( strlen(name) < (size_t)10 )
                strcpy(FszTbl.info[n].name, name);
            else {
                strncpy(FszTbl.info[n].name, name, 9);
                FszTbl.info[n].name[9] = '\0';
            }

            FszTbl.info[n].value = value;
            FszTbl.info[n].xval  = xfont;

            n++;
        }

    	cfl_clos(fp, &ier);

	FszTbl.nfsz = n;

    	FszReadin = 1;

}
