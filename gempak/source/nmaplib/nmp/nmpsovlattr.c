#include "nmpcmn.h"

void nmp_sovlattr ( int lp, int ovl, nmpovlstr_t ovlattr, int *iret )
/************************************************************************
 * nmp_sovlattr                                                         *
 *                                                                      *
 * This function sets the current attribute values for a given overlay	*
 * and loop.								*
 *                                                                      *
 * void nmp_sovlattr ( lp, ovl, ovlattr, iret )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      lp              int             loop number             	*
 *      ovl             int             overlay number          	*
 *      ovlattr		nmpovlstr_t     attribute values        	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret                   int             Return code             *
 *						= 0   OK		*
 *						= -2 table not read	*
 *						= -3 attribute not match*
 *						= -11 lp out of range 	*
 *						= -12 invalid overlay # *
 *						= -13 invalid ovlattr	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            12/00   Created                                 *
 * M. Li/GSC		12/00	Added nmpovlstr_t			*
 * M. Li/GSC            01/01   Added a check for invalid input         *
 * M. Li/GSC		02/01   Modifed the table element		*
 * J. Wu/SAIC		08/03   allow decimal point in attr string	*
 * T. Piper/SAIC	08/04	Added support for scale legend		*
 * T. Piper/SAIC	10/04	Changed cst_ilst to cst_clst		*
 ***********************************************************************/
{
int     ii, ier, ityp;
size_t	jj;
char	*strarr[10];
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check for invalid loop index
     */
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }


    if ( overlay[lp].novl <= 0 ) {
        *iret = -2;
        return;
    }


    /* 
     * Check for invalid overlay number.
     */
    if (ovl < 0 || ovl >= overlay[lp].novl ) {
	*iret = -12;
   	return;
    }
    
    /* 
     * Check for invalid overlay attribute input
     */
    for (jj = 0; jj < strlen(ovlattr); jj++) {
	if ( ! ( isdigit(ovlattr[jj]) || isspace(ovlattr[jj]) ) &&
	         (ovlattr[jj] != '.' && ovlattr[jj] != '-'  && 
		  ovlattr[jj] != ';')) {
	    *iret = -13;
	    return;
	}

    }
    ityp = overlay[lp].mapovl[ovl].ityp;

    for ( jj=0; jj<(size_t)10; jj++) {
	strarr[jj] = (char*)malloc(64);
    }
    cst_clst(ovlattr, ' ', "?", 10, 64, &strarr[0], &ii, &ier);
    for ( jj=0; jj<(size_t)10; jj++) {
	free(strarr[jj]);
    }
    
    if ( (ityp == 0 && ii != 5) || (ityp == 1 && ii != 3) || 
	 (ityp == 2 && ii != 6) || (ityp == 5 && ii != 10)) {
	*iret = -3;
    }
    else { 
	strcpy (overlay[lp].mapovl[ovl].attr, ovlattr );
    }  

}

/*=====================================================================*/
