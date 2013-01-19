#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern Fontsz_t		FszTbl;
extern char 		FszReadin;

void ctb_fszfnd ( float size, int *which, int *iret )
/************************************************************************
 * ctb_fszfnd								*
 *									*
 * This function finds the font size defined in the table that 'matches'*
 * with the input font size. By matching we mean the input size falls  	*
 * within the range of { (val[which-1]+val[which])/2,                   *
 * 		     	 (val[which], val[which+1]/2) }.		* 
 *									*
 * ctb_fszfnd ( size, which, iret )					*
 *									*
 * Input parameters:							*
 *  size	float	input font size					*
 *									*
 * Output parameters:							*
 * *which	int	the index to the font size array 		*
 * *iret	int	Return code					*
 *				  -1 - font size file hasn't been read	*
 **									*
 * Log:									*
 * C. Lin/EAI	 8/98							*
 ***********************************************************************/
{
int   i, n, index;
float low_sz, hi_sz;

/*---------------------------------------------------------------------*/
    	*iret = G_NORMAL;

    	if ( FszReadin == 0 )  {
		*iret = -1;
		return;
	}

        n      = FszTbl.nfsz;;
        low_sz = (FszTbl.info[0].value + FszTbl.info[1].value)/2.0F;
        hi_sz  = (FszTbl.info[n-1].value + FszTbl.info[n-2].value)/2.0F;
        if ( size <= low_sz ) {
            index = 0;
        }
        else if ( size > hi_sz ) {
            index = n-1;
        }
        else {

            for ( i = 1; i < n-1; i++ ) {
        	low_sz = (FszTbl.info[i-1].value +
                                        FszTbl.info[i].value)/2.0F;
                hi_sz = (FszTbl.info[i].value +
                                        FszTbl.info[i+1].value)/2.0F;
                if ( size > low_sz &&  size <= hi_sz ) {
                        index = i;
                        break;
                }
            }
        }

	*which = index;

}
