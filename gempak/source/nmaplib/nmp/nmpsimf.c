#include "nmpcmn.h"

void nmp_simf ( int lp, char imgfile[], int imgtyp, int *iret )
/************************************************************************
 * nmp_simf                                                           	*
 *                                                                      *
 * This function sets the image file string in the maps structure.	*
 *                                                                      *
 * void nmp_simf (lp, imgfile, imgtyp, iret )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp                  int	loop index                      	*
 *  imgfile[]           char	image file name	               		*
 *  imgtyp		int	image type				*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret               int	return code                            	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            03/01   Created                                 *
 * E. Safford/GSC	04/01	added imgtyp to param list		*
 ***********************************************************************/
{
int	image_t;
/*---------------------------------------------------------------------*/
    *iret = 0;
    image_t = NO_IMG;

    /*
     * Check for loop number out of range
     */
    if (lp < 0 || lp >= MAX_LOOP ) {
        *iret = -11;
        return;
    }

    if (strlen(imgfile) <= (size_t)0 ) {
	strcpy(maps[lp].imgfile, " ");
    }
    else {
	strcpy(maps[lp].imgfile, imgfile);
	if (imgtyp == SAT_IMG || imgtyp == RAD_IMG) {
	    image_t = imgtyp;
	}
    }

    maps[lp].imgtyp = image_t;
}

/*=====================================================================*/
