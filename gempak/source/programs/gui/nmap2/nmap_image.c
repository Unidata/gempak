#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"	/* dsrc_t, SAT, RAD */


/************************************************************************
 * nmap_image.c                                                         *
 *                                                                      *
 * This module contains functions related to displaying image data      *
 * for nmap.         							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      image_setAttr()    set the image attributes for the new dsp     *
 *      image_resetLut()   reset the image LUT file			*
 * 	image_getFileTm()  given an image file name, extract a date str *
 ***********************************************************************/

/*=====================================================================*/

void image_setAttr ( dsrc_t *dsrc )
/************************************************************************
 * image_setAttr                                                        *
 *                                                                      *
 * This function sets the data driver attributes for the image source 	*
 *                                                                      *
 * void image_setAttr ( dsrc )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dsrc	dsrc_t	 data source data structure			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/99	initial coding                       	*
 * S. Jacobs/NCEP	10/99	Changed GRAY LUT to lower case		*
 * S. Chiswell/Unidata	 3/02	Changed Gray to DEFAULT			*
 * R. Tian/SAIC		03/03	Added check img_info for NULL		*
 ***********************************************************************/
{
int	ier, idx;
char	source[256], *img_type, *img_info, *str, imlutf[256];
/*---------------------------------------------------------------------*/

    strcpy (source, dsrc->path);
    img_type = strtok(source, "/");
    img_type = strtok(NULL, "/");

    str      = strstr (dsrc->path, img_type); 
    str++;	
    if ( ( img_info = strchr (str, '/') ) == NULL )
	return; 
    img_info++;

    idx = dsrc->attridx;

    if (strcmp (img_type, "SAT") == 0) {
	/*strcpy (imlutf, "gray");*/
	strcpy (imlutf, "DEFAULT");
        nim_satt(idx, img_type, img_info, imlutf, &(dsrc->attridx), &ier);	
    }
    else {
	strcpy (imlutf, "DEFAULT");
        nim_satt(idx, img_type, img_info, imlutf, &(dsrc->attridx), &ier);
    }
}

/*=====================================================================*/

void image_resetLut ( char *lutfile )
/************************************************************************
 * image_resetLut                                                       *
 *                                                                      *
 * This function resets the LUT file in the data driver common.		*
 *                                                                      *
 * void image_resetLut ( lutfile )                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*lutfile	char	New LUT file name			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	10/99	Created					*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop	*
 * S. Chiswell/Unidata	 3/02	Added support for 24 bit graphics	*
 * T. Piper/SAIC	05/03	removed xwcmn, added XtDisplay()	*
 * E. Safford/GSC	01/04	load only if dataw & mapw aren't up   	*
 * E. Safford/SAIC	02/04	use mcanvw_getDpth			*
 * H. Zeng/SAIC		04/04	added call to xmfrmtg_saveFrmTag()	*
 * E. Safford/SAIC	04/05	added loop_changeLoop at end		*
 ***********************************************************************/
{
#define FNAME_LEN       256
int	loop, nindex, mindex, iret;
char	imtype[81], iminfo[81], imlutf[81];
int	isimage = 0;
Boolean	reload  = False;

static char     _lastlutfilesat[FNAME_LEN] = {""};
static char     _lastlutfilerad[FNAME_LEN] = {""};
static char     _currlutfilesat[FNAME_LEN] = {""};
static char     _currlutfilerad[FNAME_LEN] = {""};
/*---------------------------------------------------------------------*/

    loop = loop_getCurLoop ();

    if  ( dataw_isSatSelect (loop, &nindex) ) {
	nim_qatt ( nindex, imtype, iminfo, imlutf, &iret );
	nim_satt ( nindex, imtype, iminfo, lutfile, &mindex, &iret );
	
	isimage = 1;
    }
    else if  ( dataw_isRadSelect (loop, &nindex) ) {
	nim_qatt ( nindex, imtype, iminfo, imlutf, &iret );
	nim_satt ( nindex, imtype, iminfo, lutfile, &mindex, &iret );

	isimage = 2;
    }

    if ( mcanvw_getDpth() != _8_BIT ) {
	if ( !dataw_isUp() && !mapw_isUp() ) {
	    reload = True;
        }

	/* check to make sure images are updated for new
           color table when in 16- or 24-bit screen mode */
	switch ( isimage ) {
		case 1:
        		NxmEnhw_getLutfile(1, _currlutfilesat);

        		if ( reload && 
			     strcmp( _lastlutfilesat, _currlutfilesat ) ) {

			   xmfrmtg_saveFrmTag ( loop, &iret );
                	   dsp_reloadLoop ( loop, &iret );
			   xmfrmtg_restoreFrmTag ( loop, &iret );
		        }
            		strcpy( _lastlutfilesat, _currlutfilesat );
			break;

		case 2:
        		NxmEnhw_getLutfile(2, _currlutfilerad);
            		if ( reload && 
			     strcmp( _lastlutfilerad, _currlutfilerad ) ) {

			   xmfrmtg_saveFrmTag ( loop, &iret );
                	   dsp_reloadLoop ( loop, &iret );
			   xmfrmtg_restoreFrmTag ( loop, &iret );
    			}
            		strcpy( _lastlutfilerad, _currlutfilerad );
			break;
        }
    }

    loop_changeLoop( loop );
}

/*=====================================================================*/

void image_getFileTm ( char *fname, char *time )
/************************************************************************
 * image_getFileTm                                                      *
 *                                                                      *
 * This function extracts the file time from an image file name.	*
 *                                                                      *
 * void image_getFileTm (fname, time )                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fname		char	image file name  			*
 *									*
 * Output parameters:                                                   *
 *	*time		char	time extracted from the file name	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/99	initial coding                     	*
 ***********************************************************************/
{
char	*uscr, *ptr, tmp[256];
/*---------------------------------------------------------------------*/

    /*
     * fname will be have a 2 or 3 undercores in the file name.  The
     * last one separates the yymmdd from the hhmm.  Convert that to a
     * '/' for the gempak standard.  Then truncate the fname string at
     * the underscore immediately preceeding the yy to get the full 
     * gempak standard time. 
     *
     * Note that year will be unchanged -- it will be returned in whatever
     * format it is supplied -- yy or yyyy.
     */
    
    strcpy (tmp, fname);
    uscr = strrchr(tmp, '_');
    uscr[0] = '/';

    ptr = strrchr(tmp, '_');
    ptr++;

    strcpy (time, ptr);

}

/*=====================================================================*/
