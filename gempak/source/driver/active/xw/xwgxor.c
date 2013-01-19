#include "xwcmn.h"
#include "color.h"

void xw_gxor ( int *ixor, int *iret )
/************************************************************************
 * xw_gxor                                                              *
 *                                                                      *
 * This subroutine gets the xor value for ghost lines, by determining   *
 * what the background color is and then selecting an optimal           *
 * contrasting color.                                                   *
 *                                                                      *
 * xw_gxor ( ixor, iret )                                               *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ixor           int             Xor value                       *
 *      *iret           int             Return code                     *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		08/00						*
 * M. Li/GSC		08/00	Cleaned up				*
 ***********************************************************************/
{
    int         ii;
    XColor      colors[34];
    long	temp, max_contrast;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * scan through graphic colors to find the one which has
     * the most contrast values with the background color
     */
    for ( ii = 0; ii < ColorBanks.banks[GraphCid]; ii++ ) {
        colors[ii].pixel = ColorBanks.colrs[GraphCid][ii];
        colors[ii].flags = DoRed | DoBlue | DoGreen;
    }

    XQueryColors ( gemdisplay, gemmap, colors,
                   ColorBanks.banks[GraphCid] );

    max_contrast  = 0;
    *ixor = 1;

    for ( ii = 1; ii < ColorBanks.banks[GraphCid]; ii++ ) {
        temp =  abs(colors[ii].red - colors[0].red) +
            abs(colors[ii].green - colors[0].green) +
            abs(colors[ii].blue - colors[0].blue);

        if ( temp > max_contrast ) {
            max_contrast = temp;
            *ixor = ii;
        }
    }

}
