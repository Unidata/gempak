#include "xwcmn.h"
#include "color.h"

static unsigned int _xgbval ( unsigned char *data, int size );

void xgbank ( register Display *dpy, int *iret ) 
/************************************************************************
 * xgbank                                          			*
 *                                                                      *
 *  This function gets the shared color structure from the shared     	*
 *  window property.  It obtains the number of color banks, the     	*
 *  number of colors in each color bank, and the color indicies of	*
 *  each color bank when there exists the share color mechanism		*
 *  among the applications. 						*
 *                                                                      *
 * void xgbank(dpy, iret )    						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*dpy	register Display   specifies a connection to            *
 *                                      an X server.                    *
 *                                                                      *
 * Return parameters:                                                   *
 *  iret            int*        Return Code  				*
 *              		G_NORMAL = normal return 		*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       02/94                                               *
 * C. Lin/EAI       12/95  check if ColorBanks.banks[i] > 0             *
 * R. Tian/SAIC     05/02  Added ColorBanks entry for FAX image         *
 * S. Chiswell	    07/02  Updated for 16/24 bit color			*
 * T. Piper/SAIC	07/04	Added use of <type>Cid indexing		*
 ***********************************************************************/
{
int             ii, jj, err;
int  		nbyte;
char            shared_color_flag;
unsigned char   getcolr[MAXCOLORDATA], *dptr;
int 		xdpth, bsize;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL; 

	xdpth = DefaultDepth ( (XtPointer)gemdisplay,
				DefaultScreen((XtPointer)gemdisplay) );
	/*
	 * if xdpth != 8, then we use 4 bytes to store information
	 */
	if ( xdpth != 8 )
	   bsize = 4;
        else
	   bsize = 1;

	/*
         *  get the shared_color_flag defined by
	 *  ShareColorFlag 
         */

	shared_color_flag = 0;
	nbyte = 1;
	err = xgsdat(dpy, ShareColorFlag, (unsigned char *)&shared_color_flag, 
			(unsigned int *)&nbyte);

	/*
         * get the color bank data defined by
	 * ShareColorData when share_color_flag is set. 
	 */

	if ( (err == G_NORMAL) && (shared_color_flag == 1) ){

		nbyte = MAXCOLORDATA; 
		err = xgsdat(dpy, ShareColorData, getcolr, (unsigned int *)&nbyte);

		/*
		 * fill in the ColorBanks structure
	 	 */
		if ( err == G_NORMAL ) { 

			dptr = &getcolr[0];

			ColorBanks.nbank = _xgbval((void *)dptr, bsize);
                        dptr += bsize;

			ColorBanks.banks = (int *)
				malloc( ColorBanks.nbank * sizeof(int ));

			for ( ii = 0; ii < ColorBanks.nbank; ii++)
				{
				ColorBanks.banks[ii] = _xgbval((void *)dptr, bsize);
				dptr += bsize;
				}

			ColorBanks.colrs =(Pixel **)
				malloc( ColorBanks.nbank * sizeof(Pixel *));

			for ( ii = 0; ii < ColorBanks.nbank; ii++) {
				/*
				 * allocate the memory for each color bank
				 */
			    if ( ColorBanks.banks[ii] > 0 ) {
				ColorBanks.colrs[ii] = (Pixel *)
                                   malloc( ColorBanks.banks[ii] * sizeof(Pixel) );

				for ( jj = 0; jj < ColorBanks.banks[ii]; jj++ )
					{
					ColorBanks.colrs[ii][jj] = _xgbval((void *)dptr, bsize);
				        dptr += bsize;	
                                        }
			    }
			}

			/*
			 * set the flag to be shared color
			 */
			GColorIsInitialized = SHARECOLOR;
		}

	}
	else {

		/*
		 * use default number of color banks
		 */
        	ColorBanks.nbank = 4;   /* graphic, satellite, radar, fax */
		ColorBanks.banks = (int *)
				malloc( ColorBanks.nbank * sizeof(int) );
        	ColorBanks.banks[GraphCid] = GRAPH_COLORS;
        	ColorBanks.banks[SatCid] = SAT_COLORS;
        	ColorBanks.banks[RadCid] = RAD_COLORS;
        	ColorBanks.banks[FaxCid] = FAX_COLORS;

		/* 
		 * allocate the memory for color indicies in 
		 *  ColorBanks.colrs but the actual values are left
		 *  to xcaloc().
		 */
                 ColorBanks.colrs = (Pixel **)
                              malloc(ColorBanks.nbank * sizeof(Pixel *));
                 for ( ii = 0; ii < ColorBanks.nbank; ii++) 
                       ColorBanks.colrs[ii] = (Pixel *)
                              malloc(ColorBanks.banks[ii] * sizeof(Pixel));

		/*
		 * set the flag to NOT be shared color
		 */
        	GColorIsInitialized = NSHARECOLOR;
	}

}

/*=====================================================================*/

static unsigned int _xgbval ( unsigned char *data, int size )
/************************************************************************
 * _xgbval								*
 *                                                                      *
 * This function converts a number of 1-byte chars into a 4-byte 	*
 * integer. The number of chars is defined by the variable "size".	*
 *                                                                      *
 * unsigned int _xgbval ( unsigned char *data, int size )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*data		unsigned char	Original 1-byte data		*
 *	size		int		Number of chars to convert	*
 *                                                                      *
 * Return parameters:                                                   *
 *	_xgbval		unsigned int	Converted 4-byte integer	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Chiswell	    07/02	Created					*
 ***********************************************************************/

{

int	ii, value=0, itemp;

/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < size; ii++ ) {
	itemp = (int) data[ii];
	value = value << 8 | itemp;
    }

    return(value);
}
