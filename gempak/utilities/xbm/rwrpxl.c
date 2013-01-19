#include "faxcmn.h"
#include "xbm.h"

void rwrpxl(int bnum, int lnum, int *iret)
/************************************************************************
 * rwrpxl								*
 *									*
 *  This routine writes a pixel into a raster plane at a specified	*
 *  location.								*
 *									*
 * rwrpxl ( bnum, lnum, iret )						*
 *									*
 * Input parameters:							*
 *  bnum	int	Which bit number the pixel falls in in scanln	*
 *  lnum	int	Which scanline number the pixel falls 		*
 *									*
 * Output parameters:							*
 * *iret	int		Return code				*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/EAi	12/96	Remove POW calls			*
 * E. Wehner/EAi	 3/97	Provide rotation to pixels when needed	*
 * M. Linda/GSC		 6/97	Bytes in XBM are mirror image of FAX	*
 ***********************************************************************/
{
    int xbyte;   /* which byte along x or y */
    int ybyte;
    int rline_num;
    int rbit_num;
    double rotang;

    int bitnum;
    int offset = 0;  /* offset into bitmap... */
    char xor_mask;
    char bit_mask;
    
    *iret =  G_NORMAL;
    if (landscape == G_TRUE )
    {
        rotang = (double)(rot * M_PI / 180.000);
	rline_num = num_scans + 
            (ispanx * ( (int) ((lnum * cos(rotang) ) - (bnum * sin(rotang) ))));
	rbit_num = bpscan - 
	    (ispany * ( (int) ((lnum * sin(rotang) ) + (bnum * cos(rotang) ))));
    }
    else
    {
        rbit_num = bnum;
	rline_num = num_scans - lnum;
    }

    /* calculate which bit or byte is to be set... */
    switch (bpp)
    {
      case 1:
        xbyte = (int)rbit_num/8;
        ybyte = (int)rline_num;
        bitnum = (int)rbit_num%8;
        /* if in range */
        if ( (rbit_num < (int)bpscan) && (rline_num < (int)num_scans) )
        {
            if (rline_num > 0)
            {
	        offset = ((rline_num-1)*((int)bpscan/8));
            }
            offset += (int)(rbit_num/8);
	    xor_mask = '\0';
	    /* create xor_mask */
	    if (bitnum < 6)
	    {
                xor_mask = xor_mask << ( 6-bitnum);
	    }
            bit_mask = 1 << (bitnum); 
	    *(pixmap + offset) |= ( bit_mask ^ xor_mask);
            /* now, set the bit on by orring it with its current contents */
        }
        break;
      case 8:
        xbyte = bnum;
        ybyte = lnum;
        break;
      case 24:
        xbyte = bnum*3;
        ybyte = lnum*3;
        break;
      default:
        break;
    }
}
