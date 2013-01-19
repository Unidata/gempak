#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include <bzlib.h>
#include "proto_xw.h"

/************************************************************************
 * crnexbz.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

#define MAXRADL	1840  /*Max number of bytes of 8-bit data level values per radial*/

void crnexbz256to16 ( unsigned char *val256, unsigned char *val16, int *iret  );
void crnexbzp94color ( unsigned char *val256, unsigned char *val16, int *iret  );
void crnexbzp99color ( unsigned char *val256, unsigned char *val16, int *iret  );
void crnexbzp134color ( unsigned char *val256, unsigned char *val16, int *iret  );
void crnexbzp135color ( unsigned char *val256, unsigned char *val16, int *iret  );
float crnexbz_dc ( short dataval );
static short	_imhwd31 = 0;  /*halfword 31 in the image file*/
static short    _imhwd32 = 0;  /*halfword 32 in the image file*/
static short    _imhwd33 = 0;  /*halfword 33 in the image file*/
static short    _imhwd34 = 0;  /*halfword 34 in the image file*/
static short    _imhwd35 = 0;  /*halfword 35 in the image file*/

/*134 product*/
static float    _imdhwd31 = 0.0;/*decoded halfword 31 in the image file*/
static float    _imdhwd32 = 0.0;/*decoded halfword 32 in the image file*/
static float    _imdhwd34 = 0.0;/*decoded halfword 34 in the image file*/
static float    _imdhwd35 = 0.0;/*decoded halfword 35 in the image file*/

/*135 product*/
static unsigned char _imdatamask = 0xff;   /*DATA MASK, get from halfword 31*/
static unsigned char _imdatascale = 1;     /*DATA SCALE, get from halfword 32*/
static unsigned char _imdataoffset = 0;     /*DATA OFFSET, get from halfword 33*/
static unsigned char _imtoppedmask = 0xff; /*TOPPED MASK, get from halfword 34*/

/************************************************************************
 * crnexbz.c                                                            *
 *                                                                      *
 * This module processes the higher resolution radar images.            *
 *                                                                      *
 * CONTENTS:                                                            *
 *   crnexbz  ()              Process the higher resolution images data *
 *                                                                      *
 * private functions:                                                   *
 *   crnexbz256to16()         Convert 256 data level to 16 color code   *
 *   crnexbzp94color()        Process 256 data level for product 94     *
 *   crnexbzp99color()        Process 256 data level for product 99     *
 *   crnexbzp134color()       Process 256 data level for product 134    *
 *   crnexbzp135color()       Process 256 data level for product 135    *
 *   crnexbz_dc ()            Decode digital data value to VIL for      *
 *                            134 product                               *
 ***********************************************************************/

void crnexbz ( char *imgnam, int *iret )
/************************************************************************
 * crnexz								*
 *									*
 * This subroutine reads the image data from a bzlib NIDS format file.	*
 *									*
 * crnexbz ( imgnam, iret )						*
 *									*
 * Input parameters:							*
 *	*imgnam		char		Name of image file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 *					G_NIMGFL = cannot open/read img	*
 *					G_NMEMRY = memory alloc failure	*
 **									*
 * Log:									*
 * X. Guo/CWS		04/10		Created 			*
 * X. Guo/CWS		04/10           Updated WMO,FOS header          *
 * X. Guo/CWS           05/10           Added code to handle 99, 134 and*
 *                                      135 products			*
 ***********************************************************************/
{

	FILE		*fp;
	char		newfil[160],*defdir=NULL;
	int		ib, ier,i,j;
	unsigned int	nbend,  ii, iii,destLen;
	unsigned short	numr, length, rstang, rdlang, run, ir,compress;
	unsigned char   *imdptr, radial[MAXRADL];

	int		ierr;
	long		flen;

        int     	nbytes, err;
        unsigned char 	*p;
        unsigned char  	*barr=NULL;

	/*
	 * BZLIB decompression variables
	 */
        unsigned char   uncompr[MAX_UNCOMPRESS_BLOCK];
        unsigned short  iarr1,iarr2;
/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;
/*
 *	Get the file size.
 */
	cfl_inqr ( imgnam, defdir, &flen, newfil, &ierr );

/*
 *	Open the input file.
 */
        fp =  cfl_ropn ( imgnam, defdir, &ierr );
        if  ( ierr != 0 )  {
            *iret = G_NIMGFL;
            return;
        }

	memset (uncompr, 0, MAX_UNCOMPRESS_BLOCK);

/*
 *	Read the file.
 */
        barr = (unsigned char *) malloc ( flen * sizeof(char) );
        cfl_read ( fp, flen, (unsigned char *)barr, &nbytes, &ierr );

/*
 *	Close the file.
 */
        cfl_clos ( fp, &ierr );


        if  ( nbytes != flen )  {
            *iret = G_NIMGFL;
            return;
	}
        i = 0;
        j = 0;
        iarr1 = *(unsigned short *)barr;
        iarr2 = *(unsigned short *)&barr[2];
        if ( ((iarr1 == 21316) && (iarr2 == 21843)) || 
             ((iarr1 == 17491) && (iarr2 == 21333))) {
            /* the first 4 bytes are 'S', 'D', 'S', 'U'*/
            j = 2;
        } 
        else if (((iarr1 == 269) && (iarr2 == 3338)) || 
                 ((iarr1 == 3329) && (iarr2 == 2573))) {
           /*First 4 bytes are SOH, cr, cr, nl*/
            j = 4;
        }
       while  ( ( i < (nbytes - 1) ) && ( j > 0 ) )  {
            if  ( ( barr[i] == '\r' ) && ( barr[i+1] == '\n' ) )  {
                j -= 1;
                i += 2;
            }
            else {
                i += 1;
            }
        }
        p = barr+i;
        compress = (p[100] <<8 ) + p[101];
        p = barr + RAD_IMG_COMPRESS_POS + i;
        if ( compress == 1 ) {/*use bz2 to uncompress*/
            destLen = MAX_UNCOMPRESS_BLOCK-RAD_IMG_UNCOMPRESS_LEN-i;
            err = BZ2_bzBuffToBuffDecompress ((char *)uncompr,&destLen,(char *)p,flen,0,0);
            if ( err != BZ_OK ) {
                free (barr);
                *iret = G_NIMGFL;
                return;
            }
        }
        else {
            memcpy ( uncompr, p, flen - RAD_IMG_COMPRESS_POS - i );
        }
/*
 *     Read Packet Code from half word 69 and check if it is 16
 *     Here --- only process PACKET CODE = 16
 */
       p = uncompr + 16;
       iarr1 = (p[0] << 8) + p[1];
       if ( iarr1 != 16 ) {
           free (barr);
           *iret = G_NIMGFL;
           return;
       }       
/*
 *     Read Data Level Threshold
 */
        p = barr + i + 60;
        /* Read halfword 31 */
        _imhwd31 = (p[0] << 8) + p[1];
        /* Read halfword 32 */
        p += 2;
        _imhwd32 = (p[0] << 8) + p[1];
        /* Read halfword 33 */
        p += 2;
        _imhwd33 = (p[0] << 8) + p[1];
        /* Read halfword 34 */
        p += 2;
        _imhwd34 = (p[0] << 8) + p[1];
        /* Read halfword 35 */
        p += 2;
        _imhwd35 = (p[0] << 8) + p[1];
        if ( imtype == 134 ) {
            _imdhwd31 = crnexbz_dc(_imhwd31);
            _imdhwd32 = crnexbz_dc(_imhwd32);
            _imdhwd34 = crnexbz_dc(_imhwd34);
            _imdhwd35 = crnexbz_dc(_imhwd35);
        }
        else if (imtype == 135) {
            _imdatamask = (unsigned char )_imhwd31;
            _imdatascale = (unsigned char )_imhwd32;
            _imdataoffset = (unsigned char )_imhwd33;
            _imtoppedmask = (unsigned char )_imhwd34;
        }
/*
 *      Free the original data pointer
 */
	free(barr);
	barr = NULL;

/*
 *	Set the data pointer at image portion of uncompressed product
 */
	p = uncompr + 28;

/*
 *	Read the number of radials (radial products) or rows (raster
 *	products) and the product data
 */
        numr = (p[0] << 8) + p[1];
	p+=2;
/*
 *	Handle raster products
 */
 	if  ( imrdfl == 0 )  {

        /*
         *      Set pointers to raw data and image data arrays
         */
            imdptr = imgData;


/*
 *	    Skip packing descriptor
 */	     
 	    p += 2;
 	    
/*
 *	    Loop over the number of rows
 */
	    for ( ir = 0; ir < numr; ir++ )  {
	    
                length = (p[0] << 8) + p[1];
                p += 2;
                rstang = (p[0] << 8) + p[1];
                p += 2;
                rdlang = (p[0] << 8) + p[1];
                p += 2;
		
/*
 *		Run Length decode this line
 */
		iii = 0;
		for ( run = 0; run < length; run++ )  {
                    crnexbz256to16 (p, imdptr, &err);		
		    imdptr++; 
                    iii ++;
		    p++;
		}
/*
 *		Make sure the line is completed.
 */
		for ( ii = iii; (int)ii < imnpix; ii++ )
			*imdptr++ = 0; 
		
	    }
	}
	
/*
 *	Handle radial products
 */
 	else {

/*
 *	    Set beam ends to zero for rasterization overflows
 *	    (I don't know if we really need this.)
 */
	    nbend = imnpix / 2;
	    radial [nbend] = 0;
	    radial [nbend+1] = 0;
	  	    
/*
 *	    Loop over the number of radials
 */
	    for ( ir = 0; ir < numr; ir++ ) {		
		
                length = (p[0] << 8) + p[1];
                p += 2;
                rstang = (p[0] << 8) + p[1];
                p += 2;
                rdlang = (p[0] << 8) + p[1];
                p += 2;

/*
 *		Run Length decode this radial
 */
		ib = 0;
		for ( run = 0; run < length; run++ )  {
		    if  ( ib < MAXRADL )  {
                        crnexbz256to16 (p, &radial[ib++],&err);
		    }
		    p++;
		}
/*
 *		Insert this radial in the rasterized image
 */
		crastr ( radial, ib, imnpix,
			 (float)rstang/10., (float)rdlang/10., &ier);

    	    }
 	}
}
void crnexbz256to16 ( unsigned char *val256, unsigned char *val16, int *iret  )
/************************************************************************
 * crnexbz256to16                                                       *
 *                                                                      *
 * This function will convert 256 colors product to 16 colors           *
 *                                                                      *
 * crnexbz256to16 ( val256, val16, iret )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *     * val256       unsigned char     8-bit data level code           *
 *                                                                      *
 * Output parameters:                                                   *
 *     * val16        unsigned char     16 color codes                  *
 *      *iret           int             Return code                     *
 *                                      0 = normal return               *
 *                                     -1 = error return                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           04/10   Created                                 *
 * X. Guo/CWS           05/10	Added code to handle 99,134 and 135 prd *
 ***********************************************************************/
{
         
    *iret = 0;
    switch ( imtype ) {
        case 94:
            crnexbzp94color (val256, val16, iret);
            break;
        case 99:
            crnexbzp99color (val256, val16, iret);
            break;
        case 134:
            crnexbzp134color (val256, val16, iret);
            break;
        case 135:
            crnexbzp135color (val256, val16, iret);
            break;
        default:
            *iret = -1;
            break;
    }
    return;   
}

void crnexbzp94color ( unsigned char *val256, unsigned char *val16, int *iret  )
/************************************************************************
 * crnexbzp94color                                                      *
 *                                                                      *
 * This function will convert 256 colors to 16 colors for product 94    *
 *                                                                      *
 * crnexbzp94color ( val256, val16, iret )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *     * val256       unsigned char     8-bit data level code           *
 *                                                                      *
 * Output parameters:                                                   *
 *     * val16        unsigned char     16 color codes                  *
 *      *iret           int             Return code                     *
 *                                      0 = normal return               *
 *                                     -1 = error return                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           05/10   Created                                 *
 ***********************************************************************/
{
    short 	i;
    float       max, min,valchk;
    float       scale = 1.0, mindata=0.0;
    unsigned short  caoff = 4, psoff = 5, dataval;
    float 	mincaval = -28.0, minpsval = 0.0;
         
    *iret = 0;
    dataval = (unsigned short)(*val256);

    /*Use retrieved information to calculate minimum data value, increment value*/
    scale = _imhwd32/10.0;
    mindata = _imhwd31/10.0;

    /*calculate 8 bit data level clode*/
    valchk = (dataval-2) * scale + mindata;
    if ( immode == 1 ) {
        if ( valchk < mincaval ) {
            *val16 = 0;
            return;
        }
    }
    else {
        if ( valchk < minpsval ) {
            *val16 = 0;
            return;
        }
    }
    /*map 8 bit data level code to 16 colors*/
    for ( i = 1 ; i < 16 ; i ++ ) {
        if ( immode == 1 ) {
            max = caoff * ( i - 7 );
            min = caoff * ( i - 8 );
        }
        else {
            max = psoff * (i+1);
            min = psoff * i ;
        }
        if ( i == 1 ) {
            if ( immode == 1 ) {
                min = mincaval;
            }
            else {
                min = minpsval;
            }
        }
        if ( ( min <= valchk ) && (valchk < max)) {
            *val16 = (unsigned char )(i);
            return ;
        }
        else {
            if ( (i == 15) && (min <= valchk)) {
                *val16 = (unsigned char )(i);
                return;
            }
        }
    }
    *iret = -1;
   return ;
}

void crnexbzp99color ( unsigned char *val256, unsigned char *val16, int *iret  )
/************************************************************************
 * crnexbzp99color                                                      *
 *                                                                      *
 * This function will convert 256 colors to 16 colors for product 99    *
 *                                                                      *
 * crnexbzp99color ( val256, val16, iret )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *     * val256       unsigned char     8-bit data level code           *
 *                                                                      *
 * Output parameters:                                                   *
 *     * val16        unsigned char     16 color codes                  *
 *      *iret           int             Return code                     *
 *                                      0 = normal return               *
 *                                     -1 = error return                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           05/10   Created                                 *
 ***********************************************************************/
{
    short 	i;
    float       max, min,valchk;
    float       scale = 1.0, mindata=0.0;
    unsigned short  caoff = 4, dataval;
    float 	mincaval = -28.0, minpsval = 0.0;
/*99 product color mapping table*/
    struct {
        float min; 
        float max; 
    } _imhr64cb[16] = {{0.0, 0.0}, {-66.0, -61.5}, {-64.0, -50.0}, {-50.0, -36.0},
                       {-36.0, -26.0}, {-26.0, -20.0},{-20.0, -10.0},{-10.0, 0.0},
                       {0.0, 10.0},{10.0, 20.0},{20.0, 26.0},{26.0, 36.0},
                       {36.0, 50.0},{50.0, 64.0},{64.0, 256},{256.0, 300.0}};
    *iret = 0;

    dataval = (unsigned short)(*val256);

    /*Use retrieved information to calculate minimum data value, increment value*/
    scale = _imhwd32/10.0;
    mindata = _imhwd31/10.0;
    minpsval = -63.5;

    if ( dataval <= 1 ) {
        if ( dataval == 1 ) *val16 = 15;
        else *val16 = 0;
        return;
    }
    /*calculate 8 bit data level clode*/
    valchk = (dataval-2) * scale + mindata;

    if ( immode == 1 ) {
        if ( valchk <= mincaval ) {
            *val16 = 0;
            return;
        }
    }
    else {
        if ( valchk < minpsval ) {
            *val16 = 1;
            return;
        }
    }
    /*map 8 bit data level code to 16 colors*/
    for ( i = 1 ; i < 16 ; i ++ ) {
        if ( immode == 1 ) {
            max = caoff * ( i - 7 );
            min = caoff * ( i - 8 );
        }
        else {
            max = _imhr64cb[i].max ;
            min = _imhr64cb[i].min;
        }
        if ( i == 1 ) {
            if ( immode == 1 ) {
                min = mincaval;
            }
            else {
                min = minpsval;
            }
        }
        if ( i < 8 ) {
            if ( ( min < valchk ) && (valchk <= max)) {
                *val16 = (unsigned char )(i);
                return ;
            }
        }
        else {
            if ( ( min <= valchk ) && (valchk < max)) {
                *val16 = (unsigned char )(i);
                return;
            }
        }
    }
    *iret = -1;
   return ;
}

void crnexbzp134color ( unsigned char *val256, unsigned char *val16, int *iret  )
/************************************************************************
 * crnexbzp134color                                                     *
 *                                                                      *
 * This function will convert 256 colors to 16 colors for product 134   *
 *                                                                      *
 * crnexbzp134color ( val256, val16, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *     * val256       unsigned char     8-bit data level code           *
 *                                                                      *
 * Output parameters:                                                   *
 *     * val16        unsigned char     16 color codes                  *
 *      *iret           int             Return code                     *
 *                                      0 = normal return               *
 *                                     -1 = error return                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           05/10   Created                                 *
 ***********************************************************************/
{
    short 	i;
    float       max, min,valchk,datalvl;
    float       scale = 1.0, mindata=0.0;
    unsigned short  caoff = 4, psoff = 5, dataval;
    float 	mincaval = -28.0, minpsval = 0.0;
         
    *iret = 0;

    dataval = (unsigned short)(*val256);
    if ( dataval < _imhwd33 ) {
        datalvl = (dataval - _imdhwd32)/_imdhwd31;
    }
    else {
        datalvl = exp(( dataval - _imdhwd35)/_imdhwd34);
    }
    if ( immode == 1 ) {
        mindata = -28 ;
    }
    /*calculate 8 bit data level clode*/
    valchk = ( datalvl - 2) * scale + mindata;
    if ( immode == 1 ) {
        if ( valchk <= mincaval ) {
            *val16 = 0;
            return;
        }
    }
    else {
        if ( valchk <= minpsval ) {
            *val16 = 0;
            return;
        }
    }
    /*map 8 bit data level code to 16 colors*/
    for ( i = 1 ; i < 16 ; i ++ ) {
        if ( immode == 1 ) {
            max = caoff * ( i - 8 ) + 2.5;
            min = caoff * ( i - 8 ) - 2.0;
        }
        else {
            if ( i == 1 ) {
//                max = 5.0;
//                min = 1.0;
                max = 3.5;
                min = -1.0;
            }
            else {
                 max = psoff * (i-1) + 2.5;
                 min = psoff * (i-1) - 2.0;
//                 max = psoff * i;
//                 min = psoff * (i-1);
            }
        }
        if ( i == 1 ) {
            if ( immode == 1 ) {
                min = mincaval;
            }
            else {
                min = minpsval;
            }
        }
        if ( ( min <= valchk ) && (valchk < max)) {
            *val16 = (unsigned char )(i);
            return ;
        }
        else {
            if ( (i == 15) && (min <= valchk)) {
                *val16 = (unsigned char )(i);
                return;
            }
        }
    }
    *iret = -1;
   return ;
}

void crnexbzp135color ( unsigned char *val256, unsigned char *val16, int *iret  )
/************************************************************************
 * crnexbzp135color                                                     *
 *                                                                      *
 * This function will convert 256 colors to 16 colors for product 135   *
 *                                                                      *
 * crnexbzp135color ( val256, val16, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *     * val256       unsigned char     8-bit data level code           *
 *                                                                      *
 * Output parameters:                                                   *
 *     * val16        unsigned char     16 color codes                  *
 *      *iret           int             Return code                     *
 *                                      0 = normal return               *
 *                                     -1 = error return                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           05/10   Created                                 *
 ***********************************************************************/
{
    short 	i;
    float       max, min,valchk,datalvl;
    float       scale = 1.0, mindata=0.0;
    unsigned short  caoff = 4, psoff = 5, dataval;
    float 	mincaval = -28.0, minpsval = 0.0;
         
    *iret = 0;

    dataval = (unsigned short)(*val256);
    datalvl = ((dataval & _imdatamask ) /_imdatascale) - _imdataoffset ;
    if ( (dataval & _imtoppedmask) && (datalvl > 70.0) ) {
        datalvl = 1;
    }
    if ( immode == 1 ) {
        mindata = -28 ;
    }
    /*calculate 8 bit data level clode*/
    valchk = ( datalvl - 2)* scale + mindata;

    if ( immode == 1 ) {
        if ( valchk <= mincaval ) {
            *val16 = 0;
            return;
        }
    }
    else {
        if ( valchk <= minpsval ) {
            *val16 = 0;
            return;
        }
    }
    /*map 8 bit data level code to 16 colors*/
    for ( i = 1 ; i < 16 ; i ++ ) {
        if ( immode == 1 ) {
            max = caoff * ( i - 8 ) + 2.5;
            min = caoff * ( i - 8 ) - 2.0;
        }
        else {
            if ( i == 1 ) {
                max = 5.0;
                min = 1.0;
//                max = 2.5;
//                min = -2.0;
            }
            else {
//                max = psoff * (i-1) + 2.5;
//                min = psoff * (i-1) - 2.0;
                max = psoff * i;
                min = psoff * (i-1);
            }
        }
        if ( i == 1 ) {
            if ( immode == 1 ) {
                min = mincaval;
            }
            else {
                min = minpsval;
            }
        }
        if ( ( min <= valchk ) && (valchk < max)) {
            *val16 = (unsigned char )(i);
            return ;
        }
        else {
            if ( (i == 15) && (min <= valchk)) {
                *val16 = (unsigned char )(i);
                return;
            }
        }
    }
    *iret = -1;
   return ;
}

float crnexbz_dc ( short dataval )
/************************************************************************
 * crnexbz_dc                                                           *
 *                                                                      *
 * This function will decode digital data value to VIL for 134 product  *
 *                                                                      *
 * crnexbz_dc ( dataval)                                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      dataval       unsigned short     8-bit digital data value       *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return:								*
 *                    float              Digital data value             *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           05/10   	Created                         *
 ***********************************************************************/
{
    float dval;
    unsigned short mask = 0x8000, i;
    short s=1 ;
    float ee = 0.0, ff = 0.0;

   if ( dataval & mask ) {
        s = -1;
    }
    mask  = mask >> 1;
    for ( i = 1 ;i <=5 ; i ++ )
    {
        if ( dataval & mask ) {
            ee = ee + pow (2, 5 - i);
        }
        mask = mask >> 1;
    }
    for ( i = 1 ; i <= 10 ; i ++ ) {
        if ( dataval & mask ) {
            ff = ff + pow (2 , 10 - i );
        } 
        mask = (mask >> 1);
    }
    if ( ee <= 0.0 ) {
        dval = s * (ff/512);
    }
    else {
        dval = s * pow (2, ee-16)*(1 + (ff/1024));
    }
    return dval;
}
