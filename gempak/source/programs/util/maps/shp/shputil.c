#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

/************************************************************************
 * shputil.c                                                            *
 *                                                                      *
 * This module contains the utility functions that deal with platform	*
 * endian. They are searched from internet.				*
 *                                                                      *
 * CONTENTS:                                                            *
 *      shp_get_llong()  changes little endian long to host long.       *
 *      shp_get_lshort() changes little endian short to host short.     *
 *      shp_get_blong()  changes big endian long to host long.       	*
 *      shp_get_bshort() changes big endian short to host short.     	*
 *	shp_get_point()	 get one point value.				*
 ***********************************************************************/

/*=====================================================================*/

long shp_get_llong ( unsigned char *cp )
/************************************************************************
 * shp_get_llong                                                        *
 *                                                                      *
 * This function changes little endian long to host long.               *
 *                                                                      *
 * long shp_get_llong ( cp )                                            *
 *                                                                      *
 * Input parameters:                                                    *
 *      *cp             unsigned char   Little endian long              *
 *                                                                      *
 * Output parameters:                                                   *
 *      shp_get_llong    long           Host long                       *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    long ret;
/*---------------------------------------------------------------------*/
    ret = *cp++;
    ret += ((*cp++)<<8);
    ret += ((*cp++)<<16);
    ret += ((*cp++)<<24);

    return ret;
}

short shp_get_lshort ( unsigned char *cp )
/************************************************************************
 * shp_get_lshort                                                       *
 *                                                                      *
 * This function changes little endian short to host short.             *
 *                                                                      *
 * short shp_get_lshort ( cp )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *cp             unsigned char   Little endian short             *
 *                                                                      *
 * Output parameters:                                                   *
 *      shp_get_lshort   short          Host short                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    short ret;
/*---------------------------------------------------------------------*/
    ret = *cp++;
    ret += ((*cp++)<<8);

    return ret;
}

long shp_get_blong ( unsigned char *cp )
/************************************************************************
 * shp_get_blong                                                        *
 *                                                                      *
 * This function changes big endian long to host long.                  *
 *                                                                      *
 * long shp_get_blong ( cp )                                            *
 *                                                                      *
 * Input parameters:                                                    *
 *      *cp             unsigned char   Big endian long                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      shp_get_blong    long           Host long                       *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    long ret;
/*---------------------------------------------------------------------*/

    ret = *(cp+3);
    ret += ((*(cp+2))<<8);
    ret += ((*(cp+1))<<16);
    ret += ((*cp)<<24);

    return ret;
}

short shp_get_bshort ( unsigned char *cp )
/************************************************************************
 * shp_get_bshort                                                       *
 *                                                                      *
 * This function changes big endian short to host short.                *
 *                                                                      *
 * short shp_get_bshort ( cp )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *cp             unsigned char   Big endian short                *
 *                                                                      *
 * Output parameters:                                                   *
 *      shp_get_bshort   short          Host short                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    short ret;
/*---------------------------------------------------------------------*/
    ret = *(cp+1);
    ret += ((*cp)<<8);

    return ret;
}

static void swap8 ( unsigned char *cp )
/************************************************************************
 * swap8                                                                *
 *                                                                      *
 * This function swaps 8 bytes.                                         *
 *                                                                      *
 * swap8 ( cp ) 				                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *cp             unsigned char   Buffer                          *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    unsigned char ch;
    int ichar;
/*---------------------------------------------------------------------*/

    for ( ichar = 0; ichar < 4; ichar++ ) {
        ch = cp[ichar];
	cp[ichar] = cp[7-ichar];
	cp[7-ichar] = ch;
    }
}

void shp_get_point ( unsigned char *buffer, float *ptx, float *pty, 
                     int *iret )
/************************************************************************
 * shp_get_point                                                        *
 *                                                                      *
 * This function gets one point value from the buffer.                  *
 *                                                                      *
 * shp_get_point ( buffer, ptx, pty, iret )    	                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *buffer         unsigned char   Point buffer                    *
 *                                                                      *
 * Output parameters:                                                   *
 *	*ptx		float		Point x value			*
 *	*pty		float		Point y value			*
 *	*iret		int		Return code			*
 *					  0 = Normal			*
 *					 -1 = Error			*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03	Initial coding				*
 ***********************************************************************/
{
    unsigned char tmpbuf[POINT_SIZE/2];
/*---------------------------------------------------------------------*/
    *iret = 0;

    memcpy ( tmpbuf, buffer, POINT_SIZE/2 );
    if ( mch_endian == BIG ) {
        swap8 ( tmpbuf );
    }
    *ptx = (float)(*((double *)tmpbuf));

    memcpy ( tmpbuf, &buffer[POINT_SIZE/2], POINT_SIZE/2 );
    if ( mch_endian == BIG ) {
        swap8 ( tmpbuf );
    }
    *pty = (float)(*((double *)tmpbuf));
}
