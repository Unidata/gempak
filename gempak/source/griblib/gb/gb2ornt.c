#include "gb2def.h"

void gb2_ornt ( int kx, int ky, int scan_mode, float *ingrid, 
                float *fgrid, int *iret )
/************************************************************************
 * gb2_ornt                                                             *
 *                                                                      *
 * This function checks the fields scanning mode flags and re-orders    *
 * the grid point values so that they traverse left to right for each   *
 * row starting at the bottom row.                                      *
 *                                                                      *
 * gb2_ornt ( kx, ky, scan_mode, ingrid, fgrid, iret )			*
 *                                                                      *
 * Input parameters:							*
 *	kx		int		Number of columns		*
 *	ky		int		Number of rows			*
 *      scan_mode       int             GRIB2 scanning mode flag        *
 *      *ingrid         float           unpacked GRIB2 grid data        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *fgrid	float	Unpacked grid data                              *
 *  *iret	int		Return code                             *
 *                       -40 = scan mode not implemented                *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert            1/04                                           *
 ***********************************************************************/
{

    int ibeg, jbeg, iinc, jinc, itmp;
    int icnt, jcnt, kcnt, idxarr;

    int idrct, jdrct, consec, boustr;


/*---------------------------------------------------------------------*/

    *iret = 0;
    idrct  = ( scan_mode >> 7 ) & 1;
    jdrct  = ( scan_mode >> 6 ) & 1;
    consec = ( scan_mode >> 5 ) & 1;
    boustr = ( scan_mode >> 4 ) & 1;

    if ( idrct == 0 ) {
        ibeg = 0;
        iinc = 1;
    }
    else {
        ibeg = kx - 1;
        iinc = -1;
    }
    if ( jdrct == 1 ) {
        jbeg = 0;
        jinc = 1;
    }
    else {
        jbeg = ky - 1;
        jinc = -1;
    }

    kcnt = 0;
    if ( consec == 1 && boustr == 0 ) {
        /*  adjacent points in same column;  each column same direction  */
        for ( jcnt=jbeg; (0<=jcnt&&jcnt<ky); jcnt+=jinc ) {
            for ( icnt=ibeg; (0<=icnt&&icnt<kx); icnt+=iinc ) {
               idxarr = ky * icnt + jcnt;
               fgrid[kcnt] = ingrid[idxarr];
               kcnt++;
            }
        }
    }
    else if( consec == 0 && boustr == 0 ) {
        /*  adjacent points in same row;  each row same direction  */
        for ( jcnt=jbeg; (0<=jcnt&&jcnt<ky); jcnt+=jinc ) {
            for ( icnt=ibeg; (0<=icnt&&icnt<kx); icnt+=iinc ) {
               idxarr = kx * jcnt + icnt;
               fgrid[kcnt] = ingrid[idxarr];
               kcnt++;
            }
        }
    }
    else if ( consec == 1 && boustr == 1 ) {
        /*  adjacent points in same column; each column alternates direction */
        for ( jcnt=jbeg; (0<=jcnt&&jcnt<ky); jcnt+=jinc ) {
            itmp=jcnt;
            if (idrct == 1 && kx%2 == 0 ) itmp = ky - jcnt - 1;
            for ( icnt=ibeg; (0<=icnt&&icnt<kx); icnt+=iinc ) {
               idxarr = ky * icnt + itmp;
               fgrid[kcnt] = ingrid[idxarr];
               itmp = ( itmp != jcnt ) ? jcnt : ky - jcnt - 1;  /* toggle */
               kcnt++;
            }
        }
    }
    else if( consec == 0 && boustr == 1 ) {
        /*  adjacent points in same row;  each row alternates direction  */
        if ( jdrct == 0 ) {
           if (idrct == 0 && ky%2 == 0) {
              ibeg = kx - 1;
              iinc = -1;
           }
           if (idrct == 1 && ky%2 == 0) {
              ibeg = 0;
              iinc = 1;
           }
        }
        for ( jcnt=jbeg; (0<=jcnt&&jcnt<ky); jcnt+=jinc ) {
            for ( icnt=ibeg; (0<=icnt&&icnt<kx); icnt+=iinc ) {
               idxarr = kx * jcnt + icnt;
               fgrid[kcnt] = ingrid[idxarr];
               kcnt++;
            }
            ibeg = ( ibeg != 0 ) ? 0 : kx - 1;         /* toggle */
            iinc = ( iinc != 1 ) ? 1 : - 1;         /* toggle */
        }
    }
    else {
        *iret=-40;
    }

}
