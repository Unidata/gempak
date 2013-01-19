#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbradtn ( FILE *ifpout, FILE *ifpoutm, int *iret )
/************************************************************************
 * sigbradtn                                                            *
 *                                                                      *
 * This function loops through the radiation leaks in a sigwx bufr file *
 * and writes them to an ASCII file.                                    *
 *                                                                      *
 * sigbradtn ( ifpout, ifpoutm iret )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout         FILE            Output file name pointer        *
 *      *ifpoutm        FILE            Output file name pointer(mid)   * 
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC		12/04	Added ifpoutm				*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         n, descrip, nflg;
    int         numrad, numstm, numvol, mode, time_code, ij, ier;
    int         idtarr[5];
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    char        symtyp[10];
    Boolean     memalc;

    BUFR_Val_t  bv;
    volrad_t    *rad, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret     = 0;
    nflg      = 0;
    n         = 0;
    numrad    = 0;
    numstm    = 0;
    numvol    = 0;
    mode      = 1;
    time_code = 0;
    for ( ij = 0; ij < 5; ij++ ) {
        idtarr[ij] = SIGIMSS;
    }
    strcpy ( symtyp, "RADIATION" );
    memalc    = FALSE;
    rad       = NULL;
    head      = NULL;
    leverr    = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /* 
    *Get each value (and any associated fields) from the BUFR message.
    */

    while ( nflg == 0 ) {
        n = (int)BUFR_Get_Value( &bv, 1 );
        if ( n != BUFR_EOM && n != BUFR_EOF ) {
            if ( n == BUFR_ERROR ) {
                sigberror ( "sigbradtn", &ier );
                *iret = BUFR_EOF;
                return;
            }

           /*
            * Test for IGNORE_FXY, used to indicate that information that
            * was returned should be ignored.  This happens when a local
            * descriptor that is not defined in Table B is the last
            * descriptor in the data set.
            */

            if ( bv.FXY_Val != (FXY_t)IGNORE_FXY ) {
                descrip = FXY_Unpack_Dec (bv.FXY_Val);
                if ( descrip == D_FACILITY ) {
                   /*
                    * This is a V_T_S/Radiation Leak message.
                    */             
                    if ( mode == 0 ) {
                        mode = 1;
                    }
                    else {
                        mode = 0;
                    }
                }
                else if ( descrip == D_MET_ATTRIB ) {
                   /*
                    * This is a V_T_S/Storm message.
                    */             
                    numstm = 1;
                    nflg   = -1;
                }
                else if ( descrip == D_MET_FEATURE ) {
                   /*
                    * This is a V_T_S/Volcano message.
                    */             
                    numvol = 1;
                    nflg   = -1;
                }
                else if ( descrip == D_DIMEN_ATTRIB ) {
                    if ( mode == 1 ) {
                        mode   = 2;
                        new = ( volrad_t *) malloc (sizeof ( volrad_t ));
                        memalc = TRUE;
                        new -> next = NULL;
                        if ( numrad != 0 ) {
                            rad -> next = new;
                            rad         = new;
                        }
                        else {
                            rad  = new;
                            head = rad;
                        }
                        numrad++;
                        time_code = 0;
                    }
                    else if ( mode == 2 ) {
                        mode = 0;
                    }
                }
                else if ( mode == 2 ) {

                   /*
                    * Store radiation leak information into current
                    * linked list.
                    */

                    if ( descrip == D_FEATURE_NAME ) {
                        strcpy ( rad -> name, bv.Val.string );
                        rad -> name[39] = CHNULL;
                    }
                    else if ( descrip == D_LAT ) {
                        rad -> lat = (float)bv.Val.number;
                    }
                    else if ( descrip == D_LON ) {
                        rad -> lon = (float)bv.Val.number;
                    }
                    else if ( descrip == D_TIME_SIGNIF ) {
                        if ( bv.Val.int_number == 17 ) {
                            time_code = 1;
                        }
                        else {
                            rad -> year   = idtarr[0];
                            rad -> month  = idtarr[1];
                            rad -> day    = idtarr[2];
                            rad  -> hour   = idtarr[3];
                            rad -> minute = idtarr[4];
                            time_code = 0;
                        }
                    }
                    else if ( descrip >= D_YEAR 
                           && descrip <= D_MINUTE ) {
                        if ( time_code == 1 ) {
                            if ( bv.missing_flag != 0 ) {
                                idtarr [FXY_Y_Value (bv.FXY_Val)-1] = 
                                        bv.Val.int_number;
                            }
                            else {
                                idtarr [FXY_Y_Value (bv.FXY_Val)-1] = 
                                        SIGIMSS;
                            }
                        }
                    }
                }
             /* BUFR_Val_Print ( bv, 1, 0, stdout ); */
            }
            if ( n == BUFR_EOD ) {
               /* 
                * This decoded value is the last one in the dataset.
                */
                numerr = -10;
                er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                          strlen(errgrp), strlen(cc) );
            }
        }
        else {
            nflg = -1;
        }
    }

    if ( n == BUFR_EOM ) {
        sigbeom ( &n, &ier );
    }

   /*
    * Write out V_T_S/radiation leak information in BUFR ASCII format.
    */

    if ( numrad > 0 ) {
        ptr = head;
        cas_wrvlrd ( ifpout, ptr, numrad, symtyp, &ier );
    }

   /*
    * Write out mid level V_T_S/radiation leak information in BUFR ASCII format.
    */

    if ( numrad > 0 ) {
        ptr = head;
        cas_wrvlrd ( ifpoutm, ptr, numrad, symtyp, &ier );
    }


    if ( n == BUFR_EOF ) {
        numerr = -13;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        *iret = n;
    }
    BUFR_Err_Print( NULL );

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numrad > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }

    if ( numstm == 1 ) {  
        sigbstorm ( ifpout, ifpoutm, &ier);
        *iret = ier;
    }
    else if ( numvol == 1 ) {  
        sigbvolcano ( ifpout, ifpoutm, &ier);
        *iret = ier;
    }
    else {
        BUFR_Destroy(1);
    }

}
