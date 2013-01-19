#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbvolcano ( FILE *ifpout, FILE *ifpoutm, int *iret )
/************************************************************************
 * sigbvolcano                                                          *
 *                                                                      *
 * This function loops through the volcanoes in a sigwx bufr file and   *
 * writes them to an ASCII file.                                        *
 *                                                                      *
 * sigbvolcano ( ifpout, ifpoutm, iret )                                *
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
 * M. Li/SAIC           12/04   Added ifpoutm                           *
 * K. Tyle/UAlbany      11/10   Increased dimension of errgrp           *
 ***********************************************************************/
{
    int         n, descrip, nflg;
    int         numvolcanos, numstm, numrd, mode, time_code, ij, ier;
    int         idtarr[5];
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    char        symtyp[8];
    Boolean     memalc;

    BUFR_Val_t  bv;
    volrad_t    *vol, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret       = 0;
    nflg        = 0;
    n           = 0;
    numvolcanos = 0;
    numstm      = 0;
    numrd       = 0;
    mode        = 1;
    time_code   = 0;
    for ( ij = 0; ij < 5; ij++ ) {
        idtarr[ij] = SIGIMSS;
    }
    strcpy ( symtyp, "VOLCANO" );
    memalc      = FALSE;
    vol         = NULL;
    head        = NULL;
    leverr      = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /* 
    *Get each value (and any associated fields) from the BUFR message.
    */

    while ( nflg == 0 ) {
        n = (int)BUFR_Get_Value( &bv, 1 );
        if ( n != BUFR_EOM && n != BUFR_EOF ) {
            if ( n == BUFR_ERROR ) {
                sigberror ( "sigbvolcano", &ier );
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
                if ( descrip == D_MET_FEATURE ) {
                   /*
                    * This is a V_T_S/Volcano message.
                    */             
                    if ( mode == 0 ) {
                        mode = 1;
                    }
                    else if ( mode == 1 ) {
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
                else if ( descrip == D_FACILITY ) {
                   /*
                    * This is a V_T_S/Radiation Leak message.
                    */             
                    numrd = 1;
                    nflg  = -1;
                }
                else if ( descrip == D_FEATURE_NAME ) {
                    new    = ( volrad_t *) malloc (sizeof ( volrad_t ) );
                    memalc = TRUE;
                    new -> next = NULL;
                    if ( numvolcanos != 0 ) {
                        vol -> next = new;
                        vol         = new;
                    }
                    else {
                        vol  = new;
                        head = vol;
                    }
                    strcpy ( vol -> name, bv.Val.string );
                    vol -> name[39] = CHNULL;
                }
                else if ( descrip == D_DIMEN_ATTRIB ) {
                    if ( mode == 1 ) {
                        mode = 2;
                        numvolcanos++;
                        time_code = 0;
                    }
                    else if ( mode == 2 ) {
                        mode = 1;
                    }
                }
                else if ( mode == 2 ) {

                   /*
                    * Store volcano information into current linked list.
                    */

                    if ( descrip == D_LAT ) {
                        vol -> lat = (float)bv.Val.number;
                    }
                    else if ( descrip == D_LON ) {
                        vol -> lon = (float)bv.Val.number;
                    }
                    else if ( descrip == D_TIME_SIGNIF ) {
                        if ( bv.Val.int_number == 17 ) {
                            time_code = 1;
                        }
                        else {
                            vol -> year   = idtarr[0];
                            vol -> month  = idtarr[1];
                            vol -> day    = idtarr[2];
                            vol -> hour   = idtarr[3];
                            vol -> minute = idtarr[4];
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
    * Write out V_T_S/volcano information in BUFR ASCII format.
    */

    if ( numvolcanos > 0 ) {
        ptr = head;
        cas_wrvlrd ( ifpout, ptr, numvolcanos, symtyp, &ier );
    }

   /*
    * Write out mid level V_T_S/volcano information in BUFR ASCII format.
    */

    if ( numvolcanos > 0 ) {
        ptr = head;
        cas_wrvlrd ( ifpoutm, ptr, numvolcanos, symtyp, &ier );
    }


    if( n == BUFR_EOF ) {
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
        if ( numvolcanos > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }

    if ( numstm == 1 ) {
        sigbstorm ( ifpout, ifpoutm, &ier );
        *iret = ier;
    }
    else if ( numrd == 1 ) {
        sigbradtn ( ifpout, ifpoutm, &ier );
        *iret = ier;
    }
    else {
        BUFR_Destroy(1);
    }

}
