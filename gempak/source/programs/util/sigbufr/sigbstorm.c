#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbstorm ( FILE *ifpout, FILE *ifpoutm, int *iret )
/************************************************************************
 * sigbstorm                                                            *
 *                                                                      *
 * This function loops through the storms in a sigwx bufr file and      *
 * writes them to an ASCII file.                                        *
 *                                                                      *
 * sigbstorm ( ifpout, ifpoutm, iret )                                  *
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
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         n, descrip, nflg;
    int         numstorms, numvol, numrd, mode, ier;
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    char        symtyp[6];
    Boolean     memalc;

    BUFR_Val_t  bv;
    storm_t     *stm, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret     = 0;
    nflg      = 0;
    n         = 0;
    numstorms = 0;
    numvol    = 0;
    numrd     = 0;
    mode      = 1;
    strcpy ( symtyp, "STORM" );
    memalc    = FALSE;
    stm       = NULL;
    head      = NULL;
    leverr    = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /* 
    * Get each value (and any associated fields) from the BUFR message.
    */

    while ( nflg == 0 ) {
        n = (int)BUFR_Get_Value( &bv, 1 );
        if ( n != BUFR_EOM && n != BUFR_EOF ) {
            if ( n == BUFR_ERROR ) {
                sigberror ( "sigbstorm", &ier );
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

                if ( descrip == D_MET_ATTRIB ) {
                   /*
                    * This is a V_T_S/Storm message.
                    */             
                    if ( mode == 0 ) {
                        mode = 1;
                    }
                    else if ( mode == 1 ) {
                        mode = 0;
                    }
                }
                else if ( descrip == D_MET_FEATURE ) { 
                   /*
                    * This is a V_T_S/Volcano message.
                    */             
                    numvol = 1;
                    nflg   = -1;
                }
                else if ( descrip == D_FACILITY ) { 
                   /*
                    * This is a V_T_S/Radiation Leak message.
                    */             
                    numrd  = 1;
                    nflg   = -1;
                }
                else if ( descrip == D_DIMEN_ATTRIB ) {
                    if ( mode == 1 ) {
                        new  = ( storm_t *) malloc (sizeof ( storm_t ) );
                        memalc = TRUE;
                        new -> next = NULL;
                        if ( numstorms != 0 ) {
                            stm -> next = new;
                            stm         = new;
                        }
                        else {
                            stm  = new;
                            head = stm;
                        }
                        mode = 2;
                        numstorms++;
                    }
                    else if ( mode == 2 ) {
                        mode = 1;
                    }
                }
                else if ( mode == 2 ) {

                   /*
                    * Store storm information into current linked list.
                    */

                    if ( descrip == D_STORM_NAME ) {
                        strcpy ( stm -> name, bv.Val.string );
                        stm -> name[39] = CHNULL;
                    }
                    else if ( descrip == D_LAT ) {
                        stm -> lat = (float)bv.Val.number;
                    }
                    else if ( descrip == D_LON ) {
                        stm -> lon = (float)bv.Val.number;
                    }
                    else if ( descrip == D_TYPE_FEATURE ) {
                        stm -> stmtyp = bv.Val.int_number;        
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

    if( n == BUFR_EOM ) {
        sigbeom ( &n, &ier );
    }

   /*
    * Write out V_T_S/storm information in BUFR ASCII format.
    */

    if ( numstorms > 0 ) {
        ptr = head;
        cas_wrstm ( ifpout, ptr, numstorms, symtyp, &ier );
    }

   /*
    * Write out mid level V_T_S/storm information in BUFR ASCII format.
    */

    if ( numstorms > 0 ) {
        ptr = head;
        cas_wrstm ( ifpoutm, ptr, numstorms, symtyp, &ier );
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
       /*
        * Without the following if test, it seg faults when 
        * numstorms = 1.  Find out if this is the RIGHT way to 
        * fix this bug, and put fix in other feature routines.
        */
        if ( numstorms > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }

    if ( numvol == 1 ) {
        sigbvolcano ( ifpout, ifpoutm, &ier );
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
