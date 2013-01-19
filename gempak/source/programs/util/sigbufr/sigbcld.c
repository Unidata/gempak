#include <mel_bufr.h>

#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h" 

#include "cascmn.h"
#include "sigbcmn.h"

void sigbcld ( FILE *ifpout, int *iret )
/************************************************************************
 * sigbcld                                                              *
 *                                                                      *
 * This function loops through the high level cloud areas in a sigwx 	*
 * bufr file and  writes them to an ASCII file.                         *
 *                                                                      *
 * sigbcld ( ifpout, iret )                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout         FILE            Output file name pointer        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC		12/04	Changed prologue only			*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         n, descrip;
    int         numclds, mode, levs, ier;
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    Boolean     memalc;

    BUFR_Val_t  bv;
    cloud_t     *cld, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    numclds = 0;
    mode    = 1;
    levs    = 0;
    memalc  = FALSE;
    cld     = NULL;
    head    = NULL;
    leverr  = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /*
    * Get each value (and any associated fields) from the BUFR message. 
    */

    while ( (n=(int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM 
             && n != BUFR_EOF ) {
        if ( n == BUFR_ERROR ) {
            sigberror ( "sigbcld", &ier );
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
           /*
            * This is a CLOUD message.
            */             
            if ( descrip == D_MET_FEATURE ) {
                if ( mode == 0 ) {
                    mode = 1;
                }
                else if ( mode == 1 ) {
                    mode = 0;
                }
            }
            else if ( descrip == D_DIMEN_ATTRIB ) {
                if ( mode == 1 ) {
                    mode   = 2;
                    new    = ( cloud_t *) malloc (sizeof ( cloud_t ) );
                    memalc = TRUE;
                    new -> next = NULL;
                    if ( numclds != 0 ) {
	                cld -> next = new;
                        cld         = new;
                    }
                    else {
                        cld  = new;
                        head = cld;
                    }
                    numclds++;
                    levs        = 1;
                    cld -> npt  = 0;
                }
                else if ( mode == 2 ) {
                    mode = 1;
                }
            }
            else if ( mode == 2 ) {

	       /*
	        * Store cloud information into current linked list.
	        */

                if ( descrip == D_ALTITUDE ) {
                     if ( levs == 1 ) {
                         if ( bv.missing_flag != 0 ) {
                             cld -> level1 = (float)bv.Val.int_number;
                         }
                         else {
                             cld -> level1 = SIGRLMS;
                         }
                         levs   = 2;
                     }
                     else if ( levs == 2 ) {
                         cld -> level2 = (float)bv.Val.int_number;
                     }
                }
                else if ( descrip == D_LAT ) {
                     cld -> lat [cld -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_LON ) {
                    cld -> lon [cld -> npt] = (float)bv.Val.number;
                    cld -> npt++;
                }
                else if ( descrip == D_CLOUD_AMT ) {
                    cld -> clddist = bv.Val.int_number;
                }
                else if ( descrip == D_CLOUD_TYPE ) {
                    cld -> cldtyp  = bv.Val.int_number;
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

    if ( n == BUFR_EOM ) {
        sigbeom ( &n, &ier );
    }

   /* 
    * Write out CLOUD information in BUFR ASCII format.
    */

    if ( numclds > 0 ) { 
        ptr = head;
        cas_wrcld ( ifpout, ptr, numclds, &ier );
    }

    sigbeof ( n, &ier );
    *iret = BUFR_EOF;

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numclds > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }
 
    BUFR_Destroy(1);

}
