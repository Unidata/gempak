#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbfronts ( FILE *ifpout, int type, char *chlvl, int *iret )
/************************************************************************
 * sigbfronts                                                           *
 *                                                                      *
 * This function loops through the fronts in a sigwx bufr file and      *
 * writes them to an ASCII file.                                        *
 *                                                                      *
 * sigbfronts ( ifpout, type, chlvl, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout         FILE            Output file name pointer        *
 *      type            int             Type of front                   *
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * A. Hardy/NCEP	 4/03		Changed direction to int value  *
 * M. Li/SAIC            9/04   	Added chlvl                     *
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC		12/04		Added input parameter chlvl	*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int 	n, descrip;
    int  	numfronts, mode, typef, ier;
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    Boolean     memalc;

    BUFR_Val_t  bv;
    front_t     *fnt, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret     = 0;
    numfronts = 0;
    mode      = 1;
    typef     = type;
    memalc    = FALSE;
    fnt       = NULL;
    head      = NULL;
    leverr    = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /*
    * Get each value (and any associated fields) from the BUFR message.
    */

    while ( (n=(int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM 
            && n != BUFR_EOF ) {
        if ( n == BUFR_ERROR ) {
            sigberror ( "sigbfronts", &ier );
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
            * This is a FRONTS message.
            */             
            if ( descrip == D_MET_FEATURE ) {
                if ( mode == 0 ) {
                    mode  = 1;
                    typef = bv.Val.int_number;
                }
                else if ( mode == 1 ) {
                    mode = 0;
                }
            }
            else if ( descrip == D_DIMEN_ATTRIB ) {
                if ( mode == 1 ) {
                    mode   = 2;
                    new    = ( front_t *) malloc (sizeof ( front_t ) );
                    memalc = TRUE;
                    new -> next = NULL;
                    if ( numfronts != 0 ) {
                        fnt -> next = new;
                        fnt         = new;
                    }
                    else {
                        fnt  = new;
                        head = fnt;
                    }

                    numfronts++;
                    fnt -> npt = 0;
                }
                else if ( mode == 2 ) {
                    mode = 1;
                }
            }
            else if ( mode == 2 ) {

               /*
                * Store fronts information into current linked list.
                */

                if ( descrip == D_LAT ) {
                    fnt -> ftype = typef;
                    fnt -> lat [fnt -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_LON ) {
                    fnt -> lon [fnt -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_DIR_FEATURE ) {
                    if ( bv.missing_flag != 0 ) {
                        fnt -> fntdir [fnt -> npt] = (float)bv.Val.int_number;
                    }
                    else {
                        fnt -> fntdir [fnt -> npt] = SIGRLMS;
                    }
                }
                else if ( descrip == D_SPD_FEATURE ) {
                    if ( bv.missing_flag != 0 ) {
                        fnt -> fntspd [fnt -> npt] = (float)bv.Val.number;
                    }
                    else {
                        fnt -> fntspd [fnt -> npt] = SIGRLMS;
                    }
                    fnt -> npt++;
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
    * Write out FRONTS information in BUFR ASCII format.
    */

    if ( numfronts > 0 ) {
        ptr = head;
        cas_wrfrt ( ifpout, ptr, numfronts, chlvl, &ier );
    }

    sigbeof ( n, &ier );
    *iret = BUFR_EOF;

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numfronts > 1 ) {
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
