#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbtrop ( FILE *ifpout, char *chlvl, int *iret )
/************************************************************************
 * sigbtrop                                                             *
 *                                                                      *
 * This function loops through the trop groups in a sigwx bufr file and *
 * writes them to an ASCII file.                                        *
 *                                                                      *
 * sigbtrop ( ifpout, chlvl, iret )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout         FILE            Output file name pointer        *
 *      *chlvl          char            Chart level                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * M. Li/SAIC		 9/04		Added chlvl			*
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC           12/04           Added input parameter chlvl     *
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         n, descrip;
    int         numspot, numhi, numlo, mode, signif, ier;
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    Boolean     memspot, memhi, memlo;

    BUFR_Val_t  bv;
    trop_t      *trop, *head, *new, *ptr, *ptr2;
    trophi_t    *trophi, *headh, *newh, *ptrh, *ptr2h;
    troplo_t    *troplo, *headl, *newl, *ptrl, *ptr2l;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    numspot = 0;
    numhi   = 0;
    numlo   = 0;
    mode    = 1;
    signif  = 0;
    memspot = FALSE;
    memhi   = FALSE;
    memlo   = FALSE;
    trop    = NULL;
    trophi  = NULL;
    troplo  = NULL;
    head    = NULL;
    headh   = NULL;
    headl   = NULL;
    leverr  = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /* 
    *Get each value (and any associated fields) from the BUFR message.
    */

    while ( (n=(int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM 
            && n != BUFR_EOF ) {
        if ( n == BUFR_ERROR ) {
            sigberror ( "sigbtrop", &ier );
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
            * This is a TROP message.
            */             
            if ( descrip == D_VERT_SIGNIF ) {
                if ( mode == 0 ) {
                    if  ( ( bv.missing_flag != 0 ) &&
                          ( bv.Val.int_number & ( 1 << 4 ) ) ) {
                        mode = 1;
                    }
                }
                else if ( mode == 1 ) {
                    mode = 0;
                }
            }
            else if ( descrip == D_DIMEN_ATTRIB ) {
                if ( mode == 1 ) {
                    mode = 2;
                }
                else if ( mode == 2 ) {
                    mode = 1;
                }
            }
            else if ( descrip == D_STATISTIC ) {
                if ( mode == 2 ) {
                    mode   = 3;
                    signif = bv.Val.int_number;
                    if ( bv.missing_flag == 0 ) signif = SIGIMSS;
                }
                else if ( mode == 3 ) {
                    mode = 2;     
                }
            }
            else if ( mode == 3 ) {

               /*
                * Store trop information into correct linked list.
                */

                if ( descrip == D_LAT ) {
                    if ( signif == 2 ) {
                        newh = ( trophi_t *) malloc (sizeof( trophi_t ));
                        memhi = TRUE;
                        newh -> next = NULL;
                        if ( numhi != 0 ) {
                            trophi -> next = newh;
                            trophi         = newh;
                        }
                        else {
                            trophi = newh;
                            headh  = trophi;
                        }
                        numhi++;
                        trophi -> lat = (float)bv.Val.number;
                    }
                    else if ( signif == 3 ) {
                        newl = ( troplo_t *) malloc (sizeof( troplo_t ));
                        memlo = TRUE;
                        newl -> next = NULL;
                        if ( numlo != 0 ) {
                            troplo -> next = newl;
                            troplo         = newl;
                        }
                        else {
                            troplo = newl;
                            headl  = troplo;
                        }
                        numlo++;
                        troplo -> lat = (float)bv.Val.number;
                    }
                    else if ( signif == SIGIMSS ) {
                        new    = ( trop_t *) malloc (sizeof ( trop_t ) );
                        memspot = TRUE;
                        new -> next = NULL;
                        if ( numspot != 0 ) {
                            trop -> next = new;
                            trop         = new;
                        }
                        else {
                            trop = new;
                            head = trop;
                        }
                        numspot++;
                        trop -> lat = (float)bv.Val.number;
                    }
                }
                else if ( descrip == D_LON ) {
                    if ( signif == 2 ) {
                        trophi -> lon = (float)bv.Val.number;
                    }
                    else if ( signif == 3 ) {
                        troplo -> lon = (float)bv.Val.number;
                    }
                    else if ( signif == SIGIMSS ) {
                        trop -> lon = (float)bv.Val.number;
                    }
                }
                else if ( descrip == D_HEIGHT ) {
                    if ( signif == 2 ) {
                        trophi -> level = (float)bv.Val.int_number;
                    }
                    else if ( signif == 3 ) {
                        troplo -> level = (float)bv.Val.int_number;
                    }
                    else if ( signif == SIGIMSS ) {
                        trop -> level = (float)bv.Val.int_number;
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

    if ( n == BUFR_EOM ) {
        sigbeom ( &n, &ier );
    }

   /*
    * Write out TROP information in BUFR ASCII format.
    */

    if ( numspot + numhi + numlo > 0 ) {
        ptr  = head;
        ptrh = headh;
        ptrl = headl;
        cas_wrtrop ( ifpout, ptr, numspot, ptrh, numhi, ptrl, numlo, 
                     chlvl, &ier );
    }

    sigbeof ( n, &ier );
    *iret = BUFR_EOF;

   /*
    * Free memory from the linked lists.
    */

    if ( memspot ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numspot > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }

    if ( memhi ) {
        ptrh  = headh;
        ptr2h = ptrh -> next;
        if ( numhi > 1 ) {
            while ( ptr2h -> next != NULL ) {
                ptrh  = ptr2h;
                ptr2h = ptr2h -> next;
                free (ptrh);
            }
        }
        free ( ptr2h );
    }

    if ( memlo ) {
        ptrl  = headl;
        ptr2l = ptrl -> next;
        if ( numlo > 1 ) {
            while ( ptr2l -> next != NULL ) {
                ptrl  = ptr2l;
                ptr2l = ptr2l -> next;
                free (ptrl);
            }
        }
        free ( ptr2l );
    }

    BUFR_Destroy(1);

}
