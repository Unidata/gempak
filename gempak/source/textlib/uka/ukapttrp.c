#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  TEXT_BOX       4
#define  TEXT_HIGH      2
#define  TEXT_LOW       1

void uka_pttrp ( FILE *ifpout, char *fname, FILE *fptr, long size,
	        char gptyp, char *gtstr, char *chlvl, int *iret )
/************************************************************************
 * uka_pttrp								*
 *                                                                      *
 * This function scans a VG file for elements related to tropopause box *
 * text types and then prints out all that match to an ASCII file.	*
 *                                                                      *
 * uka_pttrp ( ifpout, fname, fptr, size, gptyp, gtstr, chlvl, iret )  	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input VG file name		*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"TROP" type number		*
 *      *gtstr		char		"TROP" type name		*
 * 	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					=  0 Normal return		*
 *					= -2 Group type not found	*
 *					=  9 Missing text boxes 	*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC         2/02   Fixed freeing memory error		*
 * A. Hardy/SAIC         5/02   Corrected high/low box types		*
 * A. Hardy/NCEP	 6/02   Added sig_grpchk			*
 * A. Hardy/NCEP	 7/02   Added check to verify text is numeric   *
 * A. Hardy/NCEP	 4/03   Changed strln ' ' -> " " in error check *
 * M. Li/SAIC		 5/04	Copied from sig_trop			*
 * M. Li/SAIC		 9/04	Added chlvl				*
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level; 
    int		ii, ingrp, ier, ier1, type;
    int	 	numbx, numhi, numlo, numtrp;
    float       trplvl;
    char	vgclss, vgnum, grp[4], sggrp[12], strln[256];
    Boolean	membx, memhi, memlo, process;

    trop_t	*trop=NULL, *head, *new, *ptr, *ptr2;
    trophi_t	*trophi=NULL, *headh, *newh, *ptrh, *ptr2h;
    troplo_t	*troplo=NULL, *headl, *newl, *ptrl, *ptr2l;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    istoff  = 0;
    numtrp  = 0;
    numbx   = 0;
    numhi   = 0;
    numlo   = 0;
    level   = 0;
    membx   = False;
    memhi   = False;
    memlo   = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_TEXT;
    vgnum  = SPTX_ELM;
    sbtyp  = TEXT_BOX;

   /*
    * Read an element header to find the group type number.
    */


        cvg_rdgtn ( fname, fptr, &size, istoff, 0, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0, continue.
        */

        if ( ( ieloff != IMISSD ) && ( gpnum == 0 ) && ( ier == 0 ) ) {

            cvg_srchgrp ( fname, fptr, &size, 0, 0, MAXOFF, members, 
	                  &ingrp, &ier);

            uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	                 &process, &ier);
	    if ( ier != 0 ) {
                strcpy(strln, " ");
		er_lmsg ( &level, sggrp, &ier, strln, &ier1,
		strlen(sggrp), strlen(strln) );
	    }

            if ( process ) {
                for ( ii = 0; ii < ingrp; ii++ ) {
	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);
		    cst_alnm (  el.elem.spt.text[0], &type, &ier);

                    if ( ( el.hdr.vg_class == CLASS_TEXT ) &&
	                 ( el.hdr.vg_type == SPTX_ELM )    &&
	                 ( ( el.elem.spt.info.sptxtyp == TEXT_BOX )  ||
		           ( el.elem.spt.info.sptxtyp == TEXT_HIGH ) ||
		           ( el.elem.spt.info.sptxtyp == TEXT_LOW ) ) &&
			   ( type == 2 ) ) {

		        cst_crnm (  el.elem.spt.text, &trplvl, &ier );
		        trplvl = trplvl * 100.0F;
		    
		        if ( el.elem.spt.info.sptxtyp == TEXT_BOX ) {

                           /*
                            * Check to see if the ungrouped element is a 
                            * tropopause box type.
                            */

                            if ( ingrp > 0 ) {
                                new = ( trop_t *) malloc (sizeof ( trop_t ) );
			        membx = True;
                                new->next = NULL;
                                if ( numbx != 0 ) {
	                            trop -> next = new;
                                    trop = new;
                                }
                                else {
                                    trop = new;
                                    head = trop;
                                }
		            }
                            numbx++;
		            trop -> level = pr_hgfm (&trplvl);
		            trop -> lat   = el.elem.spt.info.lat; 
		            trop -> lon   = el.elem.spt.info.lon;
		        }
		        else if ( el.elem.spt.info.sptxtyp == TEXT_HIGH ) {
                           /*
                            * Check to see if the ungrouped element is a 
                            * high tropopause box type.
                            */

                            if ( ingrp > 0 ) {
                                newh = (trophi_t *)malloc(sizeof(trophi_t));
			        memhi = True;
                                newh -> next = NULL;
                                if ( numhi != 0 ) {
	                            trophi -> next = newh;
                                    trophi = newh;
                                }
                                else {
                                    trophi = newh;
                                    headh = trophi;
                                }
		            }
		            numhi++;
		            trophi -> level = pr_hgfm (&trplvl);
		            trophi -> lat   = el.elem.spt.info.lat; 
		            trophi -> lon   = el.elem.spt.info.lon;
                        }
		        else if ( el.elem.spt.info.sptxtyp == TEXT_LOW ) {

                           /*
                            * Check to see if the ungrouped element is a 
                            * low tropopause box type.
                            */

                            if ( ingrp > 0 ) {
                                newl = (troplo_t *)malloc(sizeof(troplo_t));
			        memlo = True;
                                newl -> next = NULL;
                                if ( numlo != 0 ) {
	                            troplo -> next = newl;
                                    troplo = newl;
                                }
                                else {
                                    troplo = newl;
                                    headl = troplo;
                                }
		            }
		            numlo++;
		            troplo -> level = pr_hgfm (&trplvl);
		            troplo -> lat   = el.elem.spt.info.lat; 
		            troplo -> lon   = el.elem.spt.info.lon;
                        }
	            }
                }
	        numtrp = numbx + numhi + numlo;
	    } 
	    if ( numtrp == 0 ) {
	        *iret = -2;
	    }
        }
	else {
	    if ( numtrp == 0 ) {
	        *iret = 9;
                er_lmsg ( &level, sggrp, iret, gtstr, &ier1,
	                  strlen(sggrp), strlen(gtstr) );
	    }
	}
   /* 
    * Write out tropospheric information in BUFR ASCII format.
    */
    if ( ier == 0 ) {
        ptr = head;
        ptrh = headh;
        ptrl = headl;

        cas_wrtrop ( ifpout, ptr, numbx, ptrh, numhi, ptrl, numlo, chlvl, &ier );
	
    }
   /*
    * Free memory from the linked list.
    */

    if ( membx ) {

       /*
        * Free memory for spot trop linked list.
	*/
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2 != NULL ) {
	    free (ptr);
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
        }
        free ( ptr );
    }

    if ( memhi ) {
       /*
        * Free memory for high trop linked list.
	*/
        ptrh = headh;
        ptr2h = ptrh -> next;
        while ( ptr2h != NULL ) {
	    free (ptrh);
            ptrh = ptr2h;
	    ptr2h = ptr2h -> next;
        }
        free ( ptrh );
    }

    if ( memlo ) {
       /*
        * Free memory for low trop linked list.
	*/
        ptrl = headl;
        ptr2l = ptrl -> next;
        while ( ptr2l != NULL ) {
	    free (ptrl);
            ptrl = ptr2l;
	    ptr2l = ptr2l -> next;
        }
        free ( ptrl );
    }
}
