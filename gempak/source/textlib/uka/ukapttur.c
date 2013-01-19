#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  TURB_LINE 	5

void uka_pttur ( FILE *ifpout, char *fname, FILE *fptr, long size,
                char gptyp, char *gtstr, int maxgnm, char *chlvl, int *iret )
/************************************************************************
 * uka_pttur								*
 *                                                                      *
 * This function scans a VG file for elements of group type "TURB" and  *
 * prints out all that match to an ASCII file.				*
 *                                                                      *
 * uka_pttur ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, chlvl,	*
 *		iret )  						* 
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input VG file name		*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"TURB" group type number	*
 *      *gtstr		char		"TURB" group type name		*
 *	maxgnm		int		Maximum number of group numbers *
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0 normal return		*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC         2/02   Fixed freeing memory error		*
 * A. Hardy/SAIC         2/02   Changed IMISSD -> SIGIMSS for tmpdeg	*
 * A. Hardy/SAIC         3/02   Fixed closed line end points		*
 * A. Hardy/NCEP	 6/02   Added sig_grpchk			*
 * A. Hardy/NCEP	 8/02   Added check for '0.0' base & top	*
 * M. Li/SAIC		 4/04	Change the check on turbsym == 10 to 8	*
 * M. Li/SAIC		 4/04	Modified turbulence categories		*
 * M. Li/SAIC		 5/04	Copied from sig_turb			*
 * M. Li/SAIC		 9/04	Added chlvl				*
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level; 
    int		ii, jj, ij, ingrp, numturb, ier, ier1;
    int	 	*tmpgrp, numstr, tmpdeg;
    float       top, base, tmplv1, tmplv2;
    char	vgclss, vgnum,  grp[4], sggrp[12], strln[256];
    char        **aryptr;
    Boolean	done, found, memalc, process;

    turb_t	*turb=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    istoff  = 0;
    numturb = 0;
    level   = 0;
    done    = False;
    memalc  = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_LINES;
    vgnum  = LINE_ELM;
    sbtyp  = TURB_LINE;

   /*
    * Declaring the memory size for parsing the text string
    * later. Also declare the memory size for temporary group
    * number array.
    */

    aryptr = (char **) malloc(sizeof(char *) * 2);
    for ( ii = 0; ii < 2; ii++ ) {
	aryptr[ii] = (char *) malloc(40);
    }
    tmpgrp = (int *) malloc(sizeof(int *) * (size_t)maxgnm);
    for ( ij = 0; ij < maxgnm; ij++){
	tmpgrp[ij] = 0;
    }

   /*
    * Read an element header to find the group type number.
    */

    while ( !done ) {

        cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;
        if ( ( gpnum > 0 ) && (ier == 0 ) ){
	    if ( tmpgrp[gpnum-1] == 0 ) {
		 found = False;
	    }
	}

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0 and a special line has not been
	* found, continue.
        */

        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) &&
	     ( !found ) ) {

	    cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	    	          members, &ingrp, &ier);

	    uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	                 &process, &ier);
            if ( ier != 0 ) {
		sprintf (strln,"TURB group number %d", gpnum);
                er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	                  strlen(sggrp), strlen(strln) );
	    }

            if ( process ) {
 
                for ( ii = 0; ii < ingrp; ii++ ) {
	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

                    if ( el.hdr.vg_class == CLASS_TEXT ) {

		       /* 
		        * Parse text string into CAT top and base,
		        * and turbulence catagory.
		        */

		        cst_clst ( el.elem.spt.text, '/', " ", 2, 40, 
			            aryptr, &numstr, &ier );
		        if ( numstr == 2 ) {
		            cst_crnm ( aryptr[1], &base, &ier );
			    base = base * 100.0F;
		            tmplv1 = pr_hgfm (&base);
			    if ( tmplv1 < 1.0F ) {
				tmplv1 = SIGRLMS;
			    }
		        }
		        else {
		            tmplv1 = SIGRLMS;
		        }
		        if ( ( numstr == 1) || ( numstr == 2 ) ) {
		            cst_crnm ( aryptr[0], &top, &ier );
			    top = top * 100.0F;
		            tmplv2 = pr_hgfm (&top);
			    if ( tmplv2 < 1.0F ) {
				tmplv2 = SIGRLMS;
			    }
		        }
		        else {
		            tmplv2 = SIGRLMS;
		        }

		       /* 
		        * Determine turbulence category.
		        */

			/* Severe  */
		        if ( el.elem.spt.info.turbsym == 6 ) {
			    tmpdeg = 7;
		        }

			/* Moderate, occasional severe */
		        else if ( el.elem.spt.info.turbsym  == 56) {
			    tmpdeg = 19;
		        }

			/* Moderate */
                        else if ( el.elem.spt.info.turbsym == 4 ) {
		    	    tmpdeg = 6;
		        }
		        else {
			    tmpdeg = SIGIMSS;
		        }
	            }
	        }


	       /*
	        * Begin looping through the individual elements with the 
	        * same group type and group number.
	        */

                for ( ii = 0; ii < ingrp; ii++ ) {

	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	            if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	                 ( el.hdr.vg_type  == LINE_ELM )    &&
	                 ( el.elem.spl.info.spltyp == TURB_LINE ) ) {

                       /*
                        * Check to see if the element in the group is a 
                        * turbulence type line. 
                        */

                        if ( ingrp > 0 ) {
                            new = ( turb_t *) malloc (sizeof ( turb_t ) );
			    memalc = True;
                            new->next = NULL;
                            if ( numturb != 0 ) {
	                        turb -> next = new;
                                turb = new;
                            }
                            else {
                                turb = new;
                                head = turb;
                            }
		        }

		        turb -> npt = el.elem.spl.info.numpts;
		        for ( jj = 0; jj < turb->npt;jj++ ) {
		            turb -> lat[jj] = el.elem.lin.latlon[jj];
		            turb -> lon[jj] = 
		                             el.elem.lin.latlon[jj + 
			                      el.elem.lin.info.numpts];
		        }

                       /*
		        * Check for a closed line flag.  If true, add
		        * the first point to the last array position.
		        */

		        if (  el.hdr.closed == 1 ) {
		            turb -> lat[turb->npt] = turb->lat[0];
		            turb -> lon[turb->npt] = turb->lon[0];
		            turb -> npt = el.elem.spl.info.numpts+1;
		        }

		       /*
		        * Store turbulence information into current 
		        * linked list.
		        */

		        numturb++;
		        turb -> level1  = tmplv1;
		        turb -> level2  = tmplv2;
		        turb -> tdeg    = tmpdeg;
		        if ( !found ) {
		            found = True;
		            tmpgrp[gpnum-1] = 1;
		        }
	            }
                }
	    } 
        }
	else if  ( ( ieloff == IMISSD ) || ( ier != 0 ) ) {
	    done = True; 
	    ier = 0;
	    if ( numturb == 0 ) {
	        ier = 1;
                er_lmsg ( &level, sggrp, &ier, gtstr, &ier1,
	                  strlen(sggrp), strlen(gtstr) );
	    }
	}
    }

   /* 
    * Write out TURB information in BUFR ASCII format.
    */

    if ( ier == 0 ) {
        ptr = head;
        cas_wrturb ( ifpout, ptr, numturb, chlvl, &ier );
    }

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2 != NULL ) {
	    free (ptr);
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
        }
        free ( ptr );
    }

   /*
    * Free memory from parsing text string and temp. group number.
    */

    for ( ii = 0; ii < 2; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
    free ( tmpgrp );

}
