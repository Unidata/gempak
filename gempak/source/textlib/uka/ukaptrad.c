#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  RAD_ELM 	41.0F

void uka_ptrad ( FILE *ifpout, char *fname, FILE *fptr, long size,
                   char gptyp, char *gtstr, int *iret )
/************************************************************************
 * uka_ptrad 								*
 *                                                                      *
 * This function scans a VG file for elements of group type "LABEL",    *
 * look for radiation symbols and prints out all that match to an       *
 * ASCII file.								*
 *                                                                      *
 * uka_ptrad( ifpout, fname, fptr, size, gptyp, gtstr, iret ) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input file name			*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"LABEL" group type		*
 *      *gtstr		char		"LABEL" group type name		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0  normal			*
 *					= 16 no RADIATION found		*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC        01/02   Added error check numrad; fixed freeing	*
 * A. Hardy/SAIC        01/02   Removed unused found; moved numrad	*
 * A. Hardy/SAIC        03/02   Removed unused variable level		*
 * M. Li/SAIC		05/04	Copied from sig_radtn			*
 * M. Li/SAIC		06/04	Added a check for RADIATION		*
 * D. Kidwell/NCEP	06/04	Increased length of symtyp              *
 * L. Hinson/AWC        03/12   Re-Engineer for RADIATION with Text LBL *
 *                              from ukaptvol.c                         *
 * L. Hinson/AWC        09/12   Fix Problem with Encoding Other objects *
 *                              of grptyp LABEL to RADIATION            *
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level; 
    int		ii, ingrp, numrad, numstr, ier, ier1;
    float       tmplat, tmplon;
    char	vgclss, vgnum, grp[4], sggrp[12];
    char        **aryptr; 
    char        symtyp[10], radnam[24];
    char        cget;
    Boolean     done, memrad;
    int         foundrad;

    volrad_t	*rad=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ier     = 0;
    istoff  = 0;
    numrad  = 0;
    level   = 0;
    done    = False;
    memrad  = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_SYMBOLS;
    vgnum  = SPSYM_ELM;
    sbtyp  = -99;
    
    aryptr = (char **) malloc(sizeof(char *) * 3);
    for ( ii = 0; ii < 3; ii++ ) {
        aryptr[ii] = (char *) malloc(40);
    }

   /*
    * Read an element header to find the group type number.
    */

    while ( !done ) {

        cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0, continue.
        */


        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) ) {
	     
            cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF,
               members, &ingrp, &ier);
	   /*
	    * Begin looping through the individual elements with the 
	    * same group type and group number.
	    */
            foundrad = 0;
            for ( ii = 0; ii < ingrp; ii++ ) {

	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

               /*
		* Check for radiation symbol.
		*/ 

	        if (  el.hdr.vg_class == CLASS_SYMBOLS  &&
	              el.hdr.vg_type == SPSYM_ELM       &&
                      G_DIFF(el.elem.sym.data.code[0], RAD_ELM) ) {
                    foundrad = 1;
                    tmplat = el.elem.sym.data.latlon[0];
                    tmplon = el.elem.sym.data.latlon[0 + 
			                       el.elem.sym.info.numsym];
                }
                else if (el.hdr.vg_class == CLASS_TEXT  && foundrad) {
                  cget = CHLF;
                  cst_clst (el.elem.spt.text, cget, " ", 3, 40,
                            aryptr, &numstr, &ier );
                            
                  strcpy (radnam, aryptr[0]);
                  if (radnam[strlen(radnam)-1] == '-') {
                    strcat(radnam,aryptr[1]);
                  }
                  
                  
                }
            }

           /*
            * Fill out the radtn structure, setting date/time
	    * information to missing.
            */

            if ( ingrp > 0 && foundrad) {
		numrad++;
		memrad = True;
                new = (volrad_t *) malloc(sizeof(volrad_t));
                new -> next = NULL;
                if ( numrad > 1 ) { 
	            rad -> next = new;
                    rad = new;
                }
                else {
                    rad = new;
                    head = rad;
	        }

		strcpy (rad -> name, radnam);
                rad -> lat = tmplat;
                rad -> lon = tmplon;
                rad -> year   = SIGIMSS;
                rad -> month  = SIGIMSS;
                rad -> day    = SIGIMSS;
                rad -> hour   = SIGIMSS;
                rad -> minute = SIGIMSS;
	    } 
            ier  = 0;
        }  
        else {
            ier  = 0;
            done = True;
	    if ( numrad == 0 ) {
                ier  = -1;
	    }
        }
    } 

   /* 
    * Write out LABEL information in BUFR ASCII format.
    */

    strcpy ( symtyp, "RADIATION" );
    if ( ier == 0 ) {
        ptr = head;
	cas_wrvlrd ( ifpout, ptr, numrad, symtyp, &ier); 

    }
    else {
        *iret = 16;
        er_lmsg ( &level, sggrp, iret, symtyp, &ier1,
                              strlen(sggrp), strlen(symtyp) );
    }

   /*
    * Free linked list memory for radiation.
    */

    if ( memrad ) {
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2 != NULL ) {
	    free ( ptr );
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
        }
        free ( ptr );
    }
    for ( ii = 0; ii < 3; ii++) {
      free ( aryptr[ii] );
    }
    free ( aryptr );
}
