#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "proto_uka.h"
#include "cascmn.h"

#define  CLOUD_LINE 	3
#define  TURB_LINE 	5
#define  FRONT_QUAS     000
#define  FRONT_COLD     200
#define  FRONT_WARM     400
#define  FRONT_OCCL     600
#define  ITCZ_LINE      23
#define  ITCZ_LINE_Z    26
#define  JETS_LINE 	6
#define  TEXT_BOX       4
#define  TEXT_HIGH      2
#define  TEXT_LOW       1
#define  LABEL_VOLC 	201.0F

void uka_grpchk ( char *fname, FILE *fptr, long size, char *gtstr, 
                  int members[], int ingrp, Boolean *process, int *iret )
/************************************************************************
 * uka_grpchk								*
 *                                                                      *
 * This function checks for specific elements in a grouped set of       *
 * to verify that all necessary elements for the group exist.           *
 *                                                                      *
 * uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, process,      *
 *              iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fname		char		Input filename			*
 *	*fptr		FILE		File pointer			*
 *      size		long		Size of file in bytes		*
 *      *gtstr          char            Name of group type		*
 *      members[ingrp]	int		Array of elements of the group  *
 *	ingrp		int		Number of elements		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *process	Boolean		Process flag			*
 *      *iret           int             Return code                     *
 *					= 0  normal			*
 *					= 2  Found more than one line	*
 *					= 3  Missing line		*
 *					= 4  Missing text label		*
 *					= 5  Missing wind speed text	*
 *					= 6  Missing directional arrow  *
 *					= 7  Missing wind barb  	*
 *					= 8  Missing flight level text  *
 *					= 9  Missing tropopause text	*
 *					= 10 Slow front found		*
 *					= 11 Missing symbol    		*
 *					= 12 More than one text label	*
 *					= 13 Found more than one symbol	*
 *					= 14 wrong line type            *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP         6/02   Created					*
 * A. Hardy/NCEP         6/02   Fixed checks for fronts and jets	*
 * A. Hardy/NCEP         7/02   Only 1 symbol for t.s. & vols. allowed  *
 * A. Hardy/NCEP         8/02   Changed process to false in jet check;  *
 *				check for wrong grouped line types	*
 * A. Hardy/NCEP         1/03   Process jet if barbs != levels		*
 * M. Li/SAIC		 5/04	Copied from sig_grpchk			*
 * M. Li/SAIC		12/04	Checked for slow front			*
 ***********************************************************************/
{
    int             ii, jj, ier;
    int             isrch, flag, tmpfrt, tfrtnm, type;
    int             iwind, ispeed, ilevel, isym, itxt, iln, ivol, ivtxt;
    int		    iothr;
    Boolean         line, text, itcz;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ier     = 0;
    iln     = 0;
    isym    = 0;
    itxt    = 0;
    iothr   = 0;
    ivol    = 0;
    ivtxt   = 0;
    iwind   = 0;
    ispeed  = 0;
    ilevel  = 0;
    line    = False;
    text    = False;
    itcz    = False;
    *process = False;

    if ( strcmp ( gtstr, "CLOUD" ) == 0 ) {
	isrch = 1;
    }
    else if ( strcmp ( gtstr, "TURB" ) == 0 ) {
	isrch = 2;
    }
    else if ( strcmp ( gtstr, "FRONT" ) == 0 ) {
	isrch = 3;
    }
    else if ( strcmp ( gtstr, "JETS" ) == 0 ) {
	isrch = 4;
    }
    else if ( strcmp ( gtstr, "TROP" ) == 0 ) {
	isrch = 5;
    }
    else if ( strcmp ( gtstr, "LABEL" ) == 0 ) {
	isrch = 6;
    }
    else {
	isrch = 0;
    }

    switch ( isrch ) {
	case 	1:

	   /*
	    * Checking cloud group for at least 1 line and 1 text label.
	    */

	    ii = 0;
	    while ( ( ii < ingrp ) && ( !line ) ) {

	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier); 

		if ( ( el.hdr.vg_class == CLASS_LINES) &&
		     ( el.hdr.vg_type == SPLN_ELM )    &&
		     ( el.elem.spl.info.spltyp == CLOUD_LINE ) ) {
			line = True;

                    for ( jj = 0; jj < ingrp; jj++ ) {

	                cvg_rdhdr ( fname, fptr, members[jj], (int)size, 
			        &el, &flag, &ier );
                        cvg_rdele( &el, members[jj], el.hdr.recsz, fptr, 
			           &ier );
                        if ( el.hdr.vg_class == CLASS_TEXT )  {
                            itxt++;
			    text = True;
			}

		    }
		}
		ii++;
	    }

	   /*
	    * Check for other line types improperly grouped with clouds.
	    */

	    ii = 0;
            while ( ii < ingrp ) {
	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	        if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == LINE_ELM )    &&
	             ( el.elem.spl.info.spltyp == TURB_LINE ) ) { 
			iothr++;
		}
                else if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( el.elem.spl.info.spltyp == JETS_LINE ) ) { 
			iothr++;
		}
                else if ( ( el.hdr.vg_class == CLASS_FRONTS) &&
                          ( el.hdr.vg_type == FRONT_ELM ) ) {
			iothr++;
		}
		ii++;
	    }

	   /*
	    * Process cloud group if at least 1 line and a text
	    * label are grouped.
	    */

	    if ( ( line ) && ( text ) ) {
	        *process = True;
                if ( iothr > 1 ) {
		    *iret = 14;
	            *process = False;
                }
                else if ( itxt > 1 ) {
		    *iret = 12;
                }
	    }
	    else {
		if ( !line ) {
		    *iret = 3;
	            *process = False;
		}
		else if ( !text ) {
		    *iret = 4;
	            *process = False;
		}
	    }
	    break;

	case 	2:

	   /*
	    * Checking turbulence group for at least 1 line 
	    * and 1 text label.
	    */

	    ii = 0;
            while ( ( ii < ingrp ) && ( !line ) ) {

	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	        if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == LINE_ELM )    &&
	             ( el.elem.spl.info.spltyp == TURB_LINE ) ) { 
			line = True;
		   for ( jj = 0; jj < ingrp; jj++ ) {

	                cvg_rdhdr ( fname, fptr, members[jj], (int)size, 
			        &el, &flag, &ier );
                        cvg_rdele( &el, members[jj], el.hdr.recsz, fptr, 
			           &ier );
                        if ( el.hdr.vg_class == CLASS_TEXT )  {
			    itxt++;
			    text = True;
			}
		    }
	        }
		ii++;
	    }

	   /*
	    * Check for other line types improperly grouped with turb.
	    */

	    ii = 0;
            while ( ii < ingrp ) {
	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	        if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( ( el.elem.spl.info.spltyp == CLOUD_LINE ) ||
		       ( el.elem.spl.info.spltyp == JETS_LINE ) ) ) { 
			iothr++;
		}
                else if ( ( el.hdr.vg_class == CLASS_FRONTS) &&
                          ( el.hdr.vg_type == FRONT_ELM ) ) {
			iothr++;
		}
		ii++;
	    }

	   /*
	    * Process turb group if at least 1 line and text label
	    * are grouped.
	    */

            if ( ( line ) && ( text ) ) {
	        *process = True;
	        if ( iothr > 0 ) {
	            *iret = 14;
	            *process = False;
	        }
		else if ( itxt > 1 ) {
		    *iret = 12;
		}
	    }
	    else {
		if ( !line ) {
		    *iret = 3;
	            *process = False;
		}
		else if ( !text ) {
		    *iret = 4;
	            *process = False;
		}
	    }
	    break;
        case   3:

	   /*
	    * Check fronts for at least 1 front and that the number of
	    * directional arrows equal the number of speed text labels.
	    */

	    ii = 0;
            while ( ii < ingrp ) {

                cvg_rdhdr ( fname, fptr, members[ii], (int) size, &el,
                                &flag, &ier );
                cvg_rdele (&el, members[ii], el.hdr.recsz, fptr, &ier);

                if ( ( el.hdr.vg_class == CLASS_FRONTS) &&
                     ( el.hdr.vg_type == FRONT_ELM ) ) {
                       tmpfrt = ( el.elem.frt.info.fcode / 100 );
                       tfrtnm = tmpfrt * 100;
                    if ( ( tfrtnm == FRONT_QUAS ) ||
                       ( tfrtnm == FRONT_COLD ) ||
                       ( tfrtnm == FRONT_WARM ) ||
                       ( tfrtnm == FRONT_OCCL ) ) {
			iln++;
                        line = True;
		    }
		}
                else if ( ( el.hdr.vg_class == CLASS_LINES ) &&
                        ( el.hdr.vg_type == SPLN_ELM )     &&
                        ( el.elem.spl.info.spltyp == ITCZ_LINE ||
                          el.elem.spl.info.spltyp == ITCZ_LINE_Z ) &&
			( !line ) ) {
	            iln++;
                    line = True;
		    itcz = True;
		}
		ii++;
	    }
	    if ( !itcz ) {
                for ( jj = 0; jj < ingrp; jj++ ) {
                    cvg_rdhdr ( fname, fptr, members[jj], (int) size, &el,
                                &flag, &ier );
                    cvg_rdele(&el, members[jj], el.hdr.recsz, fptr, &ier);
                    if ( ( el.hdr.vg_class == CLASS_WINDS ) &&
                        ( el.hdr.vg_type == ARROW_ELM ) ) {
		        iwind++;
		    }
		    if  ( el.hdr.vg_class == CLASS_TEXT ) {
		        ispeed++;
		    }
	        }
	    }

	   /*
	    * Check for other line types improperly grouped with front.
	    */

	    ii = 0;
            while ( ii < ingrp ) {
	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	        if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( ( el.elem.spl.info.spltyp == CLOUD_LINE ) ||
		       ( el.elem.spl.info.spltyp == JETS_LINE ) ) ) { 
			iothr++;
		}
                else if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( el.elem.spl.info.spltyp == JETS_LINE ) ) { 
			iothr++;
		}
		ii++;
	    }

	   /*
	    * Process if front line is found and number of directional 
	    * arrows and text labels are the same, or if slow front is
  	    * found and there is just one speed text lavel and no
  	    * directinal arrow.
	    */

	    if ( ( line ) && ( iwind == ispeed ) ) {
	        *process = True;
		if ( iothr > 1 ) {
		    *iret = 14;
	            *process = False;
		}
		else if ( iln > 1 ) {
		    *iret = 2;
	            *process = False;
		}
	    }
	    else {
		if ( !line ) {
		    *iret = 3;
	            *process = False;
		}
		else if ( iwind != ispeed ) {
		    if ( iwind > ispeed ) {
		        *iret = 5;
	                *process = False;
		    }
		    else {
			if ( ispeed > 1 ) {
		            *iret = 6;
	                    *process = False;
			}
			else {
			    *iret = 10;
			    *process = True;
			}
		    }
		}
	    }
	    break;
	case   4:

	   /*
	    * Check jets for 1 line and that the number of wind barbs
	    * equals the number of text labels.
	    */

	    ii = 0;
	    while ( ii < ingrp ) {
		
                cvg_rdhdr ( fname, fptr, members[ii], (int) size, &el,
                                &flag, &ier );
                cvg_rdele (&el, members[ii], el.hdr.recsz, fptr, &ier);
                if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	             ( el.hdr.vg_type  == SPLN_ELM )    &&
	             ( el.elem.spl.info.spltyp == JETS_LINE ) ) { 
		    iln++;
		    line = True;
		}
		ii++;
	    }
            for ( jj = 0; jj < ingrp; jj++ ) {
                cvg_rdhdr ( fname, fptr, members[jj], (int) size, &el,
                            &flag, &ier );
                cvg_rdele(&el, members[jj], el.hdr.recsz, fptr, &ier); 
                if ( ( el.hdr.vg_class == CLASS_WINDS ) && 
		     ( el.hdr.vg_type  == BARB_ELM ) ) {
		    iwind++;
		}
                if  ( ( el.hdr.vg_class == CLASS_TEXT ) &&
                      ( el.hdr.vg_type == SPTX_ELM ) ) {
		    ilevel++;
	        }
	    }

	   /*
	    * Process if jet line is found and number of directional 
	    * arrows and text labels are the same.
	    */


            if ( ( line ) && ( iwind == ilevel ) ) {
	        *process = True;
		if ( iln > 1 ) {
		    *iret = 2;
	            *process = False;
		}
	    }
	    else {
		if ( !line ) {
		    *iret = 3;
	            *process = False;
		}
		else if ( iwind != ilevel) {
		    if ( iwind > ilevel ) {
		        *iret = 8;
	                *process = True;
		    }
		    else {
		        *iret = 7;
	                *process = True;
		    }
		}
	    }
	    break;
	case   5:

	   /*
	    * Check tropopause text boxes for at least 1 of any type.
	    */

	    ii = 0;
            while ( ( ii < ingrp ) && ( !text ) ) {
	        cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

                if ( ( el.hdr.vg_class == CLASS_TEXT ) &&
	             ( el.hdr.vg_type == SPTX_ELM )    &&
	             ( ( el.elem.spt.info.sptxtyp == TEXT_BOX )  ||
		       ( el.elem.spt.info.sptxtyp == TEXT_HIGH ) ||
		       ( el.elem.spt.info.sptxtyp == TEXT_LOW ) ) ) { 

 		      /*
                       * Check that the text label is numeric.
		       */

			cst_alnm ( el.elem.spt.text[0], &type, &ier );
			if ( type == 2 ) {
			    text = True;
			}
		}
		ii++;
	    }

	   /*
	    * Process if at least 1 trop. box is found.
	    */

            if ( text ) {
	        *process = True;
	    }
	    else {
	        *iret = 9;
	    }

	    break;
	case   6:

	   /*
	    * Check for tropical storm symbols.
	    */

	    ii = 0;
	    while ( ii < ingrp ) {
		cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
			        &flag, &ier );
	        cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

                if ( ( el.hdr.vg_class == CLASS_SYMBOLS )  &&
		      ( el.hdr.vg_type == SPSYM_ELM ) ) {
                    for ( jj = 0; jj < el.elem.sym.info.numsym; jj++) {
                   
                        if (  G_DIFFT(el.elem.sym.data.code[jj], 25.0F, 0.01) ||
                              G_DIFFT(el.elem.sym.data.code[jj], 26.0F, 0.01) ||
                              G_DIFFT(el.elem.sym.data.code[jj], 27.0F, 0.01) ||
                              G_DIFFT(el.elem.sym.data.code[jj], 28.0F, 0.01) ) {
	                        isym++;
	                } 
		    } 
		}

                else if ( el.hdr.vg_class == CLASS_TEXT) {
	            itxt++;
		}
		ii++;
	    } 

	    if ( ( isym == itxt ) && ( isym == 1 ) ) {
		*process = True;
	    }
	    else if ( ( isym == itxt ) && ( isym > 1 ) ) {
		    *iret = 13;
	    }
	    else {
	        if ( isym > itxt) {
		    *iret = 4;
		    
		}
	        else if ( isym < itxt ) {
		    *iret = 11;
	        }
		itxt = 0;
	    }

	   /*
	    * Checking for volcano symbols.
	    */ 

	    if ( ( isym == 0 ) && ( itxt == 0 ) ) {
		*iret = 0;
                ii = 0;
	        while ( ii < ingrp ) {
		    cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		    	        &flag, &ier );
	            cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

                    if ( el.hdr.vg_class == CLASS_SYMBOLS &&
	                 el.hdr.vg_type == WXSYM_ELM      &&
                         G_DIFF(el.elem.sym.data.code[0], LABEL_VOLC) ) {
			    ivol++;
		    }
                    if ( el.hdr.vg_class == CLASS_TEXT) {
	                ivtxt++;
		    }
		    ii++;
	        } 

	        if ( ( ivol == ivtxt ) && ( ivol == 1 ) ) {
		    *process = True;
	        }
	        else if ( ( ivol == ivtxt ) && ( ivol > 1 ) ) {
		    *iret = 13;
	        }
	        else {
	            if ( ivol > ivtxt) {
		        *iret = 4;
		    }
		    else if ( ivol < ivtxt ) {
		        *iret = 11;
		    }
	        }
	    }
	    break;
	default:
	    break;
    }
}
