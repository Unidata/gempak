#include "geminc.h"
#include "gemprm.h"
#define CES_GRPTYP
#include "cesgtcmn.h"


void ces_gtrtbl ( int *iret )
/************************************************************************
 * ces_gtrtbl								*
 *									*
 * This function initializes the _grpTbl structure in 2 steps.		*
 *									*
 *    1) The hard coded master list of all possible group types and     *
 *    associated group numbers are loaded into the _grpTbl.mstr[] array.*
 *									*
 *    2) The grptyp.tbl is read.  If any group entry in that table      *
 *    matches an entry in the master list, then the information on that *
 *    group type is placed in the _grpTbl.grps[] array.  If the         *
 *    grptyp.tbl cannot be read, the grps pointer will be left NULL to  *
 *    signal this condition to all subsequent querying routines.	*
 *									*
 *    All routines which return translations between group name and id  *
 *    number or vice-versa should use the mstr[] array.			*
 *									*
 *    All routines which return information that comes from the         *
 *    grptyp.tbl (labels and element attributes associated with a       *
 *    specific group type) should be taken from the grps[] array.	*
 *									*
 * ces_gtrtbl ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -3 = unable to load grptyp.tbl *
 *				  	 -4 = unable to allocate memory *
 **									*
 * Log:									*
 * H. Zeng/EAI          02/01   copied from pggrpw_rtbl()               *
 * H. Zeng/EAI          03/01   created an internal mapping array       *
 * H. Zeng/EAI          03/01   modified to read attribute info.        *
 * S. Schotz/NCEP	11/01 	Renamed TORN, WIND and HAIL groups	*
 * E. Safford/SAIC	02/02	combined w/ ces_gtinit; use cesgtcmn.h	*
 * R. Tian/SAIC		07/02	handle continuation of group name line *
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
int  ii, nn, nr = 0, ier, ier2, index, length, num_str, intg;
char buffer[256], tmpbuf[256], str[128], *iptr, grpnam[20]; 
char ignore[10], label[20], str1[128];
int  nc;
FILE *fp;
Boolean   exist;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Load the master group list.
     */
    ces_gtlmstr( iret );
    if ( *iret < 0 ) {
	return;
    }


    /*
     *  Now read the Group Type table.  For every entry there that
     *  matches a group name in the master list, add an entry into the 
     *  grps[] array. 
     */
    fp = cfl_tbop( GRPTYPE_TBL, "pgen", &ier );
    if ( ier == 0 ) { 
	cfl_tbnr( fp, &nr, &ier);
    }
	    
    if ( nr == 0 ) {
	fclose(fp);
        *iret = -1;
        return;
    }

    /* 
     *  First, count the number of lines with group names.  
     */ 
    ii = 0;
    nn = 0;
    while ( ii < nr ) {
        cfl_trln(fp, 256, buffer, &ier);
        if ( buffer[0] != '+' && !isspace(buffer[0]) ) {
            nn++;
        }
        ii++;
    }
    rewind ( fp );

    /*
     *  Allocate space for the groups.
     */
    _grpTbl.grps = (avail_grp_t *)malloc( (size_t)nn * sizeof(avail_grp_t) );
    if (_grpTbl.grps == NULL ) {
	*iret = -4;
	return;
    }
   
    _grpTbl.ngrp = 0;
    ii = 0;

    cfl_trln(fp, 256, buffer, &ier);
    while ( ier >= 0 && ier != 4 ) {

        if ( ier == 0 ) {

	    /*
	     *  A '+' character indicates a label and associated attribute
	     *  values for a specific group. 
	     */
	    if (buffer[0] != '+') {
                exist = FALSE;

		/*
		 * If the GOURP NAME line has continuation indicator, read
		 * all of them in, remove the continuation indicator 
		 * and blanks, and concat them into one line.
		 */
		while(buffer[strlen(buffer)-1] == '\\') {
    		    cfl_trln(fp, 256, tmpbuf, &ier);
		    cst_ldsp(tmpbuf, tmpbuf, &nc, &ier);
		    cst_ncpy(&buffer[strlen(buffer)-1], tmpbuf, nc, &ier);
		}

                /*
                 * Read info. from buffer, allowing older format. 
                 */
                num_str = sscanf(buffer, "%s %s %s", grpnam, str, str1);
                cst_lcuc(grpnam, grpnam, &ier2);
                if (num_str == 3) {
                   cst_ncpy(str, str1, sizeof(str), &ier2);
                }

                for( nn = 0; nn < _grpTbl.nmstr; nn++ ) {
                     if(strcmp(grpnam, _grpTbl.mstr[nn].name) == 0) {
	                cst_ncpy( _grpTbl.grps[ii].name, grpnam, 
							MAX_GRP_STR, &ier2);
	                _grpTbl.grps[ii].type = _grpTbl.mstr[nn].type;
                        _grpTbl.grps[ii].nitems = 0;
                        _grpTbl.ngrp++;
                        exist = TRUE;
                        break;
                     }
                }

		/*
 		 *  'Exist' indicates the presence of labels and attrib strings
		 */
                if( exist && strcmp(str, "-")!=0 ) {
                     
		     /*
                      * Process the label item list
                      */
		     iptr = strtok( str, ";" );
		     while ( iptr != (char)NULL && 
                             _grpTbl.grps[ii].nitems < GRP_MXELE-1 )  {

			 index = _grpTbl.grps[ii].nitems;
			 cst_ncpy( _grpTbl.grps[ii].label[index], iptr, 
						GRP_MXCHR, &ier2 );
			 cst_ncpy( _grpTbl.grps[ii].info[index], "\0",
						GRP_MXINFO, &ier2 );
                         _grpTbl.grps[ii].nitems++;
		         iptr = strtok( NULL, ";" );
		     }

		} /* the end of if */

                if( exist ) {
                     index = _grpTbl.grps[ii].nitems;
                     strcpy(_grpTbl.grps[ii].label[index], "Other");
                     _grpTbl.grps[ii].nitems++;
		     ii++;
                }

	    } /* the end of if(buffer[0]...) */
            else if( exist ) {

	       /*
		* read attribute info.
		*/
               sscanf(buffer, "%s %s %s", ignore, label, str);
	    
	       for (nn = 0; nn < _grpTbl.grps[ii-1].nitems; nn++) {
		  if (strcmp(_grpTbl.grps[ii-1].label[nn], label) == 0 ) {

                     if(strstr(str, "/") == NULL ) {

                       /*
                        * Allow having only color info.
                        */
                       cst_numb(str, &intg, &ier2);            
                       sprintf(str, 
                       "%d/1/1.0/2/0/0|%d/medium/21/2/121/s/c/hw",intg,intg);

                     }
                     else {
                       iptr = strstr(buffer, str);
                       cst_rmbl(iptr, str, &length, &ier2);

                     }

		     cst_ncpy( _grpTbl.grps[ii-1].info[nn], str, 
							GRP_MXINFO, &ier2);
                     break;
		  }
	       }

	    } /* End of if (buffer[0] != '+') */
         
        } /* the end of if ( ier == 0 ) */

	cfl_trln(fp, 256, buffer, &ier);

    } /* End of while */    

    fclose(fp);
  

}
