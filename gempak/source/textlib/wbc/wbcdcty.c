#include "geminc.h"
#include "gemprm.h"

#define  EOL  "\n"
#define  LINE_LEN  66 

void wbc_dcty ( char **ugc_arr, char **cnam_arr, char **st_arr, int *ncnty, 
		char *eday, char *ehour, int *len1, int *ugcln, int *vtecln,
		char *prdcod, char *actn, char *offid, char *phen, 
		char *sigcd, char *etn,	int vtime[], int etime[], 
		char **ind_arr, char *cntystr, int *iret )
/************************************************************************
 * wbc_dcty                                                             *
 *                                                                      *
 * This program formats the UGC line for each state and the list of 	*
 * county names and independent city names. 				* 
 *                                                                      *
 * wbc_dcty ( ugc_arr, cnam_arr, st_arr, ncnty, eday, ehour, len1, 	*
 *	      ugcln, vtecln, prdcod, actn, offid, phen sigcd, etn,	*
 *            vtime, etime, ind_arr, cntystr, iret )         		*
 *                                                                      *
 * Input parameters:                                                    *
 *	**ugc_arr	char		UG codes array			*
 *	**cnam_arr	char		County names array		*
 *	**st_arr	char		County states array		*
 *	ncnty		int		Number of counties in the arrays*
 *	*eday		int		Date (DD)			*
 *	*ehour		char		Hour (HH)			*
 *	len1		int		Max length of 'cntystr'		*
 *	*ugcln		int		Flag for printing UG codes	*
 *					   0 - Only county names	*
 *					   1 - UG codes and counties	*
 *	*vtecln		int		Flag for writing VTEC line	*
 *					   0 - No VTEC line in product	*
 *					   1 - Use VTEC; No prod code	*
 *					   2 - Use VTEC; Use prod code	*
 * 	*prdcod		char		VTEC product type code		*
 * 	*actn		char		VTEC action code		*	
 * 	*offid		char		VTEC issuing office id 		*	
 * 	*phen		char		VTEC phenonmena code		*	
 * 	*sigcd		char		VTEC significant action code	*	
 * 	*etn		char		VTEC event tracking number	*	
 * 	vtime[]		int		VTEC start time 		*	
 * 	etime[]		int 		VTEC ending time 		*	
 *                                                                      *
 * Output parameters:                                                   *
 *	**ind_arr	char		Independent cities array	*
 *	*cntystr	char		County/independent cities string*
 *      *iret           int		Return Code                     *
 *					  0 = normal return		*
 *					 -1 = max size of cntystr reached
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03   					*
 * A. Hardy/NCEP	11/03		Fixed memory deallocation	*
 * A. Hardy/NCEP	 1/04		Added VTEC parameters to call   *
 * T. Piper/SAIC	02/04	Removed unused variables lenug, itype,	*
 *				iugcnt, and oneln 			*
 * A. Hardy/NCEP	 2/04		Removed printf statement	* 
 * A. Hardy/NCEP	 4/04	Add 'NEW' vs. 'CON'/'CAN' start time	*	
 * A. Hardy/NCEP	 4/04   Added marine zone logic and formatting  *
 * A. Hardy/NCEP	 7/04   Inc. arrays cntyname,cname,holdw,ugcstr *
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * F. J. Yen/NCEP	 2/06	Use len1 to check for mem leaks;set iret*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 ***********************************************************************/
{
    int     ii, jj, ik, im, ier, len, six, srch; 
    int	    lenc;
    int     ier1;
    int     inumb, iugc, indy, icnt, iind, numcnty, ietn;
    char    stid[3], name[22], county[9], cname[33], ctyind[19];
    char    tmpw[5000], holdw[5000], parish[9], rmname[9], cntyname[33];
    char    blank[2]={' '}, ugcstr[1500], **cty_arr, vtec[49], zname[256];
/*-------------------------------------------------------------------*/
    *iret = 0;

    ier = 0;
    ier1 = 0;
    ii = 0;
    jj = 0;
    six = 6;
    srch = 2;
    cntystr[0] = '\0';
    strcpy( county, "COUNTIES");
    strcpy( parish, "PARISHES");
    strcpy( ctyind, "INDEPENDENT CITIES");
    strcpy( rmname, "CITY OF ");

    cty_arr = (char **)malloc(*ncnty * sizeof(char *));
   /*
    * Set up county watch section. 
    */

     iugc = 0;
     while ( ii < *ncnty ) {
         strcpy(stid, st_arr[ii]);
         tb_idst ( stid, name, &ier1, strlen(stid), sizeof(name) ); 

        /*
	 * Setting up the zone county string.
	 */

	 if ( *ugcln == 1 ) {
	     holdw[0] = '\0';
	     tmpw[0] = '\0';
	     cst_numb (ugc_arr[iugc]+3, &numcnty, &ier);

	     ik = 0;
             while  ( (iugc < *ncnty) && 
	             ( (strcmp(st_arr[iugc], stid) == 0 ) ) ){
		 if ( iugc < *ncnty ) {
    		    cty_arr[ik] = (char *)malloc((six+1) * sizeof(char));
		    strcpy ( cty_arr[ik], ugc_arr[iugc] );
	            iugc++;
		    ik++;
		 }
	     }

	    /*
	     * Check if a Great Lake or Lake St. Clair is the name.
	     * Reset error code if it is to group all lake UG codes
	     * together.
	     */

	     if ( ( strcmp(name,"LAKE HURON") == 0 ) || 
	          ( strcmp(name,"LAKE ONTARIO") == 0 ) || 
	          ( strcmp(name,"LAKE MICHIGAN") == 0 ) ||
	          ( strcmp(name,"LAKE ST. CLAIR") == 0 ) ||
	          ( strcmp(name,"LAKE ERIE") == 0 ) ||
	          ( strcmp(name,"LAKE SUPERIOR") == 0 ) ) ier1 = -15;
	    /*
	     * Check if another marine zone region follows. If so, append it to
	     * the current marine zones in the UGC array.
	     */

	     if ( (ier1 < 0 ) && ( (iugc < *ncnty) &&
	           (ugc_arr[iugc][2] == 'Z' ) ) ) {

                 while  ( (iugc < *ncnty) && (ugc_arr[iugc][2] == 'Z') ) {
		     if ( iugc < *ncnty ) {
    		         cty_arr[ik] = (char *)malloc((six+1) * sizeof(char));
		         strcpy ( cty_arr[ik], ugc_arr[iugc] );
	                 iugc++;
		         ik++;
		     }
	         }
	     }

	     utl_ugcp ( cty_arr, &ik, eday, ehour, &len, ugcstr, &ier);

             if ( ik > 1 ) {
                 for( im = 0; im < ik-1; im++ ) {
                     free( cty_arr[im] );
                 }
	     }
	     else {
                 free( cty_arr[0] );
	     }

             strcat (tmpw, ugcstr );
             strcat (tmpw, EOL );

            /*
	     * Setting up the VTEC string.
	     */

	     if ( *vtecln == 1 ) {
		 cst_numb ( etn, &ietn, &ier);
	         if ( strcmp ( actn, "NEW") == 0 ) {
		 sprintf( vtec, 
		  "/%s.%s.%s.%s.%04d.%02d%02d%02dT%02d%02dZ-%02d%02d%02dT%02d%02dZ/",
		  actn, offid, phen, sigcd, ietn,
		  vtime[0]%100, vtime[1], vtime[2], vtime[3], vtime[4],
		  etime[0]%100, etime[1], etime[2], etime[3], etime[4]);
	         }
		 else {
		 sprintf( vtec, 
		  "/%s.%s.%s.%s.%04d.000000T0000Z-%02d%02d%02dT%02d%02dZ/",
		  actn, offid, phen, sigcd, ietn,
		  etime[0]%100, etime[1], etime[2], etime[3], etime[4]);
		 }
                  strcat (tmpw, vtec );
                  strcat (tmpw, EOL );
	     }
	     else if ( *vtecln == 2 ) {
		 cst_numb ( etn, &ietn, &ier);
	         if ( strcmp ( actn, "NEW") == 0 ) {
		 sprintf( vtec, 
		  "/%s.%s.%s.%s.%s.%04d.%02d%02d%02dT%02d%02dZ-%02d%02d%02dT%02d%02dZ/",
		   prdcod, actn, offid, phen, sigcd, ietn,
		   vtime[0]%100, vtime[1], vtime[2], vtime[3], vtime[4],
		   etime[0]%100, etime[1], etime[2], etime[3], etime[4]);
		 }
		 else {
		 sprintf( vtec, 
		  "/%s.%s.%s.%s.%s.%04d.000000T0000Z-%02d%02d%02dT%02d%02dZ/",
		   prdcod, actn, offid, phen, sigcd, ietn,
		   etime[0]%100, etime[1], etime[2], etime[3], etime[4]);
		 }
                  strcat (tmpw, vtec );
                  strcat (tmpw, EOL );
	     }

             strcat (tmpw, EOL );
             strcat( cntystr, tmpw);
	 }

        /*
	 * Setting up the states included string.
	 */

	 tmpw[0] = '\0';
	 if ( ugc_arr[ii][2] != 'Z' ) {
          sprintf ( tmpw, "%s \n", stid);
          strcat( cntystr, tmpw);
	  if ( strcmp ( stid,"LA") != 0 ) {
	     sprintf ( tmpw, ".    %s %s INCLUDED ARE\n\n", 
	               name, county);
	 }
	 else {
	     sprintf ( tmpw, ".    %s %s INCLUDED ARE\n\n", 
	               name, parish);
	 }
         strcat( cntystr, tmpw);

         jj = 0;
	 iind = 0;
	 indy = 0;
	 holdw[0] = '\0';
         while ( ( ii < *ncnty ) &&
		 ( strcmp(st_arr[ii], stid) == 0 ) ) {

              if ( jj == 3 ) { 
                  strcat(holdw, "\n");
                  strcat( cntystr, holdw);
                 jj = 0;
	      }

	      cst_rnan (  cnam_arr[ii], cname, &ier );
	      cst_lcuc ( cname, cname, &ier );

	     /*
	      * Check for independent cities.
	      */
	        
	      cst_rmst (cname, rmname, &inumb, cntyname, &ier);
              if ( inumb == 0 ) {
	          strcpy ( cname, cntyname );
	      }

	      cst_numb (ugc_arr[ii]+3, &numcnty, &ier);
	      if (  numcnty > 509 ) {
		   strcpy ( ind_arr[iind], cname );
		   iind++;
		   indy = 1;
	      }
               
	      else {
	         /*
	          * Print out names of counties. Set spacing for columns
		  * 2 and 3.
	          */

		  if ( jj < 1 ) {
                      sprintf ( holdw, "%-21s", cname);
		  }
		  else {
                      sprintf ( holdw, "%-20s", cname);
		  }
                  strcat( cntystr, holdw);
	          holdw[0] = '\0';
	          jj++;
	      }
	      ii++;
          }
          strcat( cntystr, holdw);

	  tmpw[0] = '\0';
	  if ( indy == 0 ) {
              if ( ii >= *ncnty ) {
	          strcat ( tmpw, "\n$$\n\n");
	          if ( *ugcln ) {
		    strcat ( tmpw, "\n");
		  }
                  strcat( cntystr, tmpw);
	      }
              else {
	          if ( (ii == ( *ncnty - 1) ) || 
		    ( strcmp(st_arr[ii+1], stid) != 0 ) ) {
	              strcat ( tmpw, "\n$$\n\n");
	              if ( *ugcln ) {
		        strcat ( tmpw, "\n");
		      }
                      strcat( cntystr, tmpw);
		  }
              }
	  }

         /*
          *  Print out independent cities list.
          */

	  tmpw[0] = '\0';
	  holdw[0] = '\0';
	  lenc = strlen (cntystr);
          /*  Subtract 500 characters (for constant/known text throughout) */
	  lenc = lenc - 500;
          if (  indy  != 0 ) {
              cst_sort( srch, &iind, ind_arr, &iind, ind_arr, &ier );
	      if  ( ii == 0 )  {
		  strcat(tmpw, "\n\n");
	      } else {
		  strcat(tmpw, "\n\n\n");
	      }
	      if ( *ugcln ) {
                  sprintf ( holdw, "%s %s INCLUDED ARE\n\n", name, ctyind);
	      }
	      else {
                  sprintf ( holdw, "%s %s INCLUDED \n\n", name, ctyind);
	      }
              strcat( tmpw, holdw);
	      lenc = lenc + strlen (tmpw);
	      if ( lenc >= *len1 ) {
		  *iret = -1;
		  return;
	      }
              strcat( cntystr, tmpw);

	      tmpw[0] = '\0';
	      holdw[0] = '\0';
             /*
	      * Set spacing for columns 2 and 3.
	      */

              jj = 0;
              for ( icnt = 0; icnt <  iind; icnt++ ){
		   if ( jj < 1 ) {
                       sprintf ( holdw, "%-21s", ind_arr[icnt]);
		   }
		   else {
                       sprintf ( holdw, "%-20s", ind_arr[icnt]);
		   }
                   strcat( cntystr, holdw);
	      lenc = lenc + strlen (holdw);
	      if ( lenc >= *len1 ) {
		  *iret = -1;
		  return;
	      }

		  /*
		   * Only add new line if the column lengths aren't
		   * even.
		   */

                   jj++;
                   if ( ( jj == 3 ) && ( icnt != iind-1 ) ){
                       strcat(cntystr, "\n");
                       jj = 0;
                   }
              }
              strcat ( cntystr, "\n$$\n\n");
              if ( *ugcln ) strcat ( cntystr, "\n");
            }
	 }
	 else {

	    /*
	     * Write out all of the marine zone names under one heading.
	     */

	     len = LINE_LEN;
	     sprintf ( tmpw, "CW \n\n");
	     strcat ( cntystr, tmpw);
	     sprintf ( tmpw, ".    ADJACENT COASTAL WATERS INCLUDED ARE\n\n" );
             strcat( cntystr, tmpw);

	     holdw[0] = '\0';
	     tmpw[0] = '\0';
             while  ( (ii < *ncnty) && (ugc_arr[ii][2] == 'Z') ) {
	         cst_rnan (  cnam_arr[ii], zname, &ier );
		 cst_rpst ( zname, ",", "", zname, &ier );
		 cst_rpst ( zname, ",", "", zname, &ier );
		 cst_lcuc ( zname, zname, &ier );

	         sprintf(holdw,"%s \n\n", zname);
		 cst_wrap ( holdw, blank, &len, EOL, (char *)NULL, tmpw, &ier );
	      lenc = lenc + strlen (tmpw);
	      if ( lenc >= *len1 ) {
		  *iret = -1;
		  return;
	      }
	         strcat(cntystr, tmpw);
	         holdw[0] = '\0';
	         tmpw[0] = '\0';
	         ii++;
	     }
             strcat ( cntystr, "$$\n");
	 }
     }
     if (cty_arr) {
          free( (char **) cty_arr);
     }
}
