
#include "afcmn.h"


/************************************************************************
 * afconditions.c                                             		*
 *                                                                      *
 * This module contains all the subroutines used to construct the 	*
 * conditions wording statements.                       		*
 *                                                                      *
 * CONTENTS:                                                            *
 *   public( to the library) functions:                                 *
 *      af_condWording		- perform conditional wording based on	*
 *                                                                      *
 *   private functions:							*
 *      af_findOtlkSeq		- find snapshot sequence for outlook	*
 *	af_fromDvlpgWording 	- Wording for <FROM CONDS DVLPG>	*
 *	af_fromEndgWording 	- Wording for <FROM CONDS ENDG>		*
 *      af_contgBydWording	- wording for <OTLK CONDS CONTG BYD>	*
 *				  available snapshot information	*	
 *	af_otlkDvlpgWording 	- Wording for <OTLK CONDS DVLPG>	*
 *	af_otlkEndgWording 	- Wording for <OTLK CONDS ENDG>		*
 *	af_otlkGenWording 	- Wording for <GEN_OLK>			*
 *      af_processMAYBE		- process MAYBE cases in wording	*
 *      af_adjEmbedSSW		- adjust otlk SS type embedded in airmet*
 *	af_intAreaStatus 	- find if intersection of polygon with a*
 *				  bound is greater than /equal to 3k	*
 *	af_parseWording		- get start and end phrases from wording*
 *	af_genInstOtlk		- check in-state outlook generation rule*
 ***********************************************************************/

static void af_findOtlkSeq	( int nin, GFA_SS_Attr *ssw,
				 Boolean onTime, int *olkSeq );
                                                                        
static void af_fromDvlpgWording	( int nin, GFA_SS_Attr *ssw, 
				 Boolean onTime, char *wording );

static void af_fromEndgWording	( int nin,GFA_SS_Attr *ssw, 
				 Boolean onTime, char *wording );

static void af_contgBydWording	( int nin, GFA_SS_Attr *ssw, Boolean onTime, 
				 int index, int nfmt, GFA_Elem_Format *fmt,
				 int *olkSeq, char *wording );

static void af_otlkDvlpgWording	( GFA_SS_Attr *ssw, int *olkSeq, char *wording );

static void af_otlkEndgWording	( GFA_SS_Attr *ssw, Boolean onTime, 
				 int *olkSeq, char *wording );

static void af_otlkGenWording	( GFA_SS_Attr *ssw, int *olkSeq, char *genOlk );

static void af_processMAYBE( GFA_Elem_Format *fmt, Boolean *genOlk );

static void af_adjEmbedSSW	( int nin, GFA_SS_Attr *ssw, int index,
                            	int nfmt, GFA_Elem_Format *fmt,
			    	int *olkSeq, GFA_SS_Attr *ssw_embed );

static void af_intAreaStatus( gpc_polygon *poly, char *bounds, 
                             char *name, Boolean *flag, int *iret ); 

static void af_addOtlkSnapshots( VG_DBStruct ***el, int *nss, 
				 VG_DBStruct **elNoCancelOtlk, int nssOtlk );

static void af_dvlpgApplyIssueTm ( int xx, int yy, int basetime, char* issueTm,
			    GFA_Elem_Format *fmt, char * wording );

static void af_endgApplyIssueTm ( int xx, int yy, int basetime, char* issueTm,
			    GFA_Elem_Format *fmt, char * wording );

static void af_getHours ( char* wording, int* startHr, int* endHr );

static void af_genInstOtlk( GFA_Elem_Format *fmt, char *bounds, 
               char *name, float ratioIn, Boolean *flag, int *iret );

/*=====================================================================*/

void af_condsWording ( char *cycle, char *day, int issuance, int numFmt, 
			     GFA_Elem_Format *fmt, int *iret )
/************************************************************************
 * af_condsWording                                                     	*
 *                                                                      *
 * This routine determines the wording specific to a hazard based on	*
 * the avaliable snapshot information.                  		*
 *                                                                      *
 * void af_condsWording ( nin, fmt_in, iret )				*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *cycle		char		cycle				*
 *       numFmt		int     	Number of GFA format structures	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *fmt_in		GFA_Elem_Format Array of GFA format structures	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	03/06   Created                         	*
 * E. Safford/SAIC	04/06	fix G_MALLOC wording length bug		*
 * B. Yin/SAIC		05/06	set delete flag for false gen_olk olks  *
 * E. Safford/SAIC	05/06	avoid processing any FZLVL snapshots    *
 * J. Wu/SAIC          	07/06   adjust for outlook embeded in airmet	*
 * J. Wu/SAIC          	07/06   process MAYBE cases for outlooks	*
 * B. Yin/SAIC          07/06   change subtype (haz and category types) *
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * B. Yin/SAIC		10/06	ignore canceled snapshots.              *
 * J. Wu/SAIC          	10/06   Skip deleted format structures         	*
 * B. Yin/SAIC		10/06	add back 6/9/12 snapshots for AIRMET    *
 * J. Wu/SAIC          	10/06   do "CONTG BYD" wording only for AIRMETs	*
 * B. Yin/SAIC		10/06	fix bug when allocating ssw		*
 * E. Safford/SAIC	12/06	param change to af_useSnapshots		*
 * B. Yin/SAIC		03/07	account for issue time for wordings	*
 * J. Wu/SAIC          	04/07   check X_SNAPSHOTS intersecting smear 	*
 *			 	with an area > 3K			*
 * J. Wu/SAIC      	04/07	add a new rule - look for O only if no 	*
 *				X found inside the clipped FROM line	*
 * B. Yin/SAIC		12/07	add override-issue-time flag in GFA    	*
 * B. Yin               07/09   handle IFR->C&G issues                  *
 * L. Hinson/AWC        11/09   Fix call to ctb_gfagcat to fix issue    *
 *                              with Amendment Issue Times on C&V       *
 * L. Hinson/AWC        11/09   Fix amendment time issue on IFR         *
 ***********************************************************************/
{
    int 		ii, jj, kk, ier, smearFlag, nssOtlk, nhour;
    int			nss, base_time, olk_seq[3], adj_hr; 
    int			nCancel, nCancelOtlk, xx, yy;

    char		*pstr, rep[8], subtype[8];
    char		tmpstr[ ONEBLOCK ], tmp_wording[ ONEBLOCK ];
    char		wording[ ONEBLOCK ], hazType[32];
    char		issueTm[ 32 ], cat[ 32 ];
    
    char		*prev[]={"+00", "+01", "+02", "+03", "+04", 
    				 "+05", "+06", "+09", "+12" };
 
    Boolean		on_time, gen_olk, overrideIssueTm;
    
    GFA_SS_Attr		*ss_attr;
    VG_DBStruct		**elNoCancel, **elNoCancelOtlk;
    VG_DBStruct 	**elCancel, **elCancelOtlk;
 /*---------------------------------------------------------------------*/
    
    *iret = 0;

        
    /*
     *  Find the type of the forecast cycle - on-time or off-time
     *  "on-time" cycles - 03/09/15/21 for winter and 02/08/14/20 for summer.
     *                     snapshot sequence runs 00 to 06 for FROM line
     *                     snapshot sequence include 06, 09, 12 for OUTLOOK
     *  "OffTime" cycles - 00/06/12/18 for winter and 05/11/17/23 for summer. 
     *                     snapshot sequence runs 00 to 03 for FROM line
     *                     snapshot sequence include 03, 06, 09 for OUTLOOK
     *   Note: special hours may be issued for FROM line, but NOT for OUTLOOK.
     */
    base_time = atoi ( cycle );
    on_time = ( base_time == 3  || base_time == 9  ||
	        base_time == 15 || base_time == 21 ||
		base_time == 2  || base_time == 8  ||
	        base_time == 14 || base_time == 20 );
            
    /*
     *  Wording for each format structure
     */
    for ( ii = 0; ii < numFmt; ii++ ) {
        
	/* 
	 *  Skip deleted structures.
	 */
        if ( fmt[ii].delete == G_TRUE ) {
	    continue;
	}

	/*
	  *  Skip FZLVL and those hazards without snapshots.
	  */
	nss = fmt[ii].origInfo->nsnapshot;
	if ( nss <= 0 || strcmp( fmt[ii].origInfo->haz_type, "FZLVL" ) == 0 ) {
	    continue;
	}

	/* 
	 *  Remove canceled snapshots.
	 */
        cvg_getFld( &fmt[ii].el, TAG_GFA_SUBTYPE, subtype, &ier );
        smearFlag = atoi( subtype ) - atoi( subtype ) / 10 * 10;

        af_useSnapshots( fmt[ii].origInfo->snapshots,
			 fmt[ii].origInfo->nsnapshot,
			 smearFlag, &elNoCancel, &nss, 
			 &elCancel, &nCancel, &ier );

 	/*
	 *  Add back 6/9/12 snapshots for AIRMET because the end wording needs
	 *  all snapshots.
	 */
	if ( smearFlag == GFA_USER_SMEAR || smearFlag == GFA_SYSTEM_SMEAR ) {

           af_useSnapshots( fmt[ii].origInfo->snapshots,
			    fmt[ii].origInfo->nsnapshot,
			    GFA_SYSTEM_OUTLOOK, &elNoCancelOtlk, &nssOtlk, 
			    &elCancelOtlk, &nCancelOtlk, &ier );

	   af_addOtlkSnapshots( &elNoCancel, &nss, elNoCancelOtlk, nssOtlk );

	   G_FREE( elNoCancelOtlk, VG_DBStruct* );
	   G_FREE( elCancelOtlk, VG_DBStruct* );

	}
        

        /*
         *  Group snapshots with their forecast hours and determine each group's
         *  attributes - forecast hour,  number of snopshots in group , start index, 
         *  each snapshot's type (X or O),  type of the group, hour for
         *  development wording, and hour for ending wording.  
         *   
         *  The type of the group is determined as:
         *      O_SNAPSHOTS - if all snapshots in the group are type O;  
         *      X_SNAPSHOTS - if at least one of the snapshots in the group is type X.
         *   
         *  The hours of the development/ending wording are computed as:  
         *      Round UP   - X_SNAPSHOTS for development wording or
         *                   O_SNAPSHOTS for ending wording 
         *      Round Down - X_SNAPSHOTS for ending wording or
         *                   O_SNAPSHOTS for development wording 
         */
	G_MALLOC ( ss_attr, GFA_SS_Attr, nss, "af_applyAttr: ss_attr" );	    
        for ( jj = 0; jj < nss; jj++ ) {
	    G_MALLOC ( ss_attr[jj].ssw, GFA_SS_Wording, nss, "af_condsWording: ss_attr.ssw" );
	}
	
	af_getSSAttr ( nss, elNoCancel, fmt[ii].el_poly, &nhour, ss_attr, &ier );		
              
	
	/* 
	  * Find the outlook SS sequence in ssw for the cycle. 
	  *
	  */
        af_findOtlkSeq ( nhour, ss_attr, on_time, olk_seq );	

	
	/*
	 *  Get the issue time.
	 */
	overrideIssueTm = False;

	if ( strcasecmp( fmt[ii].origInfo->haz_type, "IFR" ) == 0) {
	       strcpy( hazType, "C&V" );
	}
	else {
	       strcpy( hazType, fmt[ii].origInfo->haz_type );
	}
        
        ctb_gfagcat ( hazType, cat, &ier );
        if (ier != 0 && strcasecmp( fmt[ii].origInfo->haz_type, "IFR" ) == 0) {
          ctb_gfagcat( "IFR", cat, &ier );
        }

        overrideIssueTm = af_overrideIssueTm( fmt[ii].area, cat,
	    		        numFmt, fmt, &ier );

	af_getIssueTm ( cycle, day, overrideIssueTm, issueTm, &ier );

    	cvg_setFld ( &(fmt[ ii ].el), TAG_GFA_OVERRIDE_TM, 
			(overrideIssueTm)?"1":"0", &ier );
	
	/* 
	  * FROM CONDS DVLPG wording
	  */	    	    
	af_fromDvlpgWording ( nhour, ss_attr, on_time, tmpstr );

	if ( ( issuance == 1 ) && ( strlen(tmpstr) != 0 ) ) {

	   af_getHours( tmpstr, &xx, &yy );

	   if ( ( xx >= 0 ) && ( yy >= 0 ) ) {

		af_dvlpgApplyIssueTm ( xx, yy, base_time, &issueTm[2], 
					&(fmt[ ii ]), wording );


		if ( strlen( wording ) != 0 ) {

	   		sprintf( tmpstr, "CONDS DVLPG %s", wording );

 		}
		else {
			tmpstr[ 0 ] ='\0';

		}

	   }
		
	}

    	strcpy ( tmp_wording, "<fromCondsDvlpg>" );
    	strcat ( tmp_wording, tmpstr );
    	strcat ( tmp_wording, "</fromCondsDvlpg>" );
	                        	  	    
	
	/* 
	  * FROM CONDS ENDG wording 
	  */
        strcat ( tmp_wording, "<fromCondsEndg>");
        af_fromEndgWording ( nhour, ss_attr, on_time, tmpstr );
	
	if ( ( issuance == 1 ) && ( strlen( tmpstr ) != 0 ) ) {

	   af_getHours( tmpstr, &xx, &yy );

	   if ( ( xx >= 0 ) && ( yy >= 0 ) ) {

		af_endgApplyIssueTm ( xx, yy, base_time, &issueTm[2], 
				&(fmt[ ii ]), wording );

		if ( strlen( wording ) != 0 ) {

	   		if ( strcmp( wording, "CONDS HV ENDED" ) == 0 ) {

				strcpy( tmpstr, "CONDS HV ENDED" );

	   		}
	   		else {

	        		sprintf( tmpstr, "CONDS ENDG %s", wording );

	   		}

 		}
		else {

			tmpstr[ 0 ] ='\0';

		}
	   }

	}

    	strcat ( tmp_wording, tmpstr );
        strcat ( tmp_wording, "</fromCondsEndg>");
	
		
	/* 
	  * GEN_OLK wording 
	  */
    	strcat ( tmp_wording, "<genOlk>" );
	af_otlkGenWording ( ss_attr, olk_seq, tmpstr );            
        
	if ( strcmp ( tmpstr, "MAYBE" ) == 0 ) {
	    af_processMAYBE( &fmt[ii], &gen_olk );
	    if ( gen_olk ) {
    	        strcpy ( tmpstr, "YES" );	    
	    }
	    else {
    	        strcpy ( tmpstr, "NO" );	    	    
	    }
        }    	
   	
    	strcat ( tmp_wording, tmpstr );
    	strcat ( tmp_wording, "</genOlk>" );
	
 	/* 
	  *  Check if OUTLOOK should be generated 
	  */
	if ( strcmp ( tmpstr, "NO" ) == 0 ) {
            gen_olk = False;
	}
	else {
            gen_olk = True;	
	} 
	
	/*
	 *  Set the delete flag for outlooks with a false gen_olk
	 *  so as to skip it when generating xml strings.
	 */
        if ( !gen_olk && ( smearFlag == GFA_SYSTEM_OUTLOOK ||
		           smearFlag == GFA_USER_OUTLOOK ) )  {

	   fmt[ ii ].delete = True;

	}
			   
			   
	/* 
	  * OTLK CONDS CONTG BYD wording 
	  */
    	strcat ( tmp_wording, "<condsContg>");
	if ( smearFlag == GFA_SYSTEM_SMEAR || smearFlag == GFA_USER_SMEAR ) {
	    af_contgBydWording ( nhour, ss_attr, on_time, ii, numFmt, fmt, olk_seq, tmpstr );
    	    strcat ( tmp_wording, tmpstr );
	}
	strcat ( tmp_wording, "</condsContg>");
	
	
	/* 
	  * OTLK CONDS DVLPG wording 
	  */
    	strcat ( tmp_wording, "<otlkCondsDvlpg>");
        if ( gen_olk ) {
	    af_otlkDvlpgWording ( ss_attr, olk_seq, tmpstr );
    	    strcat ( tmp_wording, tmpstr );
    	}
	strcat ( tmp_wording, "</otlkCondsDvlpg>");

	
	/* 
	  * OTLK CONDS ENDG wording 
	  */
    	strcat ( tmp_wording, "<otlkCondsEndg>");
        if ( gen_olk ) {
	    af_otlkEndgWording ( ss_attr, on_time, olk_seq, tmpstr );	
    	    strcat ( tmp_wording, tmpstr );
    	}
	strcat ( tmp_wording, "</otlkCondsEndg>");
	
			
	/* 
	  * Adjust the wording according by adding the base time to the 
	  * forecast and performing mod to hours >= 24.
	  */        
	for ( jj = 0; jj < 9; jj++ ) {	    
	    adj_hr = atoi( &prev[ jj ][1] ) + base_time;
	    adj_hr = adj_hr - ( adj_hr / 24 ) * 24;
	    if ( adj_hr == 0 ) {
	        strcpy ( rep, "00" );
	    }
	    else if ( adj_hr < 10 ) {
	        sprintf ( rep, "0%d", adj_hr );
	    }
	    else {
	        sprintf ( rep, "%d", adj_hr );	    
	    }
	    	    
	    pstr = (char *) strstr ( tmp_wording, prev[ jj ] );
	    while ( pstr != (char *)NULL ) {
	        cst_rpst ( tmp_wording, prev[ jj ], rep, tmpstr, &ier );    
	        strcpy ( tmp_wording, tmpstr );	    
	        pstr = (char *) strstr ( tmp_wording, prev[ jj ] );
	    }	    
	}
	

	/* 
	  * Put the XML string into GFA fromat structure
	  */        
 	G_MALLOC ( fmt[ii].wording, char, strlen( tmp_wording ) + 1, 
	             "af_condsWording: fmt[ii].wording" );	    
        strcpy ( fmt[ii].wording, tmp_wording );
        	
	
	
	/* 
	  * Free memory.
	  */	
	for ( kk = 0; kk < nss; kk++ ) {
	    G_FREE ( ss_attr[ kk ].ssw, GFA_SS_Wording );
        }
	G_FREE ( ss_attr, GFA_SS_Attr );
	
	G_FREE ( elNoCancel, VG_DBStruct* );
	G_FREE ( elCancel, VG_DBStruct* );
		    
    }

}

/*=====================================================================*/

static void af_findOtlkSeq ( int nin, GFA_SS_Attr *ssw,
			    Boolean onTime, int *olkSeq )
/************************************************************************
 * af_findOtlkSeq                                        		*
 *                                                                      *
 * Finds the outlook snapshots sequence in the GFA_SS_Attr array.	*
 *                                                                      *
 * static void af_findOtlkSeq ( nin, ss, onTime, olkSeq )  		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nin		int		Number of GFA_SS_Attr elems	*
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	onTime		Boolean		True - 03/09/15/21 for winter	*
 * 					       02/08/14/20 for summer	*
 * 					False - all other cycles	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*olkSeq		char		Outlook SS Sequence in ssw	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{    
    int		ii, start_hour; 
/*---------------------------------------------------------------------*/
    
    for ( ii = 0; ii < 3; ii++ ) {
        olkSeq [ ii ] = -1;
    }
    
    if ( nin <= 0 )  return;
       
    /*
     *  Cycle rules: 
     *  "OnTime" cycles  - outlook SS sequence 06, 09, 12;
     *  "OffTime" cycles - outlook SS sequence 03, 06, 09;
     *
     *   Note: outlook ONLY include snapshots with the above specified
     *         forcast hours. 
     */
    if ( onTime ) { 
	start_hour = 6;
    }
    else {
	start_hour = 3;
    }
         
    for ( ii = 0; ii < nin; ii++ ) {
        if ( ssw [ ii ].minute == 0  ) {
	    if  ( ssw [ ii ].hour == start_hour ) {
	        olkSeq[ 0 ] = ii;
	    }
	    else if ( ssw [ ii ].hour == ( start_hour + 3 ) ) {
	        olkSeq[ 1 ] = ii;	    
	    }
	    else if ( ssw [ ii ].hour == ( start_hour + 6 ) ) {
	        olkSeq[ 2 ] = ii;
	    }
        }
    }
}

/*=====================================================================*/


static void af_fromDvlpgWording	( int nin, GFA_SS_Attr *ssw,
				Boolean onTime, char *wording )
/************************************************************************
 * af_fromDvlpgWording                                        		*
 *                                                                      *
 * Generates "<FROM CONDS DVLPG>" wording.				*
 *                                                                      *
 * static void af_fromDvlpgWording ( nin, ss, onTime, wording )      	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nin		int		Number of GFA_SS_Attr elems	*
 * 	*ssw		GFA_SS_Attr	Array of GFA_SSAttr elems	*
 * 	onTime		Boolean		True - cycle 03/09/15/21	*
 * 					False - cycle 00/06/12/18	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*wording	char		Wording string      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	04/06	rm "FROM" from conditions wording	*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{    
    int		ii, end_hour, first_ss_hr, last_O;
/*---------------------------------------------------------------------*/
    
    wording[0] = '\0';
           
    if ( onTime ) { 
	end_hour = 6;
	    }
    else {
	end_hour = 3;	    
    }
	        
    /*
     *  Wording rules:
     *  If the first ss in the sequence is an 'X_SNAPSHOTS' {
     *      If the first ss time is 00, there is no wording.
     *      If the first ss time is 03 or less, wording is "+00-+TTZ", 
     *           where 'TT' is the ss time.
     *      If the first ss time is 06 or less (on-time cycles only), 
     *           wording is "+03-+TTZ", where 'TT' is the ss time.
     *  }
     *   
     *  If the first ss is an 'O_SNAPSHOTS' {
     *      Scan the sequence forward from the first ss to find the 
     *      last 'O' before encountering an 'X' ss.
     *      The wording is "AFT +TTZ" where 'TT' is this 'O' ss time.
     *  }
     */    
    first_ss_hr = ssw [ 0 ].dvlpg_hr;
    if ( ssw [ 0 ].type == X_SNAPSHOTS ) {
	        
	if ( first_ss_hr == 0 ) {
	    /* No wording */
	}
	else if ( first_ss_hr <= 3 ) {
            sprintf ( wording, "CONDS DVLPG +00-+0%dZ", first_ss_hr );
	}
	else if ( first_ss_hr <= 6 ) {
	    sprintf ( wording, "CONDS DVLPG +03-+0%dZ", first_ss_hr );
	}   	     
    }
    else {
	last_O = 0;
	for ( ii = 1; ii < nin; ii++ ) {
            if ( ssw [ ii ].dvlpg_hr <= end_hour &&
		 ssw [ ii ].type == X_SNAPSHOTS ) {
		last_O = ii - 1;
		break;
	    }  
	}
	    
	sprintf ( wording, "CONDS DVLPG AFT +0%dZ", ssw [ last_O ].dvlpg_hr );
    }
}

/*=====================================================================*/

static void af_fromEndgWording ( int nin, GFA_SS_Attr *ssw,
				Boolean onTime, char *wording )
/************************************************************************
 * af_fromEndgWording                                        		*
 *                                                                      *
 * Generates "<FROM CONDS ENDG>" wording.				*
 *                                                                      *
 * static void af_fromEndgWording ( nin, ssw, onTime, wording )		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nin		int		Number of GFA_SS_Attr elems	*
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	onTime		Boolean		True - cycle 03/09/15/21	*
 * 					False - cycle 00/06/12/18	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*wording	char		Wording string      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC      04/06   rm "FROM" from conditions wording       *
 * B. Yin/SAIC		05/06	change "<=3" to "<3"			*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{
    int         jj, end_hour, last_SS, last_ss_hr, first_O;
/*---------------------------------------------------------------------*/
                                                                                
    wording[0] = '\0';
                                                                       
    if ( onTime ) {
        end_hour = 6;
            }
    else {
        end_hour = 3;
    }
                                                                       
                                                                                
    /*
     * Wording Rules:
     * 
     * If last ss in the sequence is an 'X_SNAPSHOTS' {
     *     If the last ss time is the last possible time (06 for on-time cycles 
     *           or 03 for off-time cycles), wording is determined by the outlook
     *           component snapshots rules. See the 'Outlook BOUNDED BY lines' 
     *           section below for "CONDS CONTG BYD +06Z" wording 
     *           (or "CONDS CONTG BYD +03Z" for cycles 00, 06, 12 and 18).
     *     If the last ss time is less than 03, wording is "+TT-+03Z", 
     *            where 'TT' is the ss time.
     *     If the last ss time is less than 06 (on-time cycles only), 
     *             wording is "+TT-+06Z", where 'TT' is the ss time.
     * }
     * 
     * If last ss is an 'O_SNAPSHOT' {
     *     Scan the sequence backward from the last ss to find the first 'O' 
     *     after encountering an 'X' ss. The wording is "BY +TTZ" where 'TT' 
     *     is this 'O' ss time.
     * }
     * 
     */
    last_SS = nin - 1;
    while ( last_SS > 0 && ssw [ last_SS ].endg_hr > end_hour ) {
       last_SS--; 
    }
            
    last_ss_hr = ssw [ last_SS ].endg_hr;    
    if ( ssw [ last_SS ].type == X_SNAPSHOTS ) {
        if ( ssw [ last_SS ].hour == end_hour &&
             ssw [ last_SS ].minute == 0 ) {
             /* Handled separately by af_contgBydWording() */
        }
        else {
            if ( last_ss_hr < 3 ) {
                sprintf ( wording, "CONDS ENDG +0%d-+03Z", last_ss_hr );
            }
            else if ( last_ss_hr <= 6 ) {
                sprintf ( wording, "CONDS ENDG +0%d-+06Z", last_ss_hr );
            }
        }
    }
    else {
        first_O = last_SS;
        for (  jj = last_SS; jj >= 0 ; jj-- ) {
            if ( ssw [ jj ].type == X_SNAPSHOTS ) {
                first_O = jj + 1;
                break;
            }
        }

        sprintf ( wording, "CONDS ENDG BY +0%dZ", ssw [ first_O ].endg_hr );
    }
    
}

/*=====================================================================*/

static void af_contgBydWording	( int nin, GFA_SS_Attr *ssw, Boolean onTime,
				 int index, int nfmt, GFA_Elem_Format *fmt,
				 int *olkSeq, char *wording )
/************************************************************************
 * af_contgBydWording                                        		*
 *                                                                      *
 * Generates "<OTLK CONDS CONTG BYD>" wording.				*
 *                                                                      *
 * static void af_contgBydWording ( ssw, onTime, olkSeq, wording )	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nin		int		number of GFA_SS_Attr elems 	*
 * 	*ssw		GFA_SS_Wording	Array of GFA_SS_Attr elems	*
 * 	onTime		Boolean		True  - 03/09/15/21 for winter	*
 * 					        02/08/14/21 for summer	*
 * 					False - all other cycles	*
 * 	index		int		Index of the GFA format elm 	*
 * 	nfmt		int		Total number of GFA format elms	*
 *      *fmt_in		GFA_Elem_Format Array of GFA format structures	*
 * 	*olkSeq		int		Outlook SS seqence in ssw	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*wording	char		Wording string      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06 	initial coding				*
 * E. Safford/SAIC	04/06	rm "OTLK" from conditions wording	*
 * J. Wu/SAIC      	07/06 	adjust type of otlk ss embeded in airmet*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 * J. Wu/SAIC      	01/08	word only if 6h exists as X with AIRMET *
 ***********************************************************************/
{    
    int		last_SS, first_X, first_O, ii, nss;
    
    GFA_SS_Attr		*ssw_olk;
/*---------------------------------------------------------------------*/

    wording[0] = '\0';

    /*
     *  Proceed only if the 06 ss exists ( 03 ss for off-time cycles ) 
     *  as an "X_SNAPSHOTS" with the AIRMET (not the OUTLOOK). 
     */
    if ( olkSeq[ 0 ] < 0 || ssw[ olkSeq[ 0 ] ].type != X_SNAPSHOTS ||
	 ssw [ olkSeq[ 0 ] ].minute != 0 ) {
        return;
    }

    nss = fmt[index].origInfo->nsnapshot;
    G_MALLOC ( ssw_olk, GFA_SS_Attr, nin, "af_contgBydWording: ssw_olk" ); 
    for ( ii = 0; ii < nin; ii++ ) {
	G_MALLOC ( ssw_olk[ii].ssw, GFA_SS_Wording, nss, 
	                              "af_contgBydWording: ssw_olk.ssw" );
    }
                
    
    /*
     *  Find the types of outlook snapshots embeded in the airmet.
     */
    af_adjEmbedSSW ( nin, ssw, index, nfmt, fmt, olkSeq, ssw_olk );       
            
    
    /*
     * Wording Rules:
     *
     *  Proceed only if the 06 ss exists ( 03 ss for off-time cycles ) 
     *  as an "X_SNAPSHOTS". 
     *
     *  If the last ss in the sequence is an 'X' {
     *      If its time is 12, wording - "CONDS CONTG BYD +0NZ THRU +12Z".
     *      If its time is 09, wording - "CONDS CONTG BYD +0NZ ENDG +09-+12Z".
     *      If its time is 06, wording - "CONDS CONTG BYD +0NZ ENDG +06-+09Z".
     *      If its time is 03, wording - "CONDS CONTG BYD +0NZ ENDG +03-+06Z".
     *  }
     *
     *  If the last ss in the sequence is an 'O' {
     *	    Scan the sequence backward from the last ss to find the first 'O' 
     *      after encountering an 'X' ss. 
     *      
     *      If this 'O' ss time is 12, wording - "CONDS CONTG BYD +0NZ ENDG BY +12Z".
     *      If this 'O' ss time is 09, wording - "CONDS CONTG BYD +0NZ ENDG BY +09Z".
     *  }    
     *      
     *  Where "N" is "6" for on-time cycle and "3" for off-time cycle. 
     *     
     */
    if ( olkSeq[ 0 ] < 0 || ssw_olk [ olkSeq[ 0 ] ].type != X_SNAPSHOTS ||
	 ssw_olk [ olkSeq[ 0 ] ].minute != 0 ) {
        
	for ( ii = 0; ii < nin; ii++ ) {
	    G_FREE ( ssw_olk[ii].ssw, GFA_SS_Wording );
        }
	
	G_FREE ( ssw_olk, GFA_SS_Attr );
        return;
    }
	       
    if ( olkSeq[ 2 ] >= 0 )  {
        last_SS = 2;
    }
    else if ( olkSeq[ 1 ] >= 0 ) {
        last_SS = 1;
    }
    else {
        last_SS = 0; 
    }
                
                    
    if ( ssw_olk  [ olkSeq[ last_SS ] ].type == X_SNAPSHOTS ) {

	if ( ssw_olk  [ olkSeq[ last_SS ] ].hour == 12 ) {
	    strcpy ( wording, "CONDS CONTG BYD +06Z THRU +12Z");	    
	}
	else if ( ssw_olk  [ olkSeq[ last_SS ] ].hour == 9 ) {
	    if ( onTime ) {
	    strcpy ( wording, "CONDS CONTG BYD +06Z ENDG +09-+12Z");	    
	}	    
	    else {
	        strcpy ( wording, "CONDS CONTG BYD +03Z THRU +09Z");	    	    
	    }
	}	    
	else if ( ssw_olk  [ olkSeq[ last_SS ] ].hour == 6 ) {
	    if ( onTime ) {
	    strcpy ( wording, "CONDS CONTG BYD +06Z ENDG +06-+09Z");	    
	}	    
	    else {
	        strcpy ( wording, "CONDS CONTG BYD +03Z ENDG +06-+09Z");	    	    
	    }
	}	    
	else if ( ssw_olk  [ olkSeq[ last_SS ] ].hour == 3 ) {
	    strcpy ( wording, "CONDS CONTG BYD +03Z ENDG +03-+06Z");	    
	}	    
    }
    else {
        
        first_O = last_SS;	    
	
	first_X = -1;	
	for ( ii = last_SS; ii >= 0; ii-- ) {
	    if ( olkSeq[ ii ] >= 0 && 
	        ssw_olk  [ olkSeq[ ii ] ].type == X_SNAPSHOTS ) {
	        first_X = ii;
		break;
	    }
	}
	
	if ( first_X >= 0 ) {
	    for ( ii = first_X + 1; ii <= last_SS; ii++ ) {
	        if ( olkSeq[ ii ] >= 0 && 
		     ssw_olk  [ olkSeq[ ii ] ].type == O_SNAPSHOTS ) {
		    first_O = ii;
		    break;
		}
	    }
	}
	
	if ( ssw_olk  [ olkSeq[ first_O ] ].hour == 12 ) {
	    strcpy ( wording, "CONDS CONTG BYD +06Z ENDG BY +12Z");
	}
	else if ( ssw_olk  [ olkSeq[ first_O ] ].hour == 9 ) {
	    if ( onTime ) {
	        strcpy ( wording, "CONDS CONTG BYD +06Z ENDG BY +09Z");        
	    }
	    else {
	        strcpy ( wording, "CONDS CONTG BYD +03Z ENDG BY +09Z");	    
	    }
	}
        else if ( ssw_olk  [ olkSeq[ first_O ] ].hour == 6 ) {
	    if ( !onTime ) {
	        strcpy ( wording, "CONDS CONTG BYD +03Z ENDG BY +06Z");        
	    }
        }    
    }
    
    
    for ( ii = 0; ii < nin; ii++ ) {
	G_FREE ( ssw_olk[ii].ssw, GFA_SS_Wording );
    }

    G_FREE ( ssw_olk, GFA_SS_Attr );
}

/*=====================================================================*/

static void af_otlkDvlpgWording	( GFA_SS_Attr *ssw,
				 int *olkSeq, char *wording )
/************************************************************************
 * af_otlkDvlpgWording                                        		*
 *                                                                      *
 * Generates "<OTLK CONDS DVLPG>" wording.				*
 *                                                                      *
 * static void af_otlkDvlpgWording ( ssw, olkSeq, wording )		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	*olkSeq		int		Outlook SS seqence in ssw	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*wording	char		Wording string      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	04/06	rm "OTLK" from conditions wording	*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{    
    int		ii, first_SS, last_O; 
/*---------------------------------------------------------------------*/

    wording[0] = '\0';
    
    if ( olkSeq[ 0 ] < 0 && olkSeq[ 1 ] < 0 && olkSeq[ 2 ] < 0 ) {
        return;
    } 
           
    /*
     * Wording Rules:
     * 
     * If the first ss in the sequence is an 'X' {
     *     If first ss time is 06, there is no wording.
     *     If first ss time is 09, wording is "+06-+09Z".
     *     If first ss time is 12, wording is "+09-+12Z".
     * }
     * If the first ss is an 'O' {
     *     Scan the sequence forward from the first ss to find the last 'O' 
     *     before encountering an 'X' ss. The wording is "AFT +TTZ" where 'TT' 
     *     is this 'O' ss time.
     * } 
     *
     */   
    for ( ii = 0; ii < 3; ii++ ) {
        if ( olkSeq[ ii ] >= 0 ) { 
            first_SS = olkSeq[ ii ];
	    break;        
        }
    }
        
    if ( ssw [ first_SS ].type == X_SNAPSHOTS ) {
	if ( first_SS == olkSeq[ 0 ] ) {
	     /* No wording */    
	}
	else if ( ssw [ first_SS ].hour == 6 ) {
	    strcpy ( wording, "CONDS DVLPG +03-+06Z" );
	}
	else if ( ssw [ first_SS ].hour == 9 ) {
	    strcpy ( wording, "CONDS DVLPG +06-+09Z" );	    
	}
	else if ( ssw [ first_SS ].hour == 12 ) {
	    strcpy ( wording, "CONDS DVLPG +09-+12Z" );
	}
    }
    else {
	last_O = first_SS;
	for ( ii = 0; ii < 2; ii++ ) {	
	   if ( last_O == olkSeq[ ii ] && olkSeq[ ii + 1 ] >= 0 && 
	       ssw[ olkSeq[ ii + 1 ] ].type == O_SNAPSHOTS ) {
	       last_O = olkSeq[ ii + 1 ];
	   }	    
	} 
	
	if ( ssw[ last_O ].hour < 10 ) {
	    sprintf ( wording, "CONDS DVLPG AFT +0%dZ", ssw[ last_O ].hour );
	}
	else {
	    sprintf ( wording, "CONDS DVLPG AFT +%dZ", ssw[ last_O ].hour );
	}            	        	        
    }   
}

/*=====================================================================*/

static void af_otlkEndgWording	( GFA_SS_Attr *ssw, Boolean onTime,
				 int *olkSeq, char *wording )
/************************************************************************
 * af_otlkEndgWording                                        		*
 *                                                                      *
 * Generates "<OTLK CONDS ENDG>" wording.				*
 *                                                                      *
 * static void af_otlkEndgWording ( ssw, onTime, olkSeq, wording )	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	onTime		Boolean		True  - 03/09/15/21 for winter	*
 * 					        02/08/14/21 for summer	*
 * 					False - all other cycles	*
 * 	*olkSeq		int		Outlook SS seqence in ssw	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*wording	char		Wording string      		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	04/06	rm "OTLK" from conditions wording	*
 * J. Wu/SAIC      	06/06	account for two missing cases		*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{    
    int		last_SS, first_X, first_O, ii; 
/*---------------------------------------------------------------------*/
	    
    wording[0] = '\0';
        
    if ( olkSeq[ 0 ] < 0 && olkSeq[ 1 ] < 0 && olkSeq[ 2 ] < 0 ) {
        return;
    } 
     	           
    
    /*
     *  Wording Rules:
     *  
     *  If last ss in the sequence is an 'X' {
     *      If its time is 12 (09 for off-time), wording is "CONTG THRU +12Z" 
     *           ("CONTG THRU +09Z" for off-time cycle).
     *      If its time is 09, wording is "ENDG +09-+12Z" for on-time cycle and 
     *           "CONTG THRU +09Z" for off-time cycle.
     *      If its time is 06, wording is "ENDG +03-+06Z" for off-time cycle.
     *  }
     * 
     *  If last ss is an 'O' {
     *      Scan the sequence backward from the last ss to find the first 'O' 
     *      after encountering an 'X' ss. The wording is "ENDG BY +TTZ" 
     *      where 'TT' is this ss time.
     *      
     *      If this time is 12 (09 for off-time cycle).
     *      If this time is 09 (06 for off-time cycle), no wording.
     *  }
     *
     */
    if ( olkSeq[ 2 ] >= 0 ) {
        last_SS = 2;
    }
    else if ( olkSeq[ 1 ] >= 0 ) {
        last_SS = 1;
    }
    else {
        last_SS = 0;
    }
    
    
    if ( ssw [ olkSeq[ last_SS ] ].type == X_SNAPSHOTS ) {
	if ( ssw [ olkSeq[ last_SS ] ].hour == 12 ) {
	    strcpy ( wording, "CONDS CONTG THRU +12Z" );
	}
	else if ( ssw [ olkSeq[ last_SS ] ].hour == 9 )  {
	    if ( onTime ) {
	        strcpy ( wording, "CONDS ENDG +09-+12Z" );	
	    }
	    else {
	        strcpy ( wording, "CONDS CONTG THRU +09Z" );	    
	    }
	}
	else if ( ssw [ olkSeq[ last_SS ] ].hour == 6 )  {
	    if ( !onTime ) {
	        strcpy ( wording, "CONDS ENDG +06-+09Z" );	
	    }
	}
    }
    else {	
	
	first_O = last_SS;
	
	first_X = -1;	
	for ( ii = last_SS; ii >= 0; ii-- ) {
	    if ( olkSeq[ ii ] >= 0 && 
	         ssw [ olkSeq[ ii ] ].type == X_SNAPSHOTS ) {
	        first_X = ii;
		break;
	    }
	}
	
	if ( first_X >= 0 ) {
	    for ( ii = first_X + 1; ii <= last_SS; ii++ ) {
	        if ( olkSeq[ ii ] >= 0 && 
		     ssw [ olkSeq[ ii ] ].type == O_SNAPSHOTS ) {
		    first_O = ii;
		    break;
	    }
	}    
    }        


	if ( ssw [ olkSeq[ first_O ] ].hour == 12 ) {
	    strcpy ( wording, "CONDS ENDG BY +12Z" );		        
	}
	else if ( ssw [ olkSeq[ first_O ] ].hour == 9 && !onTime ) {
	    strcpy ( wording, "CONDS ENDG BY +09Z" );		        
	} 	
    }        
}

/*=====================================================================*/

static void af_otlkGenWording ( GFA_SS_Attr *ssw, int *olkSeq, 
				char *genOlk )
/************************************************************************
 * af_otlkGenWording                                        		*
 *                                                                      *
 * Generates "<GEN_OLK>" wording ("YES", "NO", or "MAYBE").		*
 *                                                                      *
 * static void af_otlkGenWording ( ss, olkSeq, genOlk )  		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	*olkSeq		int		Outlook SS seqence in ssw	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*genOlk		char		Wording for generating outlook	*
 * 					"NO" - do not generate outlook	*
 * 					"YES" - generate outlook	*
 * 					"MAYBE" - more criteria	needed	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	09/06	add "MAYBE" cases X-O-X and X---X	*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 ***********************************************************************/
{    
/*---------------------------------------------------------------------*/
	               
    /*
     *  Wording Rules:
     *  Assume there is at least one SS in the sequence. 
     *  1SS - 06 ss (03 for off-time cycles)
     *  2SS - 09 ss (06 for off-time cycles)
     *  3SS - 12 ss (09 for off-time cycles)
     *   
     *  If 1SS does not exist or exists as an O_SNAPSHOTS {
     *      <GEN OTLK> is "YES"
     *  }
     *   
     *  If both 1SS and 2SS exist as X_SNAPSHOTSs {
     *      <GEN OTLK> is "MAYBE"
     *  }
     *
     *  If 1SS & 3SS exist X_SNAPSHOTSs {
     *      <GEN OTLK> is "MAYBE"  (X-O-X case and X---X case)
     *  }
     *   
     *  All other cases - <GEN OTLK> is "NO" 
     */        
    strcpy ( genOlk, "NO" );
        
    if ( olkSeq[ 0 ] >= 0 || olkSeq[ 1 ] >= 0 || olkSeq[ 2 ] >= 0 ) {
        if ( olkSeq[ 0 ] < 0 || ( olkSeq[ 0 ] >= 0 &&
	                          ssw [ olkSeq[ 0 ] ].type == O_SNAPSHOTS ) ) {
            strcpy ( genOlk, "YES" );
	}
	else {
            if ( olkSeq[ 1 ] >= 0 && ssw [ olkSeq[ 1 ] ].type == X_SNAPSHOTS ) {	 
                strcpy ( genOlk, "MAYBE" );	    
	    }
	    else if ( olkSeq[ 2 ] >= 0 && ssw [ olkSeq[ 2 ] ].type == X_SNAPSHOTS ) {
                strcpy ( genOlk, "MAYBE" );	    	    
	    }
	}	     	
    }
}

/*=====================================================================*/

static void af_processMAYBE( GFA_Elem_Format *fmt, Boolean *genOlk ) 
/************************************************************************
 * af_processMAYBE                                       		*
 *                                                                      *
 * Processes the MAYBE conditions for an outlook to determine whether it*
 * gets generated or not.  MAYBE conditons fall under those cases where	*
 * both the 6- and 9- hr snapshots exist and intersect with the outlook	*
 * (on-time, off-time will be 3- and 6- hour).                 		*
 *                                                                      *
 * static void af_processMAYBE ( fmt, genOlk )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fmt		GFA_Elem_Format Format structure of the	outlook	*
 *					in question			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*genOlk		Boolean	 	True  - change "MAYBE" to "YES"	* 
 * 					False - change "MAYBE" to "NO"	* 
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	07/06 	initial coding				*
 * J. Wu/SAIC      	08/06 	make sure outlook & airmet exist	*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * J. Wu/SAIC      	12/07 	add in-state outlook generation rule	*
 ***********************************************************************/
{    
    int		ier, np; 
    float	areaRatio; 
    char	hazType[8], otlkArea[4], adjArea[4], tag[32];
    char	otlkStateList[ STD_STRLEN ] = "";
    char	airmStateList[ STD_STRLEN ] = "";
    char	bndsTag[32], *sptr, stateName[10];
    char	*cstlPtr;
    VG_DBStruct *otlkEl, *airmEl;
    Boolean	cstlWaterExist;
/*---------------------------------------------------------------------*/
    
    /*
     *  Default the flag to False.
     */
    *genOlk = False;
    

    /*
     *  Safety check: both outlook and airmet should exist.
     */
    if ( !( &(fmt->el) ) || !(fmt->origInfo->smear) ) {
        return;
    }


    /*
     *  Must have a specified area.
     */
    if ( strlen( fmt->area ) <= 0 ) {
        return;
    }
    

    /*
     *  Generate the state list for the outlook
     */
    strcpy( hazType, fmt->origInfo->haz_type );
    strcpy( otlkArea, fmt->area );
    strcpy( adjArea, fmt->adjarea);

    otlkEl = &(fmt->el);
    np = otlkEl->elem.gfa.info.npts;
        
    af_fmtStateList( hazType, np, otlkEl->elem.gfa.latlon,
                     &otlkEl->elem.gfa.latlon[ np ],
                     otlkArea, adjArea, otlkStateList, &ier );
    
    if( ier < 0 || strlen( otlkStateList ) <= 0 ) {
        return;
    }
    
    cstlPtr = strstr( otlkStateList, "AND" );
    cstlWaterExist = False;
    if ( cstlPtr ) {
        cstlPtr[0] = '\0';
        cstlWaterExist = True;
    }

    
    /*
     *  Generate the state list for the ORIGINAL airmet
     */
    airmEl = fmt->origInfo->smear;
    np = airmEl->elem.gfa.info.npts;
            
    af_fmtStateList( hazType, np, airmEl->elem.gfa.latlon,
                     &airmEl->elem.gfa.latlon[ np ],
                     otlkArea, adjArea, airmStateList, &ier );
    
    cstlPtr = strstr( airmStateList, "AND" );
    if ( cstlPtr ) {
        cstlPtr[0] = '\0';
    }

    if( ier < 0 || strlen( airmStateList ) <= 0 ) {
        return;
    }    

        
    /*
     *  Rule 1: For states in the outlook's state list but not listed in the
     *  airmet's state list, check the sizes of the intersection areas
     *  between the outlook with those states.  If there is at least
     *  one of them is greater than or equal to 3K, set the flag to True 
     *  to issue the outlook.  
     *
     *  Rule 2: For states both in the outlook's state list and the airmet's state
     *  list, check the in-state outlook generation ratio with the threshold
     *  set in the prefs.tbl.  If it's greater than the threshold, issue the
     *  outlook. 
     *     
     *  Both checks should be done for the state bound,  the Great Lakes
     *  bound, and the coastal water bound.
     */
    areaRatio = 0.25;
    ctb_pfstr ( "GFA_OTLKGEN_RATIO", tag, &ier );
    if ( ier == 0 ) {
        sscanf ( tag, "%f", &areaRatio );
    }
    
    sptr = otlkStateList;    
    stateName[0] = '\0';
    while ( sptr ) {
        sptr = cst_split ( sptr, ' ', 10, stateName, &ier );    
        
	if ( strlen( stateName ) == 2 ) {
	    
	    if ( !strstr( airmStateList, stateName ) )   {
	        
	        sprintf ( bndsTag, "<STATE>%s", stateName );
                
	        af_intAreaStatus( fmt->el_poly, BNDS_FILE,
                                  bndsTag, genOlk, &ier ); 
	    
	        if ( ier != 0 && !(*genOlk) ) {
		    sprintf ( bndsTag, "<ID>%s", stateName );
		    af_intAreaStatus( fmt->el_poly, GREAT_LAKES,
                                      bndsTag, genOlk, &ier );  		
                }
	     	                
	        if ( !(*genOlk) && cstlWaterExist ) {
		    sprintf ( bndsTag, "<ID>%s", stateName );
		    af_intAreaStatus( fmt->el_poly, AIRMET_CSTL_BNDS, 
		                      bndsTag, genOlk, &ier ); 
                }
	    
	        if ( *genOlk ) {
	            break;
	        }
	    }
	    
	    else {  /* In-state check */
	        sprintf ( bndsTag, "<STATE>%s", stateName );
                
		af_genInstOtlk( fmt, BNDS_FILE, bndsTag, areaRatio, 
                                genOlk, &ier );
				
	        if ( ier != 0 && !(*genOlk) ) {
		    sprintf ( bndsTag, "<ID>%s", stateName );
		    af_genInstOtlk( fmt, GREAT_LAKES, bndsTag, 
                                     areaRatio, genOlk, &ier );
                }
					    
	        if ( !(*genOlk) && cstlWaterExist ) {
		    sprintf ( bndsTag, "<ID>%s", stateName );
		    af_genInstOtlk( fmt, AIRMET_CSTL_BNDS, bndsTag, 
                                    areaRatio, genOlk, &ier );
                }
	    
	        if ( *genOlk ) {
	            break;
	        }
	    }		
	}	
    }
}

/*=====================================================================*/

static void af_adjEmbedSSW ( int nin, GFA_SS_Attr *ssw, int index, 
			     int nfmt, GFA_Elem_Format *fmt,
			     int *olkSeq, GFA_SS_Attr *ssw_embed )
/************************************************************************
 * af_adjEmbedSSW                                        		*
 *                                                                      *
 * Adjusts the type of snapshots for an outlook embeded in AIRMET.	*
 *                                                                      *
 * static void af_adjEmbedSSW (ss, index, nfmt, fmt, olkSeq, ssw_embed )*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nin		int		number of GFA_SS_Attr elems	*
 * 	*ssw		GFA_SS_Attr	Array of GFA_SS_Attr elems	*
 * 	index		int		Index of the GFA format elm 	*
 * 	nfmt		int		Total number of GFA format elms	*
 *      *fmt_in		GFA_Elem_Format Array of GFA format structures	*
 * 	*olkSeq		int		Outlook SS seqence in ssw	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*ssw_embed	GFA_SS_Attr	 GFA_SS_Attr elems for		* 
 *					 embeded outlook in smear	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	07/06 	initial coding				*
 * B. Yin/SAIC          07/06   change subtype (haz and category types) *
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 * B. Yin/SAIC          10/06   ignore canceled snapshots.              *
 * B. Yin/SAIC          10/06   add bcak 6/9/12 snapshots for AIRMET.   *
 * E. Safford/SAIC	12/06	param change to af_useSnapshots		*
 * J. Wu/SAIC      	04/07	look for O only if no X inside FROM line*
 * 				and X snapshot must intersect the FROM	*
 *				line with an area > 3K sq nm		*
 ***********************************************************************/
{    
    int		ii, jj, ier, nss, subTyp1, subTyp2, olkFound, smearFlag;  
    int		nssOtlk, nCancel, nCancelOtlk, kk;
    float	farea;
    char	tmpStr[32];
    
    Boolean	isOlkSS;
    
    gpc_polygon	ss_poly, tmp_poly;    

    VG_DBStruct **elNoCancel, **elNoCancelOtlk, **elCancel, **elCancelOtlk;
/*---------------------------------------------------------------------*/
    
    /*
     *  Remove canceled snapshots.
     */
    cvg_getFld ( &(fmt[index].el), TAG_GFA_SUBTYPE, tmpStr, &ier );
    subTyp1 = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;

    smearFlag = subTyp1;
    af_useSnapshots( fmt[index].origInfo->snapshots,
		     fmt[index].origInfo->nsnapshot,
		     smearFlag, &elNoCancel, &nss, 
		     &elCancel, &nCancel, &ier );

    /*
     *  Add back 6/9/12 snapshots for AIRMET because end wording needs
     *  all snapshots.
     */
    if ( smearFlag == GFA_USER_SMEAR || smearFlag == GFA_SYSTEM_SMEAR ) {

           af_useSnapshots( fmt[index].origInfo->snapshots,
			    fmt[index].origInfo->nsnapshot,
			    GFA_SYSTEM_OUTLOOK, &elNoCancelOtlk, &nssOtlk, 
			    &elCancelOtlk, &nCancelOtlk, &ier );

	   af_addOtlkSnapshots( &elNoCancel, &nss, elNoCancelOtlk, nssOtlk );

	   G_FREE( elNoCancelOtlk, VG_DBStruct* );
	   G_FREE( elCancelOtlk, VG_DBStruct* );

    }

    /*
     *  Copy over the information without adjustment.
     */
    for ( ii = 0; ii < nin;  ii++ ) {
        ssw_embed[ ii ].type     =  ssw[ ii ].type;  
        ssw_embed[ ii ].hour     =  ssw[ ii ].hour;  
        ssw_embed[ ii ].minute   =  ssw[ ii ].minute;  
        ssw_embed[ ii ].dvlpg_hr =  ssw[ ii ].dvlpg_hr;  
        ssw_embed[ ii ].endg_hr  =  ssw[ ii ].endg_hr;  
        ssw_embed[ ii ].nsnap    =  ssw[ ii ].nsnap;  
        ssw_embed[ ii ].start    =  ssw[ ii ].start;  
    
        for ( jj = 0; jj < ssw[ ii ].nsnap; jj++ ) {
	    ssw_embed[ ii ].ssw[ jj ].type = ssw[ ii ].ssw[ jj ].type;
	    ssw_embed[ ii ].ssw[ jj ].hour = ssw[ ii ].ssw[ jj ].hour;
	    ssw_embed[ ii ].ssw[ jj ].minute = ssw[ ii ].ssw[ jj ].minute ;
	    ssw_embed[ ii ].ssw[ jj ].dvlpg_hr = ssw[ ii ].ssw[ jj ].dvlpg_hr;
	    ssw_embed[ ii ].ssw[ jj ].endg_hr = ssw[ ii ].ssw[ jj ].endg_hr;	
	}
    }
        
    
    /*
     *  The snapshot with the starting hour must exist (hour 6 or 3 at off-time)
     *  and the group must have an outlook in it.
     */
    if ( olkSeq[ 0 ] < 0 ||  
        fmt[index].origInfo->outlook == (VG_DBStruct *)NULL ) {
        G_FREE( elNoCancel, VG_DBStruct* );
        G_FREE( elCancel, VG_DBStruct* );
        return;
    }
        
    
    /*
     *  Find the outlook originated from the same snapshots and clipped to
     *  the same region .
     */
    
    olkFound = -1;
    if ( subTyp1 == GFA_SYSTEM_SMEAR || subTyp1 == GFA_USER_SMEAR ) {
        for ( ii = 0; ii < nfmt; ii++ ) {
            cvg_getFld ( &(fmt[ii].el), TAG_GFA_SUBTYPE, tmpStr, &ier );
            subTyp2 = atoi( tmpStr )  - atoi( tmpStr ) / 10 * 10;
	    if ( fmt[ii].region == fmt[index].region &&
	         fmt[ii].origInfo == fmt[index].origInfo ) {
	     
	        if ( ( subTyp1 == GFA_SYSTEM_SMEAR && 
	               subTyp2 == GFA_SYSTEM_OUTLOOK ) ||
		     ( subTyp1 == GFA_USER_SMEAR && 
	               subTyp2 == GFA_USER_OUTLOOK ) )  {
	    
	            olkFound = ii;
	            break;
	        }
	    }         
        }            
    }
    
    
    /*
     *  Check the intersection of the snapshots with the outlook found
     *  and adjust the type of snapshots.
     */
    if ( olkFound >= 0 ) {
        for ( ii = 0; ii < nin; ii++ ) {
        
	    isOlkSS = False;
	    for ( jj = 0; jj < 3; jj++ )  {
	       if ( ssw_embed[ ii ].minute == 0 &&
	           ssw_embed[ ii ].hour == ssw_embed[ olkSeq[ jj ] ].hour ) {
	           isOlkSS = True;
	           break;
	       }	    
	    }
	        
	    if ( !isOlkSS ) {
	        continue;
	    }
	
	    
	    ssw_embed[ ii ].type = O_SNAPSHOTS;			    
	    
	    for ( jj = 0; jj < ssw_embed[ ii ].nsnap; jj++ ) {
	        af_elm2poly ( *(elNoCancel[ ssw_embed[ ii ].start + jj ] ), &ss_poly,  &ier );
	    
	        gpc_polygon_clip ( GPC_INT, fmt[olkFound].el_poly, &ss_poly, &tmp_poly );
	    
	        farea = 0.0F;
	        ssw_embed[ ii ].ssw[ jj ].type = O_SNAPSHOTS;			    
	        if ( tmp_poly.num_contours > 0  )  {
                    for ( kk = 0; kk < tmp_poly.num_contours; kk++ ) {
                        if ( tmp_poly.hole[ kk ] == G_FALSE ) {
	                    farea += af_gpcPolyArea( &tmp_poly.contour[ kk ], sys_N );
		        }
                    }	        
	        
                    if ( farea > AREA_LIMIT ) {
	                ssw_embed[ ii ].ssw[ jj ].type = X_SNAPSHOTS;			    
	                ssw_embed[ ii ].type = X_SNAPSHOTS;			    
	            }
	        }

	        gpc_free_polygon ( &tmp_poly );		            
	        gpc_free_polygon ( &ss_poly );		            	    
	        
	    }
	}	    	    	
    }

    G_FREE( elNoCancel, VG_DBStruct* );
    G_FREE( elCancel, VG_DBStruct* );
    
}

/*=====================================================================*/

static void af_intAreaStatus( gpc_polygon *poly, char *bounds, 
                             char *name, Boolean *flag, int *iret ) 
/************************************************************************
 * af_intAreaStatus                                       		*
 *                                                                      *
 * Finds out if the given polygon intersects with the given bound for 	*
 * an area greater than/equal to 3000 sq mi.        			*
 *                                                                      *
 * static void af_intAreaStatus ( poly, bounds, name, flag )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *poly		gpc_polygon 	Polygon for calculation		*
 *      *bounds		char 		Bound file name			*
 *      *name		char 		Bound name			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*flag		Boolean	 	True  - intersection >= 3K	* 
 * 					False - intersection < 3K 	* 
 *	*iret		int		0  - normal	 		*
 *					-1 - bound not found	 	*
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	07/06 	initial coding				*
 * E. Safford/SAIC	08/06	moved from afcreate.c  			*
 ***********************************************************************/
{    
    int		ii, ier;  
    float	int_area;
    gpc_polygon bnd_poly, tmp_poly;    
/*---------------------------------------------------------------------*/
    
    /*
     *  Initialize.
     */
    *iret = 0;    
    *flag = False;
    
    
    /*
     *  Retrieve the bound and put into a gpc polygon.
     */
    af_bnds2poly ( bounds, name, &bnd_poly, &ier );
    
    if ( bnd_poly.num_contours <= 0 ) {
        *iret = -1;
        gpc_free_polygon( &bnd_poly );	    	
	
	return;
    }	        
    	                	
    
    /*
     *  Calculate the size of the intersection area between the 
     *  polygin and the bound.
     */
    gpc_polygon_clip ( GPC_INT, poly, &bnd_poly, &tmp_poly );
	            
    int_area = 0.0F;
    if ( tmp_poly.num_contours > 0 ) {
    	for ( ii = 0; ii < tmp_poly.num_contours; ii++ ) {
            if ( tmp_poly.hole[ ii ] == G_FALSE && int_area < AREA_LIMIT ) {
	        int_area += af_gpcPolyArea( &tmp_poly.contour[ii], sys_N );
	    }
	}		    
    }
	

    /*
     *  If the intersection >= 3K,  set the flag to True to issue the outlook. 
     */
    if ( int_area >= AREA_LIMIT ) {	        
        *flag = True;
    }
		            
    gpc_free_polygon( &tmp_poly );	    		        
    gpc_free_polygon( &bnd_poly );

}

/*=====================================================================*/

static void af_addOtlkSnapshots( VG_DBStruct ***el, int *nss, 
				 VG_DBStruct **elNoCancelOtlk, int nssOtlk )
/************************************************************************
 * af_addOtlkSnapshots                                       		*
 *                                                                      *
 * This routine merges the two input snapshot pointer arrays. It is used*
 * to add the outlook snapshots(6/9/12 hours) into the AIRMET snapshots.*
 *                                                                      *
 * static void af_addOtlkSnapshots ( el, nss, elNpCancelOtlk, nssOtlk )	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      ***el		VG_DBStruct 	AIRMET snapshots		*
 *      *nss		int 		number of AIRMET snapshots	*
 *                                                                      *
 * Input parameters:                                                    *
 *      **elNoCancelOtlk VG_DBStruct 	Outlook snapshots		*
 *      nssOtlk		 int 		number of outlook snapshots	*
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      	10/06 	Created					*
 * B. Yin/SAIC      	10/06 	Do nothing if there is no otlk snapshots*
 * B. Yin/SAIC      	10/06 	Take care of more than one 6hr snapshots*
 ***********************************************************************/
{
    int 	ii, ier, nIn, nSixHr;
    char	fcstHr[ 32 ];
/*---------------------------------------------------------------------*/

    if ( nssOtlk == 0 ) return;

    nIn = *nss;

    /*
     *   If the 6 hour snapshot(s) is in the outlook, do not include it,
     *   because either the AIRMET already has it or the AIRMET doesn't 
     *   need it.
     */
     
    nSixHr = 0;
    for ( ii = 0; ii < nssOtlk; ii++ ) {

        fcstHr[ 0 ] = '\0';
        cvg_getFld( elNoCancelOtlk[ ii ], TAG_GFA_FCSTHR, fcstHr, &ier );

        if ( ( ier == 0 ) && ( strcmp( fcstHr, "6" ) == 0 ||
    	      		       strcmp( fcstHr, "6:00" ) == 0 ) ) {

	     nSixHr++;

        }

    }

    *nss = *nss + nssOtlk - nSixHr;

    /*
     *  Merge the two arrays.
     */
    if ( *nss > nIn ) {

	G_REALLOC( *el, VG_DBStruct*, *nss, "af_addOtlkSnapshots: el" );

	memmove( &((*el)[ nIn ]), &elNoCancelOtlk[ nSixHr ], 
		 ( nssOtlk - nSixHr ) * sizeof( VG_DBStruct* ) );

    }
    
}

/*=====================================================================*/

static void af_dvlpgApplyIssueTm ( int xx, int yy, int basetime, char* issueTm,
			    GFA_Elem_Format *fmt, char * wording )
/************************************************************************
 * af_dvlpgApplyIssueTm	                                       		*
 *                                                                      *
 * This routine reconstructs the DVLPG wording based on the issue time.	*
 *                                                                      *
 * static void af_dvlpgApplyIssueTm ( xx, yy, basetime, issuetime, fmt	*
 *					wording )			*
 *                                                                      *
 * Input parameters:	  	                                        *
 *      xx		int		starting hour			*
 *	yy		int		ending hour			*
 *	basetime	int		base time (cycle)		*
 *	*issueTm	char		issue time HHMM			*
 *	*fmt		GFA_Elem_Format	GFA format structure		*
 *                                                                      *
 * Output parameters:                                             	*
 *      *wording	char		DVLPG wording			*	
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      	03/07 	Created					*
 * B. Yin/SAIC      	05/07 	Removed 'Z' from the starting hour.	*
 ***********************************************************************/
{
    int 	issueHrMin, ier;
/*---------------------------------------------------------------------*/

    issueHrMin = atoi( issueTm );

    if ( ( yy + basetime ) * 100 <= issueHrMin ) {	/* yy in past */

	wording[ 0 ] = '\0';

	cvg_setFld( &(fmt->el), TAG_GFA_STATUS, "AMD", &ier );
	
    }
    else {						/* yy in future */
	
	if ( ( xx + basetime ) * 100 <= issueHrMin ) {	/* xx in past */

		sprintf( wording, "BY +%02iZ", yy );
		 
	 	cvg_setFld( &(fmt->el), TAG_GFA_STATUS, "AMD", &ier );

	}
	else {						/* xx in future */

		sprintf( wording, "+%02i-+%02iZ", xx, yy );
	}
    }

}

/*=====================================================================*/

static void af_endgApplyIssueTm ( int xx, int yy, int basetime, char* issueTm,
			    GFA_Elem_Format *fmt, char * wording )
/************************************************************************
 * af_endgApplyIssueTm	                                       		*
 *                                                                      *
 * This routine reconstructs the ENDG wording based on the issue time.	*
 *                                                                      *
 * static void af_endgApplyIssueTm ( xx, yy, basetime, issuetime, fmt	*
 *					wording )			*
 *                                                                      *
 * Input parameters:	  	                                        *
 *      xx		int		starting hour			*
 *	yy		int		ending hour			*
 *	basetime	int		base time (cycle)		*
 *	*issueTm	char		issue time HHMM			*
 *	*fmt		GFA_Elem_Format	GFA format structure		*
 *                                                                      *
 * Output parameters:                                             	*
 *      *wording	char		ENDG wording			*	
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      	03/07 	Created					*
 * E. Safford/SAIC     	05/07 	Removed 'Z' from the ending hour.	*
 ***********************************************************************/
{
    int 	issueHrMin, ier;
/*---------------------------------------------------------------------*/

    issueHrMin = atoi( issueTm );

    if ( ( yy + basetime ) * 100 <= issueHrMin ) {	/* yy in past */

	strcpy( wording, "CONDS HV ENDED" );

	cvg_setFld( &(fmt->el), TAG_GFA_STATUS, "CAN", &ier );
	
    }
    else {						/* yy in future */
	
	if ( ( xx + basetime ) * 100 <= issueHrMin ) {	/* xx in past */

		sprintf( wording, "BY +%02iZ", yy );
		 
	 	cvg_setFld( &(fmt->el), TAG_GFA_STATUS, "AMD", &ier );

	}
	else {						/* xx in future */

		sprintf( wording, "+%02i-+%02iZ", xx, yy );
	}
    }

}

/*=====================================================================*/

static void af_getHours ( char* wording, int* startHr, int* endHr )
/************************************************************************
 * af_getHours		                                       		*
 *                                                                      *
 * This routine get the starting and ending hours from the DVLPG or ENDG*
 * wordings.								*
 *                                                                      *
 * static void af_getHours ( wording, startHr, endHr )			*
 *                                                                      *
 * Input parameters:	  	                                        *
 *      *wording	char 		DVLPG/ENDG wording string	*
 *                                                                      *
 * Output parameters:                                             	*
 *      *startHr 	int		starting hour			*
 *      *endHr 		int		ending hour			*
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      	03/07 	Created					*
 ***********************************************************************/
{
   char	*ptr1, *ptr2;
/*---------------------------------------------------------------------*/

   ptr1 = ptr2 = NULL;
   *startHr = *endHr = -1;

   /*
    *  We are looking for xx and yy in strings such as "+xx-+yyZ".
    */
   ptr1 = strchr( wording, '+' );

   if ( ptr1 ) {

	*startHr = atoi( ptr1 + 1 );

	ptr2 = strchr( ptr1 + 1, '+' );

	if ( ptr2 ) {

		*endHr = atoi( ptr2 + 1 );

	}
   }

}
				
void af_parseWording( Boolean isSmear, char *wording, char *beginWording,
			char *endWording, int *iret )
/************************************************************************
 * af_parseWording	                                       		*
 *                                                                      *
 * This routine gets the starting and ending wordings from the format   *
 * string.								*
 *                                                                      *
 * void af_parseWording ( isSmear, wording, beginWording, endWording,   *
 *                        iret )					*
 *                                                                      *
 * Input parameters:	  	                                        *
 *	isSmear		Boolean		smear or outlook		*
 *      *wording	char 		the wording string in fmt struct*
 *                                                                      *
 * Output parameters:                                             	*
 *      *beginWording 	char		start wording			*
 *      *endWording 	char		end wording			*
 *	*iret		int		return code 0: normal		*
 *					1: empty end/begin wording	*	     
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      	09/07 	Created					*
 * J. Wu/SAIC      	10/07 	Check NULL input, remove leading space	*
 *				and the ending '.'			*
 ***********************************************************************/
{
    int 	ier;
    char	contg[ 256 ], tmpStr[ 256 ];
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    beginWording[ 0 ] = '\0';
    endWording[ 0 ] = '\0';
    
    /*
     *   "Wording" is always initialized NULL.  If no conditional wording
     *   is found later, the wording remains NULL - so passing it to
     *   cvg_getFld() could lead to a crush. 
     */
    if ( wording == (char *)NULL ) {
        *iret = 1;
	return;
    }

        
    /*
     *   For smear wording 
     */
    if ( isSmear ) {

       /*
        *  Get developing wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "fromCondsDvlpg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {

	  sprintf( beginWording, "%s", tmpStr );

       }

       /*
	*  Get continue wording and append it at the end of developing wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "condsContg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {
          if ( strlen( beginWording ) != 0 ) {
	      sprintf( contg, ". %s", tmpStr );
	  }
	  else {
	      sprintf( contg, "%s", tmpStr );	  
	  }
	  
	  strcat( beginWording, contg );

       }

       /*
	*  Get the ending wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "fromCondsEndg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {

	  sprintf( endWording, "%s", tmpStr );

       }

    }
    /*
     *  For outlook wording
     */
    else {

       /*
        *  Get developing wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "otlkCondsDvlpg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {

	  sprintf( beginWording, "%s", tmpStr );

       }

       /*
	*  Get continue wording and append it at the end of developing wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "condsContg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {

          if ( strlen( beginWording ) != 0 ) {
	      sprintf( contg, ". %s", tmpStr );
	  }
	  else {
	      sprintf( contg, "%s", tmpStr );	  
	  }

	  strcat( beginWording, contg );

       }	  

       /*
	*  Get the ending wording
	*/
       tmpStr[ 0 ] = '\0';
       cst_gtag ( "otlkCondsEndg", wording, "", tmpStr, &ier );

       if ( strlen( tmpStr ) != 0 ) {

	  sprintf( endWording, "%s", tmpStr );

       }

    }

    if ( strlen( beginWording ) == 0 || strlen( endWording ) == 0 ) *iret = 1;

}

/*=====================================================================*/

static void af_genInstOtlk( GFA_Elem_Format *fmt, char *bounds, 
               char *name, float ratioIn, Boolean *flag, int *iret ) 
/************************************************************************
 * af_genInstOtlk                                       		*
 *                                                                      *
 * Finds out if the in-state outlook generation ratio is greater than 	*
 * the given threshold ratio.						*
 *                                                                      *
 * static void af_genInstOtlk ( fmt, bounds, name, ratioIN, flag, iret )*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fmt		GFA_Elem_Format GFA format structure		*
 *      *bounds		char 		Bound file name			*
 *      *name		char 		Bound name			*
 *      ratioIn		float 		ratio to be compared with	*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*flag		Boolean	 	True  - ratio > ratioIn		* 
 * 					False - ratio <= ratioIn 	* 
 *	*iret		int		0  - normal	 		*
 *					-1 - bound not found	 	*
 *					 				*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	12/07 	initial coding				*
 * L. Hinson/AWC        06/09   Formula Change  ratio =                 *
 *                         (Outlook Area - Intersect Area) / State Area *
 ***********************************************************************/
{    
    int		ii, ier;  
    float	stateArea, otlkArea, intArea, ratio;
    gpc_polygon bndPoly, tmpPoly, airmetPoly, intPoly;    
/*---------------------------------------------------------------------*/
    
    /*
     *  Initialize.
     */
    *iret = 0;    
    *flag = False;
    
    
    /*
     *  Retrieve the bound and put into a gpc polygon.
     */
    af_bnds2poly ( bounds, name, &bndPoly, &ier );
    
    if ( bndPoly.num_contours <= 0 ) {
        *iret = -1;
        gpc_free_polygon( &bndPoly );	    	
	
	return;
    }

    /* Calculate the size of the state */
    stateArea = 0.0F;
    if ( bndPoly.num_contours > 0 ) {
      for ( ii=0; ii < bndPoly.num_contours; ii++ ) {
        if ( bndPoly.hole [ ii ] == G_FALSE ) {
          stateArea += af_gpcPolyArea( &bndPoly.contour[ii], sys_N );
        }
      }
    }	                	
    
    /*
     *  Calculate the size of the OUTLOOK area within the state
     */
    gpc_polygon_clip ( GPC_INT, fmt->el_poly, &bndPoly, &tmpPoly );
	            
    otlkArea = 0.0F;
    if ( tmpPoly.num_contours > 0 ) {
    	for ( ii = 0; ii < tmpPoly.num_contours; ii++ ) {
            if ( tmpPoly.hole[ ii ] == G_FALSE ) {
	        otlkArea += af_gpcPolyArea( &tmpPoly.contour[ii], sys_N );
	    }
	}		    
    }
    
    gpc_free_polygon( &tmpPoly );
    
    af_elm2poly ( *(fmt->origInfo->smear), &airmetPoly, &ier );	    		        
    
    /*
     *  Get the intersection polygon between the OUTLOOK and the AIRMET
     */
    gpc_polygon_clip ( GPC_INT, fmt->el_poly, &airmetPoly, &intPoly );
    
    
    /*
     *  Calculate the size of the intersection polygon within the state
     */
    gpc_polygon_clip ( GPC_INT, &intPoly, &bndPoly, &tmpPoly );
	               
    intArea = 0.0F;
    if ( tmpPoly.num_contours > 0 ) {
    	for ( ii = 0; ii < tmpPoly.num_contours; ii++ ) {
            if ( tmpPoly.hole[ ii ] == G_FALSE ) {
	        intArea += af_gpcPolyArea( &tmpPoly.contour[ii], sys_N );
	    }
	}		    
    }
	
    gpc_free_polygon( &tmpPoly );
    
    	    		        
    
    /*
     *  If the ratio > GFA_OTLKGEN_RATIO, set the flag to True to issue the outlook. 
     */
    ratio = -1.0F;
    
    if ( stateArea > 0.0F && intArea > 0.0F ) {
        ratio = ( otlkArea - intArea ) / stateArea;
    }
        
    if ( ratioIn > 0.0F && ratio > ratioIn ) {	        
        *flag = True;
    }
		            
    gpc_free_polygon( &bndPoly );
    gpc_free_polygon( &airmetPoly );	    		        
    gpc_free_polygon( &intPoly );

}

/*=====================================================================*/
