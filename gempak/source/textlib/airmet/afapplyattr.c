
#include "afcmn.h"

#define		TOP		0
#define		BOTTOM		1

/************************************************************************
 * afapplyattr.c                                             		*
 *                                                                      *
 * This module contains all the subroutines used to apply the selected 	*
 * snapshots' attributes to clipped airmet or outlook.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   public( to the library) functions:                                 *
 *      af_applyattr		- select snapshots and apply the worst	*
 *                                case attribites towards the clipped	*
 *				  airmet or outlook			*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void af_applyAttr ( char *cycle, int numFmt, GFA_Elem_Format *fmt, 
		    int *iret )
/************************************************************************
 * af_applyAttr                                                     	*
 *                                                                      *
 * This routine applies the attributes of selected snapshots to their	*
 * associated clipped airmet or outlook in the format structure.	*
 *                                                                      *
 * void af_applyAttr ( cycle, numFmt, fmt_in, iret )			*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *cycle		char		forecast cycle			*
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
 * J. Wu/SAIC          	09/06   Created                         	*
 * B. Yin/SAIC		10/06	Remove canceled snapshots.		*
 * J. Wu/SAIC          	10/06   Skip deleted format structures         	*
 * B. Yin/SAIC		11/06	Fixed the start_indx bug.		*
 * E. Safford/SAIC	12/06	use CAN status snapshots for evaluating *
 *				  resultant status types		*
 * B. Yin/SAIC		01/07	Fixed the issue status bug.		*
 * B. Yin/SAIC		03/07	Fixed the all canceled snapshots bug.	*
 * J. Wu/SAIC          	04/07   check X_SNAPSHOTS intersecting smear 	*
 *			 	with an area > 3K 			*
 * J. Wu/SAIC      	04/07	add a new rule - look for O only if no 	*
 *				X found inside the clipped FROM line	*
 * B. Yin/SAIC		01/08	Fixed the indx out of bouind bug.	*
 * B. Yin/SAIC		06/08	Use ctb_gfapCombineIFRTypes if C&V is 	*
 *				in gfa.tbl				*
 ***********************************************************************/
{
    int 		ii, jj, kk, ier, nss, base_time; 
    int			sub_type, nsnap, fcst_min;
    int			start_tm, end_tm, start_indx, end_indx;
    int			smearFlag, numCanceled, nissues;
    int			nhour, nn, firstSel, nextSel;
            
    char		tmpStr[ STD_STRLEN ];
    char		**issues,  worstIssue[32];    
    char		worstTop[32], worstBottom[32];
    char		worstBottomFZL[32], worstTopFZL[32];
    char		fzlTop1[32], fzlBottom1[32];
    char		value[32], hazard[32];
    char		freq0[32], freq1[32], sev0[32],  sev1[32];
    char		IFRType[ 256 ], type0[ 256 ], type1[ 256 ];
    char         	top[4], bot[4], topFzl[4], botFzl[4] ;
    char		worstFreq[32], worstSev[32];
       
    Boolean		on_time, isIFR;
    VG_DBStruct		**elNoCancel, **elCancel;
    GFA_SS_Attr		*ss_attr;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
   

    /*
     *  No need to proceed if no clipping.
     */
    if ( _clippingFlg == 0 ) {
        return;
    }
        
 
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
     *  Select and apply snapshot's attributes to the airmet or outlook 
     *  in each format structure.
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
	  *  Find the type of the smear (AIRMET or OUTLOOK) and determine
	  *  the starting and ending time for snapshot sequence.
	  */
	cvg_getFld ( &fmt[ii].el, TAG_GFA_SUBTYPE, tmpStr, &ier );
        sub_type = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;
	
	if ( sub_type == GFA_SYSTEM_SMEAR ||
	     sub_type == GFA_USER_SMEAR ) {
	    start_tm = 0;
	    if ( on_time ) {
		end_tm = 360;
	    }
	    else {
	        end_tm = 180;	        
	    }
	}
	else {	    
	    if ( on_time ) {
	        start_tm = 360;
		end_tm = 720;
	    }
	    else {
	        start_tm = 180;
	        end_tm = 540;	        
	    }       
	}

	/*
	 *  Remove canceled snapshots.
	 */
	smearFlag = sub_type;

	af_useSnapshots( fmt[ii].origInfo->snapshots,
			 fmt[ii].origInfo->nsnapshot,
			 smearFlag, &elNoCancel, &nss, 
			 &elCancel, &numCanceled, &ier );
			
	
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
	    G_MALLOC ( ss_attr[jj].ssw, GFA_SS_Wording, nss, 
	                "af_applyAttr: ssw_attr.ssw" );
	}
        
	af_getSSAttr ( nss, elNoCancel, fmt[ii].el_poly,
		       &nhour, ss_attr, &ier );		


								
	/* 
	  * Find the snapshot sequence based on cycle and forecast hour. 
	  */
	start_indx = -1;
	end_indx = -1;
	for ( jj = 0; jj < nhour; jj++ ) {
	    
	    fcst_min = ss_attr[ jj ].hour * 60 + ss_attr[ jj ].minute;
	    
	    if ( start_indx < 0 && fcst_min >= start_tm ) {
	         start_indx = jj;
	         end_indx = jj;	    
	    } 
	    
	    if ( fcst_min <= end_tm ) {
	         end_indx = jj;	    
	    }
	    
	}
				
	
	/*
	  *  Adjust the selection of snapshots based on the rule:
	  *  The starting snapshot should be an X or an O followed by an X.
	  *  The end snapshot should be an X or an O followed by an X, if we
	  *  look the snapshot sequence in reverse order.
	  */	
	for ( jj = start_indx;  jj <= end_indx; jj++ ) {
	    if ( ss_attr[ jj ].type == X_SNAPSHOTS ) {
	        break;		
	    }
	    else {
	        if ( (jj+1) < nhour  && ss_attr[ jj+1 ].type == X_SNAPSHOTS ) {
		    break;
		}
		else {
		    if ( ( start_indx + 1 ) < nhour ) {

		        start_indx++;

		    }
		} 
	    }	         
	}

	for ( jj = end_indx;  jj >= start_indx; jj-- ) {
	    if ( ss_attr[ jj ].type == X_SNAPSHOTS ) {
	        break;		
	    }
	    else {
	        if ( (jj-1) >= 0  && ss_attr[ jj - 1].type == X_SNAPSHOTS ) {
		    break;
		}
		else {
		    end_indx--;
		} 
	    }	         
	}
	
	
	/*
	  *  Skip if no snapshots selected.
	  */
	if ( start_indx < 0 )  {
	    continue;
	}
	 	
        
	/*
	  *  Initialize the working variables for each format structure.
	  */
	worstIssue[0]	= '\0';
        top[0]		= '\0';
        bot[0]		= '\0'; 
        worstFreq[0]	= '\0';
        worstSev[0]	= '\0';
        topFzl[0]	= '\0';
        botFzl[0]	= '\0';
        IFRType[0]	= '\0';
        isIFR 		= False;
	
	
	/*
	 *  Get the worst issue status from the selected snapshots.
	 */
	nsnap = 0;
	for ( jj = start_indx;  jj <= end_indx; jj++ ) {
	    if ( ss_attr[jj].type == O_SNAPSHOTS ) {
	        nsnap += ss_attr[jj].nsnap;
	    }
	    else {
	        for ( kk = 0; kk < ss_attr[jj].nsnap; kk++ ) {
		    if ( ss_attr[jj].ssw[kk].type == X_SNAPSHOTS ) {
		        nsnap++;
		    }	    
	        }
	    }
        }
        
	issues = (char **) malloc ( (nsnap + numCanceled) * sizeof( char * ) );
	      
	nn = 0;
	for ( jj = start_indx;  jj <= end_indx; jj++ ) {            
	    if ( ss_attr[jj].type == O_SNAPSHOTS ) {
	        for ( kk = 0; kk < ss_attr[jj].nsnap; kk++ ) {
	            
	            issues[ nn ] = (char (*) ) malloc ( sizeof( char ) * 32 );

                    cvg_getFld ( elNoCancel[ ss_attr[jj].start + kk ], 
		                 TAG_GFA_STATUS, issues[ nn ], &ier );

	            nn++;
		}	        
	    }
	    else {
	        for ( kk = 0; kk < ss_attr[jj].nsnap; kk++ ) {
		    
		    if ( ss_attr[jj].ssw[kk].type == X_SNAPSHOTS ) {
	                issues[ nn ] = (char (*) ) malloc ( sizeof( char ) * 32 );

                        cvg_getFld ( elNoCancel[ ss_attr[jj].start + kk ], 
		                     TAG_GFA_STATUS, issues[ nn ], &ier );
	                nn++;		        
		    }	    
	        }
	    }	     
        }

	nissues = nsnap;

	/*
 	 *  If all snapshots are canceled, they are put in the elNoCancel and 
	 *  we don't need to count them again.
	 */
	if( numCanceled > 0 && elCancel != NULL ) {

	    for( jj=0; jj<numCanceled; jj++ ) {
	         issues[ nissues  ] = 
		 		(char (*) ) malloc ( sizeof( char ) * 32 );
                 cvg_getFld ( elCancel[ jj ], TAG_GFA_STATUS, 
		 				issues[ nissues ], &ier );
	         nissues++;
	    }
	}

        ctb_gfaWorstIssue ( nissues, issues, worstIssue, &ier );
      
        for ( jj = 0;  jj < nissues; jj++ ) {
            free ( issues[ jj ] );
        }
      
        free ( issues );
	
	
	/*
         *  Get other worst cases from the selected snapshots.
	 *  
	 *  The top/bottom flight levels, and the top/bottom freezing levels are
	 *  to be maximum and minimum, respectively.  The Frequency and Severity 
	 *  fields are to be the worst case as well.  Here we load the intial values 
	 *  for these fields, by taking the values from first selected snapshot.  
	 *  We'll then compare these values to all the other selected snapshots 
	 *  and take the worst cases of each to be applied to the AIRMET of OUTLOOK.
         */
        if ( ss_attr[start_indx].type == O_SNAPSHOTS ) {
	    firstSel = ss_attr[start_indx].start;	    
	}
	else {
            for ( kk =0; kk < ss_attr[start_indx].nsnap; kk++ ) {
	        if ( ss_attr[start_indx].ssw[kk].type == X_SNAPSHOTS ) {
	            firstSel = ss_attr[start_indx].start + kk;	    
	        }
	    }	
	}
	
	cvg_getFld ( elNoCancel[ firstSel], TAG_GFA_TOP, top, &ier );
        cvg_getFld ( elNoCancel[ firstSel], TAG_GFA_BOTTOM, bot, &ier );

        cvg_getFld ( elNoCancel[ firstSel], TAG_GFA_FZL_TOP, topFzl, &ier );
        cvg_getFld ( elNoCancel[ firstSel], TAG_GFA_FZL_BOTTOM, botFzl, &ier );

        cvg_getFld ( elNoCancel[ firstSel], TAG_GFA_AREATYPE, value, &ier );
        strcpy( hazard, value );

        if( strcasecmp( hazard, "IFR" ) == 0 ) {
	    isIFR = True;
            cvg_getFld( elNoCancel[ firstSel ], "<Type>", IFRType, &ier );
        }

        cvg_getFld ( elNoCancel[ firstSel ], "<Frequency>", worstFreq, &ier );      
        cvg_getFld ( elNoCancel[ firstSel ], "<Severity>", worstSev, &ier );      
	
	for ( jj = start_indx;  jj <= end_indx; jj++ ) {
            
	    for ( kk = 0;  kk < ss_attr[jj].nsnap; kk++ ) {
		
		if ( ss_attr[jj].type == X_SNAPSHOTS && 
		     ss_attr[jj].ssw[kk].type == O_SNAPSHOTS ) {
		    continue;   
		}
		
		nextSel = ss_attr[jj].start + kk;
	        
		if ( nextSel == firstSel ) continue;
		
	        cvg_getFld ( elNoCancel[ nextSel ], TAG_GFA_TOP, value, &ier );

                cvg_getFld ( elNoCancel[ nextSel ], TAG_GFA_FZL_TOP, fzlTop1, &ier );
                cvg_getFld ( elNoCancel[ nextSel ], TAG_GFA_FZL_BOTTOM, fzlBottom1, &ier );

	        ctb_gfaWorstFL ( TOP, top, botFzl, topFzl, value, 
	 		      fzlBottom1, fzlTop1, worstTop, 
			      worstBottomFZL, worstTopFZL, &ier );
	        if ( ier == 0 ) {
	            strcpy ( top, worstTop );
	        }

                cvg_getFld ( elNoCancel[ nextSel ], TAG_GFA_BOTTOM, value, &ier );

	        ctb_gfaWorstFL ( BOTTOM, bot, botFzl, topFzl, value, 
	 		      fzlBottom1, fzlTop1, worstBottom, 
			      worstBottomFZL, worstTopFZL, &ier );
	        if ( ier == 0 ) {
	            strcpy ( bot, worstBottom );
	            strcpy ( topFzl, worstTopFZL );
	            strcpy ( botFzl, worstBottomFZL );
	        }


	        if ( strlen( worstFreq ) > (size_t)0 ) {
	            strcpy( freq0, worstFreq );
                    cvg_getFld ( elNoCancel[ nextSel ], "<Frequency>", freq1, &ier );                      
		    ctb_gfaCmpSeverity( hazard, "Frequency", 
	     				freq0, freq1, worstFreq, &ier ); 
                }

	        if ( strlen( worstSev ) > (size_t)0 ) {
	            strcpy( sev0, worstSev );
                    cvg_getFld ( elNoCancel[ nextSel ], "<Severity>", sev1, &ier );      
                    ctb_gfaCmpSeverity( hazard, "Severity", 
	     				sev0, sev1, worstSev, &ier ); 
                }

	        if ( isIFR ) {
	            cvg_getFld( elNoCancel[ nextSel ], "<Type>", type1, &ier );
	            strcpy( type0, IFRType );
		    
		    if ( ctb_gfaHasCV() ) {

	                ctb_gfapCombineIFRTypes( type0, type1, IFRType, &ier );

		    }
		    else {

	                ctb_gfaCombineIFRTypes( type0, type1, IFRType, &ier );

		    }
                }
	    }	            
	}
	
	
	/* 
	  * Apply the new attributes to the clipped AIRMET or OUTLOOK.
	  */		
	if ( strcasecmp ( bot, "FZL" ) == 0 ) {
	    cvg_setFld ( &fmt[ ii ].el, TAG_GFA_FZL_TOP, topFzl, &ier );
	    cvg_setFld ( &fmt[ ii ].el, TAG_GFA_FZL_BOTTOM, botFzl, &ier );
	}
  	else {
  	    cvg_rmFld ( &fmt[ ii ].el, TAG_GFA_FZL_TOP, &ier );
	    cvg_rmFld ( &fmt[ ii ].el, TAG_GFA_FZL_BOTTOM, &ier );
	} 

	cvg_setFld ( &fmt[ ii ].el, TAG_GFA_TOP, top, &ier );
	cvg_setFld ( &fmt[ ii ].el, TAG_GFA_BOTTOM, bot, &ier );

	if( strlen( worstFreq ) > (size_t)0 ) {
	    cvg_setFld ( &fmt[ ii ].el, "<Frequency>", worstFreq, &ier );
        }
	
	if( strlen( worstSev ) > (size_t)0 ) {
	    cvg_setFld ( &fmt[ ii ].el, "<Severity>", worstSev, &ier );
        }
	 
	if( strlen( worstIssue ) > (size_t)0 ) {
            cvg_setFld ( &fmt[ ii ].el, TAG_GFA_STATUS, worstIssue, &ier );
        }
	
        if ( isIFR ) {
            cvg_setFld( &fmt[ ii ].el, "<Type>", IFRType, &ier );
	}
 	
	
	/* 
	  * Free memory.
	  */	        
	for ( nn = 0; nn < nss; nn++ ) {
	    G_FREE ( ss_attr[nn].ssw, GFA_SS_Wording );
        }
	G_FREE ( ss_attr, GFA_SS_Attr );

	
	G_FREE ( elCancel,   VG_DBStruct* );
	G_FREE ( elNoCancel, VG_DBStruct* );
		    
    }

}

/*=====================================================================*/
