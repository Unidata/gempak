#include "cvgcmn.h"

void cvg_matchfilter ( filter_t el_filter, Boolean matchAny, 
		       Boolean *match, filter_t timeMatched, int *iret )
/************************************************************************
 * cvg_matchfilter							*
 *									*
 * This function checks if an element's filter value matches one of	*
 * the selections in the filter window.  				*
 *									*
 * cvg_matchfilter ( el_filter, matchAny, match, timeMatched, iret )	*
 *									*
 * Input parameters:							*
 *	el_filter	filter_t	filter value of an element	*
 *	matchAny	Boolean		if match to any available time	*
 *									*
 * Output parameters:							*
 *	*match		Boolean		True/False			*
 *	timeMatched	filter_t	actual matched time		*
 *	*iret		int		Return code			*
 *					  0 = Normal			*
 **									*
 * Log:									*
 * J. Wu/SAIC           05/06   initial coding				*
 * M. Li/SAIC		03/07	Add new parameters matchAny, timeMatched*
 ***********************************************************************/
{
    int 	ii, jj, ier;
    int		elMin, filterMin, nextTime, tmpMin, filnum;
    char	*cptr, tmpstr[32];	
    filter_t	filters[MAX_FILTER_NUM];
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    *match = False;
    strcpy(timeMatched, "");
                               
    
    /*
     *  If matchAny is True then the set of potential matches for the input 
     *  el_filter value is all available filter times.  If matchAny is False 
     *  then the set of potential matches is only those filter times that are 
     *  currently set to On (active)."
     */
    if ( matchAny ) {
	cvg_gettblfilter( &filnum, filters, &ier );
    }
    else {
        cvg_getfilter( &filnum, filters, &ier );
    }
    
    /*
     *  Match all non-GFA elements.
     */
    if ( strcmp( el_filter, "ALL" ) == 0 )  {
        *match = True;      
    }
    
    /*
     *  Match all elements if the filter window is down.
     */
    else if ( strcmp( filters[0], "ALL" ) == 0 ) {
        *match = True;          
	strcpy( timeMatched, filters[0] );
    }
    
    /*
     *  Match GFAs if the filter window is up and at least one is
     *  selected.
     */
    else if ( strcmp( filters[0], "NONE" ) != 0 )  {
       	
	for ( ii = 0; ii < filnum; ii++ ) {
	    	    
	    /*
              *  Match all GFA airmets if AIRM and/or 0-6 is selected.
              */
	    if ( strcmp( filters[ii], "AIRM" ) == 0 ||
	         strcmp( filters[ii], "0-6" ) == 0 ) {
		
		if ( strcmp( el_filter, "AIRM" ) == 0 ) {
		    *match = True;
		    strcpy( timeMatched, filters[ii] );

		}
		else {
		    if ( strchr( el_filter, '-' ) ) {
                        strcpy ( tmpstr,  el_filter );
                        cptr = strtok( tmpstr, "-" );
		        cptr = strtok ( NULL, "-" );	
                        if ( cptr && atoi( cptr ) <= 6 ) {
                            *match = True;
		    	    strcpy( timeMatched, filters[ii] );
			}
                    }
	        }
	    } 
            
	    /*
              *  Match all GFA outlooks if OTLK and/or 0-6 is selected.
              */
	    else if ( strcmp( filters[ii], "OTLK" ) == 0 ||
	             strcmp( filters[ii], "6-12" ) == 0 ) {
                if ( strcmp( el_filter, "OTLK" ) == 0 ) {
		    *match = True;		     
		    strcpy( timeMatched, filters[ii] );
		}
		else {
		    if ( strchr( el_filter, '-' ) )  {
		        strcpy ( tmpstr,  el_filter );
                        cptr = strtok( tmpstr, "-" );
		        cptr = strtok ( NULL, "-" );	
                        if ( cptr && atoi( cptr ) > 6 ) {
                            *match = True;
		    	    strcpy( timeMatched, filters[ii] );
	                }
	            }
		}
            }
	    
	    /*
              *  Strict string match for GFA smears
              */
	    else if ( strchr( filters[ii], '-' ) || 
	             strchr( el_filter, '-' ) )  {
	        if ( strcmp( filters[ii], el_filter ) == 0 ) {    
	            *match = True;
		    strcpy( timeMatched, filters[ii] );
	        }
	    }
	    
	    /*
              *  Match GFA snapshots
              */
	    else {	 
                filterMin = atoi ( filters[ii] ) * 60;
 		 
                strcpy ( tmpstr,  el_filter );
                cptr = strtok( tmpstr, ":" );
                elMin = atoi ( cptr ) * 60;
                cptr = strtok ( NULL, ":" );	
                if ( cptr ) {
                    elMin += atoi( cptr );
                }

	        /*
                  *  Match single hour GFA snapshots
                  */
		if ( !strchr( filters[ii], '+' ) ) {
                    if ( elMin == filterMin  ) {
	                *match = True;
		    	strcpy( timeMatched, filters[ii] );
		    }		
		}
	        
		/*
                  *  Match all GFA snapshots within an interval - specified
		  *  by an additional "+" following a single hour. E.g., 
                  *  "3+" means an interval from 3 to next closest single 
		  *  hour in the filter table. "Closest" means the smallest
		  *  among those single hour entries greater than 3.  
		  */
		else {
		    nextTime = 0;		     
		    for ( jj = 0; jj < nTblFilter; jj++ ) {
                        
			if ( isdigit( tblFilter[jj][0] ) && 
			     !strchr( tblFilter[jj], '-' ) ) {
			    
			    strcpy ( tmpstr,  tblFilter[ jj ] );
                            cptr = strtok( tmpstr, ":" );
                            tmpMin = atoi ( cptr ) * 60;
                            cptr = strtok ( NULL, ":" );	
                            if ( cptr ) {
                                tmpMin += atoi( cptr );
                            }
		        
			    if ( tmpMin > filterMin ) {
				if ( nextTime > 0 ) {
				    nextTime = G_MIN ( nextTime, tmpMin );
				}
				else {				
			            nextTime = tmpMin;
				}				
			    }
			}
		    }
		    
		    if ( elMin >= filterMin && elMin < nextTime ) {
		        *match = True;
		    	strcpy( timeMatched, filters[ii] );
		    }
		}
            }
	     
	    /*
	      *  Finish once a match is found.
	      */
	    if ( *match ) break;
	
	}  /* end of for loop */
    }
}
