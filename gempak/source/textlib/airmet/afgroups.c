#include "afcmn.h"


/************************************************************************
 * afgroups.c                                             		*
 *                                                                      *
 * This module contains the subroutines to group the GFA snapshots and  *
 * smears together.                              			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   library functions:                                                 *
 *	af_elms2grps		- group GFAs into "GFA_SmrOlk_Grp"s	*
 *									*
 *   private functions:							*
 *	af_sortSnapShot		- sort snapshots in GFA_SmrOlk_Grp elm	*
 ***********************************************************************/


static int af_sortSnapShot	( VG_DBStruct **ss1, VG_DBStruct **ss2 );


/*=====================================================================*/

void af_elms2grps ( int nin, VG_DBStruct *el_in, 
		int *ngrp, GFA_SmrOlk_Grp **gfaGrp, int *iret )
/************************************************************************
 * af_elms2grps                                                     	*
 *                                                                      *
 * This routine preprocesses the input array of VG elements into an 	*
 * array of "GFA_SmrOlk_Grp" elements.  GFA snaphsots, smear, and 	*
 * outlook with the same hazard type and tag are grouped into a new	*
 * "GFA_SmrOlk_Grp" element.						*
 *                                                                      *
 * void af_elms2grps ( nin, el_in, grp, gfaGrp, *iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *      *nin		int		number of input elements	*
 *      *el_in		VG_DBStruct     array of GFA elements		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *ngrp		int		number of output elements	*
 *      **gfaGrp	GFA_smrOlk_grp  array of GFA_SmrOlk_Grp	elements*
 *                                                                      *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC          	03/06   Created                         	*
 * J. Wu/SAIC          	03/06   Sort the grouped snapshots in the order	*
 *				ascending forecast hour			*
 * B. Yin/SAIC          07/06   change subtype (haz and category types) *
 * J. Wu/SAIC          	08/06   Combine system and user smears		*
 * E. Safford/SAIC	08/06	move from afcreate.c			*
 ***********************************************************************/
{
    int 		ii, jj, ier, subType1, subType2, one = 1;    
    char		hazType1[8], hazType2[8], tag1[4], tag2[4]; 
    char		tmpStr[ STD_STRLEN ];
    
    int			*grouped;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
    
    
    /*
     *  Set the flags - each GFA element should only be grouped once.
     */
    G_MALLOC ( grouped, int, nin, "af_elms2grp: grouped" );	    
    for ( ii = 0; ii < nin; ii++ ) {
         grouped[ ii ] = G_FALSE;
    }    

    
    /*
     *  Group the GFAs into GFA_SmrOlk_Grp elements.
     */
    *ngrp = 0;
    for ( ii = 0; ii < nin; ii++ ) {

        /*  
	 *  Skip non-GFA elements
	 */
	if (  grouped[ii] || el_in[ ii ].hdr.vg_type != GFA_ELM ) {
	    continue;
	}
	        
	
	/*  
	  *  Add a new GFA_SmrOlk_Grp element
	  */
        if ( *ngrp == 0 ) {
            G_MALLOC ( (*gfaGrp), GFA_SmrOlk_Grp, one, "af_elms2grp: gfaGrp" );	    
        }
	else {
	    G_REALLOC ( (*gfaGrp), GFA_SmrOlk_Grp, *ngrp + 1, "af_elms2grp: gfaGrp" );
	}
		
	cvg_getFld ( &el_in[ ii ], TAG_GFA_AREATYPE, hazType1, &ier );
	strcpy ( (*gfaGrp)[*ngrp].haz_type, hazType1 );
	if ( ( strcasecmp ( hazType1, "FZLVL" ) == 0 ) &&
	     ( el_in[ ii ].hdr.closed == 0 ) ) {	   
	    (*gfaGrp)[*ngrp].openFzlvl = True;
	}
 	else {
	    (*gfaGrp)[*ngrp].openFzlvl = False;	
	}
		
	cvg_getFld ( &el_in[ ii ], TAG_GFA_TAG, tag1, &ier );
        strcpy ( (*gfaGrp)[*ngrp].tag, tag1 );
	
	cvg_getFld ( &el_in[ ii ], TAG_GFA_SUBTYPE, tmpStr, &ier );
        subType1 = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;
	
	(*gfaGrp)[*ngrp].nsnapshot = 0;
	(*gfaGrp)[*ngrp].smear = (VG_DBStruct *)NULL;
	(*gfaGrp)[*ngrp].outlook = (VG_DBStruct *)NULL;
	(*gfaGrp)[*ngrp].snapshots = (VG_DBStruct **)NULL;
	
	if ( subType1 == GFA_SNAPSHOT ) {
	    G_MALLOC ( (*gfaGrp)[*ngrp].snapshots, VG_DBStruct *, one, 
        	                   "af_elms2grp: gfaGrp.snapshots" );	    
	    (*gfaGrp)[*ngrp].snapshots[0] = &el_in[ii];   	    
	    ( (*gfaGrp)[*ngrp].nsnapshot )++;

	}
	else if ( subType1 == GFA_USER_SMEAR || 
		 subType1 == GFA_SYSTEM_SMEAR ) {
            (*gfaGrp)[*ngrp].smear = &el_in[ii];
	}
	else {  /* Outlook */
            (*gfaGrp)[*ngrp].outlook = &el_in[ii];	
	}


	/*  
	  *  Find the rest of GFA_SmrOlk_Grp elements with same hazard type & tag
	  */
	for ( jj = (ii + 1); jj < nin; jj++ ) {
	    
	    if ( grouped[ jj ] ) {
	        continue;
	    }
	    
	    cvg_getFld ( &el_in[ jj ], TAG_GFA_SUBTYPE, tmpStr, &ier );
            subType2 = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;

	    cvg_getFld ( &el_in[ jj ], TAG_GFA_AREATYPE, hazType2, &ier );

	    cvg_getFld ( &el_in[ jj ], TAG_GFA_TAG, tag2, &ier );

	    if ( ( strcasecmp ( hazType1, hazType2 ) == 0 ) &&
	         ( strcasecmp ( tag1, tag2 ) == 0 ) )	{
		
		switch ( subType2 ) {
		    
		    case GFA_SNAPSHOT: 		    
                        if ( (*gfaGrp)[*ngrp].nsnapshot == 0 ) {
		            G_MALLOC ( (*gfaGrp)[*ngrp].snapshots, VG_DBStruct *, one, 
	                            "af_elms2grp: gfaGrp.snapshots" );	    
	                }
		        else {
		            G_REALLOC ( (*gfaGrp)[*ngrp].snapshots, VG_DBStruct *, 
			             (*gfaGrp)[*ngrp].nsnapshot + 1, 
	                             "af_elms2grp: gfaGrp.snapshots" );	    		    
		        }
		    
		        (*gfaGrp)[*ngrp].snapshots[ (*gfaGrp)[*ngrp].nsnapshot ] = &el_in[ jj ];
		        ( (*gfaGrp)[*ngrp].nsnapshot )++;
	                grouped[ jj ] = True;		    
		      break;
		    
		    case GFA_SYSTEM_SMEAR:		        
		    case GFA_USER_SMEAR:
		        if ( (*gfaGrp)[*ngrp].smear == (VG_DBStruct *)NULL ) {		    
		            (*gfaGrp)[*ngrp].smear = &el_in[jj];
	                     grouped[ jj ] = True;
		        }
		      break;
		    
		    case GFA_SYSTEM_OUTLOOK:
		    case GFA_USER_OUTLOOK:	    
		        if ( (*gfaGrp)[*ngrp].outlook == (VG_DBStruct *)NULL ) {
		    	    (*gfaGrp)[*ngrp].outlook = &el_in[jj];
	                     grouped[ jj ] = True;
		        }
		      break;
		    
		    default:
		      break;
		}		
            }        
	}
	
	(*ngrp)++;    /* increment the number of output elements */
    
    }
   
    G_FREE ( grouped, int );

    
    /*
     *  Sort the snapshots in ascending	forecast hour order
     */
    for ( ii = 0; ii < *ngrp; ii++ ) {
        	
	if ( (*gfaGrp)[ii].nsnapshot > 1 ) {
	    qsort( (*gfaGrp)[ii].snapshots, (size_t)(*gfaGrp)[ii].nsnapshot, 
	            sizeof ( VG_DBStruct * ),
	            (int(*)(const void*, const void*))af_sortSnapShot );
        }
    }    
}

/*=====================================================================*/


static int af_sortSnapShot ( VG_DBStruct **ss1, VG_DBStruct **ss2 )
/************************************************************************
 *  af_sortSnapShot                                                    	*
 *                                                                      *
 * Compares the snapshot's forecast hours for the snapshots array within*
 * a GFA_SmrOlk_Grp structure.						*
 *                                                                      *
 * static int af_sortSnapShot ( ss1, ss2 )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	**ss1		VG_DBStruct	Pointer to GFA element		*
 * 	**ss2		VG_DBStruct	Pointer to GFA element		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	af_sortSnapShot	int		Return code ()        		*
 *                                      -1 - forecast hour in ss1 <	*
 *					     forecast hour in ss2 	*
 *                                       1 - forecast hour in ss1 >=	*
 *					     forecast hour in ss2 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      	03/06	initial coding				*
 * E. Safford/SAIC	08/06	move from afcreate.c			*
 * J. Wu/SAIC      	12/07	Fix bug sorting snapshots with minute	*
 ***********************************************************************/
{    
    int		ier;
    int		hour1, hour2;
    char	*pstr, fcst_hr1[9], fcst_hr2[9];
/*---------------------------------------------------------------------*/
    
    cvg_getFld ( *ss1, TAG_GFA_FCSTHR, fcst_hr1, &ier );            
    cvg_getFld ( *ss2, TAG_GFA_FCSTHR, fcst_hr2, &ier );            
        
    pstr = strtok( fcst_hr1, ":" );
    hour1 = atoi ( pstr ) * 60;
    pstr = strtok( NULL, ":" );
    if ( pstr ) {
        hour1 += atoi( pstr );
    }

    pstr = strtok( fcst_hr2, ":" );
    hour2 = atoi ( pstr ) * 60;
    pstr = strtok( NULL, ":" );
    if ( pstr ) {
        hour2 += atoi( pstr );
    }
    
    return ( hour1 < hour2 ) ? -1 : 1;
}

/*=====================================================================*/
